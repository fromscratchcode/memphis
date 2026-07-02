use crate::{
    core::Container,
    parser::types::{Expr, Statement, StatementKind},
    treewalk::{
        pausable::{Frame, FrameKind},
        result::Raise,
        state::DebugSnapshot,
        types::{Exception, Function},
        Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal,
        TreewalkValue,
    },
};

struct PendingFunctionCall {
    function: Container<Function>,
    scope: Container<Scope>,
}

pub struct TreewalkDebugSession {
    stack: Vec<Frame>,
    interpreter: TreewalkInterpreter,
}

impl TreewalkDebugSession {
    pub fn new(frame: Frame, interpreter: TreewalkInterpreter) -> Self {
        Self {
            stack: vec![frame],
            interpreter,
        }
    }

    fn frame(&self) -> &Frame {
        self.stack.last().expect("Empty debugger call stack")
    }

    fn frame_mut(&mut self) -> &mut Frame {
        self.stack.last_mut().expect("Empty debugger call stack")
    }

    // Currently the debugger only steps into function calls that appear as standalone expression
    // statements:
    //
    //     foo()
    //
    // Calls nested inside expressions or assignments are evaluated normally and are not treated
    // as debugger step boundaries yet.
    fn prepare_function_call(&self, expr: &Expr) -> TreewalkResult<Option<PendingFunctionCall>> {
        if let Expr::FunctionCall { callee, args } = expr {
            let callable = self.interpreter.evaluate_callable(callee)?;
            let Ok(function) = self.interpreter.expect_function(callable) else {
                // we found a non-user-function. Since we cannot step into those, return as if we
                // did not find a function call
                return Ok(None);
            };
            let args = self.interpreter.evaluate_args(args)?;
            let scope = function
                .borrow()
                .create_scope(&args)
                .raise(&self.interpreter)?;
            Ok(Some(PendingFunctionCall { function, scope }))
        } else {
            Ok(None)
        }
    }

    fn execute_control_flow_statement(&mut self, statement: &Statement) -> TreewalkResult<bool> {
        match &statement.kind {
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                self.frame_mut().advance_pc();
                if let Some(selected_block) = self
                    .interpreter
                    .select_if_branch(if_part, elif_parts, else_part)?
                {
                    self.stack.push(Frame::new(selected_block));
                }

                Ok(true)
            }
            StatementKind::Expression(expr) => {
                if let Some(call) = self.prepare_function_call(expr)? {
                    let cross_module = self
                        .interpreter
                        .enter_function_context(call.function.clone(), call.scope);
                    self.frame_mut().advance_pc(); // move past the function call
                    self.stack.push(Frame::new_function(
                        call.function.borrow().body.clone(),
                        cross_module,
                    ));
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            _ => Ok(false),
        }
    }

    fn seek_next_step(&mut self) -> TreewalkResult<Option<Statement>> {
        loop {
            if self.stack.is_empty() {
                return Ok(None);
            }
            // not sure if this is necessary, can cleanup_finished_frames be enough?
            if self.frame().is_finished() {
                self.stack.pop();
                continue;
            }

            let stmt = self.frame().current_statement().clone();
            let encountered_control_flow = self.execute_control_flow_statement(&stmt)?;

            // If we did not encounter a control flow statement, the next statement is a leaf and
            // we should return to handle it.
            if !encountered_control_flow {
                return Ok(Some(stmt));
            }
        }
    }

    fn execute_statement(&mut self, stmt: &Statement) -> TreewalkResult<TreewalkValue> {
        let result = self.interpreter.evaluate_statement(stmt);

        if let Err(TreewalkDisruption::Signal(TreewalkSignal::Return(return_val))) = result {
            match self.frame().kind {
                FrameKind::Function { .. } => {
                    self.frame_mut().finish();
                    return Ok(return_val);
                }
                FrameKind::Block => {
                    // Should this move to the parser? If so, this could be an invariant rather
                    // than a user-facing error.
                    return Exception::runtime_error_with("Return outside function")
                        .raise(&self.interpreter);
                }
            }
        }

        result
    }

    pub fn step(&mut self) -> TreewalkResult<()> {
        let Some(stmt) = self.seek_next_step()? else {
            return Ok(());
        };

        self.execute_statement(&stmt)?;
        self.frame_mut().advance_pc();
        self.cleanup_finished_frames();

        Ok(())
    }

    fn cleanup_finished_frames(&mut self) {
        while let Some(frame) = self.stack.last() {
            if !frame.is_finished() {
                break;
            }

            let frame = self.stack.pop().unwrap();

            match frame.kind {
                // do nothing, no cleanup required
                FrameKind::Block => {}
                FrameKind::Function { cross_module } => {
                    self.interpreter.exit_function_context(cross_module);
                }
            }
        }
    }

    pub fn snapshot(&self) -> DebugSnapshot {
        self.interpreter
            .state
            .debug_snapshot()
            .expect("Failed to fetch debug snapshot")
    }

    pub fn is_finished(&self) -> bool {
        self.stack.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        domain::Text,
        parser::Parser,
        treewalk::{
            debugger::TreewalkDebugSession, test_utils::*, TreewalkContext, TreewalkDisruption,
            TreewalkResult,
        },
    };

    fn init(input: &str) -> TreewalkDebugSession {
        let ast = Parser::parse_text(&Text::new(input)).expect("Failed to parse input");
        TreewalkContext::stdin()
            .interpreter()
            .start_debug_session(ast)
    }

    #[test]
    fn test_debugger_step_flat_statements() -> TreewalkResult<()> {
        let input = r#"
x = 10
y = 20
"#;
        let mut debugger = init(input);

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(10)));
        assert_eq!(snapshot.get_global("y"), None);
        assert!(!debugger.is_finished());

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(10)));
        assert_eq!(snapshot.get_global("y"), Some(&int!(20)));
        assert!(debugger.is_finished());

        Ok(())
    }

    #[test]
    fn test_debugger_step_over_non_user_functions() -> TreewalkResult<()> {
        let input = r#"
x = 10
print(x)
y = 20
"#;
        let mut debugger = init(input);

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(10)));
        assert_eq!(snapshot.get_global("y"), None);
        assert!(!debugger.is_finished());

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(10)));
        assert_eq!(snapshot.get_global("y"), None);
        assert!(!debugger.is_finished());

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(10)));
        assert_eq!(snapshot.get_global("y"), Some(&int!(20)));
        assert!(debugger.is_finished());

        Ok(())
    }

    #[test]
    fn test_debugger_steps_through_if_block() -> TreewalkResult<()> {
        let input = r#"
x = 0
if True:
    x = 1
    y = 2
z = 3
"#;
        let mut debugger = init(input);

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(0)));
        assert_eq!(snapshot.get_global("y"), None);
        assert_eq!(snapshot.get_global("z"), None);
        assert!(!debugger.is_finished());

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(1)));
        assert_eq!(snapshot.get_global("y"), None);
        assert_eq!(snapshot.get_global("z"), None);
        assert!(!debugger.is_finished());

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(1)));
        assert_eq!(snapshot.get_global("y"), Some(&int!(2)));
        assert_eq!(snapshot.get_global("z"), None);
        assert!(!debugger.is_finished());

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("z"), Some(&int!(3)));
        assert!(debugger.is_finished());

        Ok(())
    }

    #[test]
    fn test_debugger_steps_through_if_block_with_return() -> TreewalkResult<()> {
        let input = r#"
x = 0
if True:
    return None
    x = 1
    y = 2
z = 3
"#;
        let mut debugger = init(input);

        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_global("x"), Some(&int!(0)));
        assert_eq!(snapshot.get_global("y"), None);
        assert_eq!(snapshot.get_global("z"), None);
        assert!(!debugger.is_finished());

        let e = match debugger.step() {
            Err(TreewalkDisruption::Error(e)) => e,
            _ => panic!(),
        };
        assert_runtime_error!(e.exception, "Return outside function");

        Ok(())
    }

    #[test]
    fn test_debugger_steps_into_top_level_call() -> TreewalkResult<()> {
        let input = r#"
def foo():
    x = 1
    y = 2
    return None

foo()
z = 3
"#;
        let mut debugger = init(input);

        // Step 1: define foo
        debugger.step()?;

        // Step 2: foo() → enters function and executes x = 1
        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_local("x"), Some(&int!(1)));
        assert_eq!(snapshot.get_local("y"), None);
        assert_eq!(snapshot.get_global("z"), None);
        assert!(!debugger.is_finished());

        // Step 3: inside foo → y = 2
        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_local("x"), Some(&int!(1)));
        assert_eq!(snapshot.get_local("y"), Some(&int!(2)));
        assert_eq!(snapshot.get_global("z"), None);
        assert!(!debugger.is_finished());

        // This is just to skip over the return None. TODO this shouldn't be necessary.
        // We put the return None in there just so we could inspect 'y' as a local
        debugger.step()?;

        // Step 4: return from foo, execute z = 3
        debugger.step()?;
        let snapshot = debugger.snapshot();
        assert_eq!(snapshot.get_local("x"), None);
        assert_eq!(snapshot.get_local("y"), None);
        assert_eq!(snapshot.get_global("z"), Some(&int!(3)));
        assert!(debugger.is_finished());

        Ok(())
    }
}
