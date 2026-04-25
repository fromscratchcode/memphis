use crate::{
    core::Container,
    domain::Type,
    parser::types::{Ast, ConditionalAst, Expr, ForClause, Statement, StatementKind},
    treewalk::{
        pausable::{Frame, Pausable, PausableRunner, PausableStack, PausableStepResult},
        protocols::Iterable,
        result::Raise,
        type_system::CloneableIterable,
        types::{Exception, Function},
        DomainResult, Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult,
        TreewalkSignal, TreewalkState, TreewalkValue,
    },
};

enum GeneratorSuspend {
    None,
    Delegating(Box<dyn CloneableIterable>),
}

pub struct Generator {
    scope: Container<Scope>,
    context: PausableStack,
    suspend: GeneratorSuspend,
}

impl Generator {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            context: PausableStack::new(frame),
            suspend: GeneratorSuspend::None,
        }
    }

    fn resume_delegation(
        &mut self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        let GeneratorSuspend::Delegating(delegated) = &mut self.suspend else {
            return Ok(None);
        };

        match delegated.try_next() {
            Ok(Some(val)) => Ok(Some(val)),
            Ok(None) => {
                self.suspend = GeneratorSuspend::None;
                interpreter
                    .state
                    .set_current_yield_from_result(TreewalkValue::None);
                Ok(None)
            }
            Err(TreewalkDisruption::Error(e)) if e.exception.get_type() == Type::StopIteration => {
                self.suspend = GeneratorSuspend::None;
                interpreter
                    .state
                    .set_current_yield_from_result(e.exception.first_arg_or_none());
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    pub fn new_from_comprehension(
        state: Container<TreewalkState>,
        body: &Expr,
        clauses: &[ForClause],
    ) -> Self {
        let generator_body = Self::build_nested_loops(body, clauses);
        let function = Container::new(Function::new_anonymous_generator(state, generator_body));
        Self::new(Container::new(Scope::default()), function)
    }

    pub fn run_until_pause(
        &mut self,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<TreewalkValue> {
        // `yield from` delegation is generator-specific behavior layered on top of the shared
        // pausable state machine.
        if let Some(val) = self.resume_delegation(interpreter)? {
            return Ok(val);
        }

        PausableRunner::run_until_pause(self, interpreter)
    }

    // This is a utility which takes the parsed elements found in a generator comprehension and
    // recursively builds a generator function out of them. This will then become the body of the
    // function provided to a generator.
    fn build_nested_loops(body: &Expr, clauses: &[ForClause]) -> Ast {
        if let Some((clause, remaining)) = clauses.split_first() {
            // Recursive case: Build nested loop for the remaining clauses
            let loop_body = Self::build_nested_loops(body, remaining);

            let ForClause {
                index,
                iterable,
                condition,
            } = clause;

            let loop_body = if let Some(condition) = &condition {
                Ast::new(vec![Statement::new(
                    1,
                    StatementKind::IfElse {
                        if_part: ConditionalAst::new(condition.clone(), loop_body),
                        elif_parts: Vec::new(),
                        else_part: None,
                    },
                )])
            } else {
                loop_body
            };

            let for_in_loop = Statement::new(
                1,
                StatementKind::ForInLoop {
                    index: index.clone(),
                    iterable: iterable.clone(),
                    body: loop_body,
                    else_block: None,
                },
            );

            Ast::new(vec![for_in_loop])
        } else {
            // Base case: Yield the body
            Ast::from_expr(Expr::Yield(Some(Box::new(body.clone()))))
        }
    }
}

impl Pausable for Generator {
    fn context(&self) -> &PausableStack {
        &self.context
    }

    fn context_mut(&mut self) -> &mut PausableStack {
        &mut self.context
    }

    fn scope(&self) -> Container<Scope> {
        self.scope.clone()
    }

    fn finish(&mut self, _result: TreewalkValue) -> DomainResult<TreewalkValue> {
        Err(Exception::stop_iteration())
    }

    /// Only yield statements will cause a value to be returned.
    fn execute_statement(
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: &Statement,
    ) -> TreewalkResult<PausableStepResult> {
        let step_result = match interpreter.evaluate_statement(stmt) {
            Ok(_) => Ok(PausableStepResult::NoOp),
            Err(TreewalkDisruption::Signal(TreewalkSignal::Return(val))) => {
                Exception::stop_iteration_with(val).raise(interpreter)
            }
            Err(TreewalkDisruption::Signal(TreewalkSignal::Yield(val))) => {
                Ok(PausableStepResult::YieldValue(val))
            }
            Err(TreewalkDisruption::Signal(TreewalkSignal::YieldFrom(val))) => {
                if matches!(self.suspend, GeneratorSuspend::None) {
                    self.suspend =
                        GeneratorSuspend::Delegating(val.as_iterator().raise(interpreter)?);
                }

                match self.resume_delegation(interpreter)? {
                    Some(val) => {
                        // yield and do _not_ advance PC
                        return Ok(PausableStepResult::YieldValue(val));
                    }
                    None => Ok(PausableStepResult::NoOp),
                }
            }
            Err(e) => Err(e),
        };

        self.context_mut().frame_mut().advance_pc();
        step_result
    }
}

#[derive(Clone)]
pub struct GeneratorIter {
    generator: Container<Generator>,
    interpreter: TreewalkInterpreter,
}

impl GeneratorIter {
    pub fn new(generator: Generator, interpreter: TreewalkInterpreter) -> Self {
        Self {
            generator: Container::new(generator),
            interpreter,
        }
    }

    pub fn run_until_pause(&mut self) -> TreewalkResult<TreewalkValue> {
        self.interpreter.state.push_yield_from_result_frame();
        let result = self
            .generator
            .borrow_mut()
            .run_until_pause(&self.interpreter);
        self.interpreter.state.pop_yield_from_result_frame();
        result
    }
}

impl Iterable for GeneratorIter {
    // We cannot use the boilerplate impl_iterable! here because we want to surface any
    // StopIteration errors, not swallow them the way Iterator::next does.
    fn try_next(&mut self) -> TreewalkResult<Option<TreewalkValue>> {
        match self.run_until_pause() {
            // is this right?
            Ok(r) => Ok(Some(r)),
            Err(e) => Err(e),
        }
    }
}
