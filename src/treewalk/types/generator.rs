use crate::{
    core::Container,
    parser::types::{Ast, ConditionalAst, Expr, ForClause, Statement, StatementKind},
    treewalk::{
        pausable::{Frame, Pausable, PausableStack, PausableStepResult},
        protocols::Iterable,
        result::Raise,
        type_system::CloneableIterable,
        types::{Exception, Function},
        DomainResult, Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult,
        TreewalkSignal, TreewalkState, TreewalkValue,
    },
};

pub struct Generator {
    scope: Container<Scope>,
    context: PausableStack,
    delegated: Option<Box<dyn CloneableIterable>>,
}

impl Generator {
    pub fn new(scope: Container<Scope>, function: Container<Function>) -> Self {
        let frame = Frame::new(function.borrow().clone().body);

        Self {
            scope,
            context: PausableStack::new(frame),
            delegated: None,
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
        if let Some(delegated) = &mut self.delegated {
            match delegated.try_next() {
                Ok(Some(val)) => return Ok(val),
                Ok(None) => {
                    self.delegated = None;
                }
                Err(TreewalkDisruption::Error(e))
                    if e.exception.get_type() == crate::domain::Type::StopIteration =>
                {
                    self.delegated = None;
                }
                Err(e) => return Err(e),
            }
        }

        <Self as Pausable>::run_until_pause(self, interpreter)
    }

    /// By this point, all control flow statements have already been handled manually. Evaluate all
    /// other statements unless we encounter a yield.
    ///
    /// Only yield statements will cause a value to be returned, everything else will return
    /// `None`.
    fn execute_statement(
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        match interpreter.evaluate_statement(&stmt) {
            Ok(_) => Ok(None),
            Err(TreewalkDisruption::Signal(TreewalkSignal::Return(val))) => {
                Exception::stop_iteration_with(val).raise(interpreter)
            }
            Err(TreewalkDisruption::Signal(TreewalkSignal::Yield(val))) => Ok(Some(val)),
            Err(TreewalkDisruption::Signal(TreewalkSignal::YieldFrom(val))) => {
                if self.delegated.is_none() {
                    let iter = val.as_iterator().raise(interpreter)?;
                    self.delegated = Some(iter);
                }

                // This is a bit leaky because we not only initializing the delegation here, we're
                // also kicking it off. Ideally, we'd do this step later.
                match self.delegated.as_mut().unwrap().try_next()? {
                    Some(val) => Ok(Some(val)),
                    // We can only hit this if the iterable we are calling yield from on is
                    // empty.
                    // This matches Python's behavior for: `yield from []`
                    None => Exception::stop_iteration().raise(interpreter),
                }
            }
            Err(e) => Err(e),
        }
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

    fn handle_step(
        &mut self,
        interpreter: &TreewalkInterpreter,
        stmt: Statement,
    ) -> TreewalkResult<PausableStepResult> {
        match self.execute_statement(interpreter, stmt)? {
            Some(yielded) => {
                self.on_exit(interpreter);
                Ok(PausableStepResult::BreakAndReturn(yielded))
            }
            None => Ok(PausableStepResult::NoOp),
        }
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
        self.generator
            .borrow_mut()
            .run_until_pause(&self.interpreter)
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
