use crate::{
    core::Container,
    parser::types::Statement,
    treewalk::{DomainResult, Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue},
};

use super::PausableStack;

/// This instructs [`Pausable::run_until_pause`] what action should happen next.
pub enum PausableStepResult {
    NoOp,
    BreakAndReturn(TreewalkValue),
    Return(TreewalkValue),
    Break,
}

/// The interface for generators and coroutines, which share the ability to be paused and resumed.
pub trait Pausable {
    /// A getter for the [`PausableContext`] of a pausable function.
    fn context(&self) -> &PausableStack;

    fn context_mut(&mut self) -> &mut PausableStack;

    /// A getter for the [`Scope`] of a pausable function.
    fn scope(&self) -> Container<Scope>;

    /// A handle to perform any necessary cleanup once this function returns, including set its
    /// return value.
    fn finish(&mut self, result: TreewalkValue) -> DomainResult<TreewalkValue>;

    /// A handle to invoke the discrete operation of evaluating an individual statement and
    /// producing a [`PausableStepResult`] based on the control flow instructions and or the
    /// expression return values encountered.
    fn handle_step(
        &mut self,
        interpreter: &TreewalkInterpreter,
        statement: Statement,
    ) -> TreewalkResult<PausableStepResult>;
}
