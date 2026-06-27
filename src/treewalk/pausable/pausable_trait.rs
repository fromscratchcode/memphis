use crate::{
    core::Container,
    parser::types::Statement,
    treewalk::{Scope, TreewalkInterpreter, TreewalkResult},
};

use super::{PausableStack, StepResult};

/// The interface for generators and coroutines, which share the ability to be paused and resumed.
pub trait Pausable {
    /// A getter for the [`PausableContext`] of a pausable function.
    fn context(&self) -> &PausableStack;

    fn context_mut(&mut self) -> &mut PausableStack;

    /// A getter for the [`Scope`] of a pausable function.
    fn scope(&self) -> Container<Scope>;

    /// A handle to invoke the discrete operation of evaluating an individual statement and
    /// producing a [`PausableStepResult`] based on the control flow instructions and or the
    /// expression return values encountered.
    fn execute_statement(
        &mut self,
        interpreter: &TreewalkInterpreter,
        statement: &Statement,
    ) -> TreewalkResult<StepResult>;
}
