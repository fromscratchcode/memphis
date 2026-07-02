use crate::{
    core::Container,
    treewalk::{
        types::{Coroutine, Exception},
        RaisedException, TreewalkInterpreter, TreewalkValue,
    },
};

#[derive(Debug, Clone)]
pub enum TreewalkDisruption {
    Signal(TreewalkSignal), // Control flow (not errors)
    Error(RaisedException), // Actual Python runtime errors
}

#[cfg(test)]
impl TreewalkDisruption {
    pub fn as_err(&self) -> &RaisedException {
        match self {
            TreewalkDisruption::Signal(_) => panic!("Expected error!"),
            TreewalkDisruption::Error(ref e) => e,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TreewalkSignal {
    Return(TreewalkValue),
    Yield(TreewalkValue),
    YieldFrom(TreewalkValue),
    Raise,
    Await(Container<Coroutine>),
    Sleep(f64),
    Break,
    Continue,
}

// control-flow & raised runtime errors, used in upper levels of the code
pub type TreewalkResult<T> = Result<T, TreewalkDisruption>;

// local semantic errors, used in lower levels of the code
pub type DomainResult<T> = Result<T, Exception>;

pub trait Raise<T> {
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T>;
}

impl<T> Raise<T> for DomainResult<T> {
    /// Convert an `ExecutionError` into a raised runtime error
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T> {
        self.map_err(|kind| interpreter.raise_and_disrupt(kind))
    }
}

impl<T> Raise<T> for Exception {
    /// Raise this `ExecutionError` in the given interpreter, returning it as a
    /// `TreewalkResult<T>`.
    fn raise(self, interpreter: &TreewalkInterpreter) -> TreewalkResult<T> {
        Err(interpreter.raise_and_disrupt(self))
    }
}
