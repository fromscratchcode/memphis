use std::{
    fmt::{Debug, Error, Formatter},
    time::Instant,
};

use crate::{
    bytecode_vm::runtime::{Frame, Reference},
    core::Container,
};

#[derive(Clone, Debug)]
pub enum CoroutineState {
    Ready,
    #[allow(dead_code)]
    // We don't read the payload here right now, not sure what this should be yet.
    WaitingOn(Container<Coroutine>),
    SleepingUntil(Instant),
    Finished(Reference),
}

#[derive(Clone)]
pub struct Coroutine {
    pub frame: Frame,
    pub state: CoroutineState,
    pub waiters: Vec<Container<Coroutine>>,
}

impl Debug for Coroutine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{} (state: {:?})", self.frame.name(), self.state)
    }
}

impl Coroutine {
    pub fn new(frame: Frame) -> Self {
        Self {
            frame,
            state: CoroutineState::Ready,
            waiters: vec![],
        }
    }
}
