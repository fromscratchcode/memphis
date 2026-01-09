use std::time::Duration;

use crate::{
    bytecode_vm::runtime::{types::Coroutine, Reference},
    core::Container,
};

#[derive(Debug)]
pub enum StepResult {
    Return(Reference),
    Yield(Reference),
    Await(Container<Coroutine>),
    Sleep(Duration),
    Continue,
    Halt,
}
