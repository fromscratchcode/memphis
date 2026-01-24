use std::time::Duration;

use crate::{
    bytecode_vm::{
        runtime::{types::Coroutine, Reference},
        RaisedException,
    },
    core::Container,
};

#[derive(Debug)]
pub enum StepResult {
    Exception(RaisedException),
    Return(Reference),
    Yield(Reference),
    Await(Container<Coroutine>),
    Sleep(Duration),
    Continue,
}
