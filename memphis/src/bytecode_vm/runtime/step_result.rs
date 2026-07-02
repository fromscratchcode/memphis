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
    Continue,
    Exit(FrameExit),
}

#[derive(Debug)]
pub enum FrameExit {
    Completed(Completion),
    Suspended(Suspension),
}

#[derive(Debug)]
pub enum Completion {
    Return(Reference),
    Exception(RaisedException),
}

#[derive(Debug)]
pub enum Suspension {
    Yield(Reference),
    Await(Container<Coroutine>),
    Sleep(Duration),
}
