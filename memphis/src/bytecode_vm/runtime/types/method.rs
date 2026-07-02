use crate::bytecode_vm::runtime::{types::FunctionObject, Reference};

#[derive(Clone, Debug, PartialEq)]
pub struct Method {
    pub receiver: Reference,
    pub function: FunctionObject,
}

impl Method {
    pub fn new(receiver: Reference, function: FunctionObject) -> Self {
        Self { receiver, function }
    }

    pub fn name(&self) -> String {
        format!("{} of {:?}", self.function.name(), &self.receiver)
    }
}
