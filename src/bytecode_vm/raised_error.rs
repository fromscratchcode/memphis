use crate::{
    bytecode_vm::{runtime::types::Exception, VirtualMachine},
    domain::{DebugCallStack, RaisedMemphisError},
};

#[derive(Debug, PartialEq, Clone)]
pub struct RaisedException {
    pub debug_call_stack: DebugCallStack,
    pub exception: Exception,
}

impl RaisedException {
    pub fn new(debug_call_stack: DebugCallStack, execution_error: Exception) -> Self {
        Self {
            debug_call_stack,
            exception: execution_error,
        }
    }

    pub fn normalize(&self, vm: &VirtualMachine) -> RaisedMemphisError {
        let exception = self.exception.normalize(vm);
        RaisedMemphisError::new(self.debug_call_stack.clone(), exception)
    }
}
