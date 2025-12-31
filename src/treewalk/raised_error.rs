use crate::{
    domain::{DebugCallStack, RaisedMemphisError},
    treewalk::types::Exception,
};

#[derive(Debug, PartialEq, Clone)]
pub struct RaisedException {
    pub debug_call_stack: DebugCallStack,
    pub exception: Exception,
}

impl RaisedException {
    pub fn new(debug_call_stack: DebugCallStack, exception: Exception) -> Self {
        Self {
            debug_call_stack,
            exception,
        }
    }
}

impl From<RaisedException> for RaisedMemphisError {
    fn from(e: RaisedException) -> Self {
        RaisedMemphisError::new(e.debug_call_stack.clone(), e.exception.into())
    }
}
