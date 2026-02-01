use crate::bytecode_vm::{runtime::types::Exception, VirtualMachine};

#[derive(Clone, PartialEq, Debug)]
pub enum CompilerError {
    Unsupported(String),
    SyntaxError(String),
    ImportError(String),
}

impl CompilerError {
    pub fn import_error(msg: impl Into<String>) -> Self {
        Self::ImportError(msg.into())
    }

    pub fn syntax_error(msg: impl Into<String>) -> Self {
        Self::SyntaxError(msg.into())
    }

    pub fn into_exception(self, vm: &mut VirtualMachine) -> Exception {
        match self {
            CompilerError::SyntaxError(msg) => Exception::syntax_error(vm.intern_string(&msg)),
            CompilerError::ImportError(msg) => Exception::import_error(vm.intern_string(&msg)),
            CompilerError::Unsupported(msg) => Exception::syntax_error(vm.intern_string(&msg)),
        }
    }
}
