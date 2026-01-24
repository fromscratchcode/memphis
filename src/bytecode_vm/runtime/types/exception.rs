use crate::{
    bytecode_vm::{runtime::Reference, VirtualMachine},
    domain::{ExceptionKind, MemphisException},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Exception {
    kind: ExceptionKind,
    payload: Vec<Reference>,
}

impl Exception {
    pub fn normalize(&self, vm: &VirtualMachine) -> MemphisException {
        let args = self
            .payload
            .iter()
            .map(|r| vm.normalize_vm_ref(*r))
            .collect();
        MemphisException::new(self.kind.clone(), args)
    }

    fn new(kind: ExceptionKind, payload: Vec<Reference>) -> Self {
        Self { kind, payload }
    }

    fn new_empty(kind: ExceptionKind) -> Self {
        Self::new(kind, vec![])
    }

    pub fn runtime_error() -> Self {
        Self::new_empty(ExceptionKind::RuntimeError)
    }

    pub fn runtime_error_with(msg: Reference) -> Self {
        Self::new(ExceptionKind::RuntimeError, vec![msg])
    }

    pub fn type_error(msg: Reference) -> Self {
        Self::new(ExceptionKind::TypeError, vec![msg])
    }

    pub fn import_error(msg: Reference) -> Self {
        Self::new(ExceptionKind::ImportError, vec![msg])
    }

    pub fn syntax_error(msg: Reference) -> Self {
        Self::new(ExceptionKind::SyntaxError, vec![msg])
    }

    pub fn stop_iteration() -> Self {
        Self::new_empty(ExceptionKind::StopIteration)
    }

    pub fn value_error(msg: Reference) -> Self {
        Self::new(ExceptionKind::ValueError, vec![msg])
    }

    pub fn name_error(name: Reference) -> Self {
        Self::new(ExceptionKind::NameError, vec![name])
    }

    pub fn attribute_error(_object_type: impl Into<String>, _attr: impl Into<String>) -> Self {
        todo!();
    }
}
