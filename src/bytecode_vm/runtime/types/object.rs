use std::collections::HashMap;

use crate::bytecode_vm::{
    runtime::{reference::Namespace, types::Exception, Reference},
    DomainResult, VirtualMachine,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub class: Reference,
    namespace: Namespace,
}

impl Object {
    pub fn new(class: Reference) -> Self {
        Self {
            class,
            namespace: HashMap::new(),
        }
    }

    /// `deref` is needed to get the actual class in case the symbol is not found on the object.
    pub fn read(&self, name: &str, vm: &VirtualMachine) -> DomainResult<Option<Reference>> {
        if let Some(result) = self.namespace.get(name) {
            return Ok(Some(*result));
        }

        let class = vm.deref(self.class);
        let class = class.as_class().ok_or_else(Exception::runtime_error)?;
        Ok(class.read(name))
    }

    pub fn write(&mut self, name: &str, value: Reference) {
        self.namespace.insert(name.to_string(), value);
    }
}
