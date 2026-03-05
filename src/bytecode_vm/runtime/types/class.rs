use std::collections::HashMap;

use crate::bytecode_vm::runtime::{reference::Namespace, Reference};

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    name: String,
    namespace: Namespace,
}

impl Class {
    pub fn new(name: String, namespace: Namespace) -> Self {
        Self { name, namespace }
    }

    pub fn new_builtin(name: String) -> Self {
        Self {
            name,
            namespace: HashMap::new(),
        }
    }

    pub fn read<S>(&self, name: S) -> Option<Reference>
    where
        S: AsRef<str>,
    {
        self.namespace.get(name.as_ref()).cloned()
    }

    pub fn write<S>(&mut self, name: S, value: Reference)
    where
        S: AsRef<str>,
    {
        self.namespace.insert(name.as_ref().to_string(), value);
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
