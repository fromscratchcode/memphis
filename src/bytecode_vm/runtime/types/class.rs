use std::collections::HashMap;

use crate::{
    bytecode_vm::runtime::{reference::Namespace, Reference},
    domain::Dunder,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    name: String,
    namespace: Namespace,
}

impl Class {
    pub fn new(name: String, namespace: Namespace) -> Self {
        Self { name, namespace }
    }

    pub fn new_builtin(name: String, name_ref: Reference) -> Self {
        let mut namespace = HashMap::new();
        namespace.insert(Dunder::Name.to_string(), name_ref);
        Self { name, namespace }
    }

    pub fn read<S>(&self, name: S) -> Option<Reference>
    where
        S: AsRef<str>,
    {
        self.namespace.get(name.as_ref()).cloned()
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
