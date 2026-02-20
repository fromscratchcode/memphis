use std::collections::HashMap;

use crate::bytecode_vm::{
    runtime::{types::Exception, Reference},
    utils::HashKey,
    VirtualMachine, VmResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Dict {
    // Python 3.7+ preserves dict insertion order, we could use IndexMap if we want that.
    pub items: HashMap<HashKey, (Reference, Reference)>,
}

impl Dict {
    pub fn new(items: Vec<(HashKey, (Reference, Reference))>) -> Self {
        let mut dict = HashMap::new();
        for (hash_key, key_val_tuple) in items {
            dict.insert(hash_key, key_val_tuple);
        }
        Self { items: dict }
    }

    pub fn getitem(&self, vm: &mut VirtualMachine, index: Reference) -> VmResult<Reference> {
        let key = vm.deref(index).as_hash_key().expect("Unhashable key");
        if let Some((_, val)) = self.items.get(&key) {
            Ok(*val)
        } else {
            let exp = Exception::key_error(index);
            Err(vm.raise(exp))
        }
    }
}
