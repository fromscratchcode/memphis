use std::collections::HashMap;

use crate::bytecode_vm::{indices::ObjectTableIndex, runtime::Heap, VmValue};

pub type Namespace = HashMap<String, Reference>;

#[derive(Debug, Clone)]
pub struct HeapObject {
    pub class: Reference,
    pub payload: VmValue,
}

impl HeapObject {
    pub fn new(class: Reference, payload: VmValue) -> Self {
        Self { class, payload }
    }
}

/// Primitive values live directly on the stack.
/// [`Reference::ObjectRef`] items reference an object in the object table.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Reference(ObjectTableIndex);

impl Reference {
    pub fn new(index: ObjectTableIndex) -> Self {
        Reference(index)
    }

    pub fn display_annotated(&self, heap: &Heap) -> String {
        format!(
            "ObjectRef({}) => {:?}",
            self.index(),
            heap.get(*self).expect("Heap lookup failed")
        )
    }

    pub fn index(&self) -> ObjectTableIndex {
        self.0
    }
}
