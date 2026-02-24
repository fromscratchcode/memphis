use std::collections::HashMap;

use crate::bytecode_vm::{indices::ObjectTableIndex, runtime::Heap};

pub type Namespace = HashMap<String, Reference>;

/// Primitive values live directly on the stack.
/// [`Reference::ObjectRef`] items reference an object in the object table.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reference {
    Int(i64),
    Float(f64),
    ObjectRef(ObjectTableIndex),
}

impl Reference {
    pub fn display_annotated(&self, heap: &Heap) -> String {
        match self {
            Self::ObjectRef(index) => format!(
                "ObjectRef({}) => {:?}",
                index,
                heap.get(*self).expect("Heap lookup failed")
            ),
            _ => format!("{:?}", self),
        }
    }
}
