use crate::bytecode_vm::{
    indices::Index,
    runtime::{HeapObject, Reference},
    VmValue,
};

pub struct Heap {
    storage: Vec<HeapObject>,
}

impl Heap {
    pub fn new() -> Self {
        Self { storage: vec![] }
    }

    pub fn allocate(&mut self, value: HeapObject) -> Reference {
        let index = Index::new(self.storage.len());
        self.storage.push(value);
        Reference::ObjectRef(index)
    }

    // This should only be used in bootstrapping code.
    pub fn allocate_raw(&mut self, value: VmValue) -> Reference {
        let index = Index::new(self.storage.len());
        let obj = HeapObject::new(Reference::Null, value);
        self.storage.push(obj);
        Reference::ObjectRef(index)
    }

    pub fn get(&self, reference: Reference) -> Option<&HeapObject> {
        match reference {
            Reference::ObjectRef(index) => self.storage.get(*index),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, reference: Reference) -> Option<&mut HeapObject> {
        match reference {
            Reference::ObjectRef(index) => self.storage.get_mut(*index),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn iter(&self) -> impl Iterator<Item = &HeapObject> {
        self.storage.iter()
    }
}
