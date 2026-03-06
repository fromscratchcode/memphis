use crate::{
    bytecode_vm::{
        indices::Index,
        runtime::{types::Class, HeapObject, Reference},
        VmValue,
    },
    domain::Type,
};

pub struct Heap {
    storage: Vec<HeapObject>,
}

impl Heap {
    pub fn new() -> Self {
        Self { storage: vec![] }
    }

    fn next_index(&self) -> Reference {
        let index = Index::new(self.storage.len());
        Reference::ObjectRef(index)
    }

    pub fn allocate(&mut self, value: HeapObject) -> Reference {
        let index = self.next_index();
        self.storage.push(value);
        index
    }

    /// Special allocator for <class 'type'>, whose class references itself.
    pub fn allocate_type(&mut self) -> Reference {
        let type_ref = self.next_index();
        let obj = HeapObject::new(
            type_ref,
            VmValue::Class(Class::new_builtin(Type::Type.to_string())),
        );
        self.storage.push(obj);
        type_ref
    }

    pub fn get(&self, reference: Reference) -> Option<&HeapObject> {
        match reference {
            Reference::ObjectRef(index) => self.storage.get(*index),
        }
    }

    pub fn get_mut(&mut self, reference: Reference) -> Option<&mut HeapObject> {
        match reference {
            Reference::ObjectRef(index) => self.storage.get_mut(*index),
        }
    }

    #[cfg(test)]
    pub fn iter(&self) -> impl Iterator<Item = &HeapObject> {
        self.storage.iter()
    }
}
