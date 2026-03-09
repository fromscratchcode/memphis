use crate::bytecode_vm::{
    runtime::{BuiltinTypes, Heap, HeapObject, Reference},
    VmValue,
};

pub struct BuiltinInstances {
    pub none: Reference,
    pub true_obj: Reference,
    pub false_obj: Reference,
}

impl BuiltinInstances {
    pub fn init(heap: &mut Heap, builtin_types: &BuiltinTypes) -> BuiltinInstances {
        let none = heap.allocate(HeapObject::new(builtin_types.none, VmValue::None));
        let true_obj = heap.allocate(HeapObject::new(builtin_types.bool, VmValue::Bool(true)));
        let false_obj = heap.allocate(HeapObject::new(builtin_types.bool, VmValue::Bool(false)));

        BuiltinInstances {
            none,
            true_obj,
            false_obj,
        }
    }
}
