use std::collections::HashMap;

use crate::{
    bytecode_vm::{
        runtime::{
            modules::{asyncio, builtins},
            types::Module,
            BuiltinFn, BuiltinFunction, BuiltinInstances, BuiltinTypes, Heap, HeapObject,
            Reference,
        },
        VmValue,
    },
    core::Container,
    domain::ModuleName,
};

pub struct Runtime {
    pub heap: Heap,

    module_store: HashMap<ModuleName, Reference>,

    pub builtin_types: BuiltinTypes,

    pub builtin_instances: BuiltinInstances,

    pub string_table: HashMap<String, Reference>,
}

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new();

        let builtin_types = BuiltinTypes::init(&mut heap);
        let builtin_instances = BuiltinInstances::init(&mut heap, &builtin_types);

        let mut runtime = Self {
            heap,
            builtin_types,
            builtin_instances,
            module_store: HashMap::new(),
            string_table: HashMap::new(),
        };

        runtime.init_modules();

        runtime
    }

    fn init_modules(&mut self) {
        self.alloc_module(Module::new(ModuleName::main()));

        let builtin_mod = builtins::init_module(self);
        let asyncio_mod = asyncio::init_module(self);

        self.alloc_module(builtin_mod);
        self.alloc_module(asyncio_mod);
    }

    pub fn read_module(&self, name: &ModuleName) -> Option<Reference> {
        self.module_store.get(name).cloned()
    }

    pub fn store_module(&mut self, name: &ModuleName, module_ref: Reference) {
        self.module_store.insert(name.clone(), module_ref);
    }

    pub fn to_heapified_bool(&self, value: bool) -> Reference {
        match value {
            true => self.builtin_instances.true_obj,
            false => self.builtin_instances.false_obj,
        }
    }

    pub fn heapify(&mut self, value: HeapObject) -> Reference {
        match value.payload {
            // This case is only needed when we receive a generic `VmValue`, i.e. loading a
            // constant. For cases where we know we have a boolean, it is preferred to use
            // `to_heapified_bool` directly.
            VmValue::Bool(bool_val) => self.to_heapified_bool(bool_val),
            _ => self.heap.allocate(value),
        }
    }

    pub fn new_object(&mut self, class: Reference, payload: VmValue) -> Reference {
        let obj = HeapObject::new(class, payload);
        self.heapify(obj)
    }

    pub fn alloc_module(&mut self, module: Module) -> Reference {
        let name = module.name().clone();
        let type_ = self.builtin_types.module;

        let module_ref = self.new_object(type_, VmValue::Module(Container::new(module)));
        self.store_module(&name, module_ref);

        module_ref
    }
}

pub fn register_builtin_funcs(
    runtime: &mut Runtime,
    module: &mut Module,
    builtins: &[(&str, BuiltinFn)],
) {
    for (name, func) in builtins {
        let type_ = runtime.builtin_types.builtin_function;
        let func_ref = runtime.new_object(
            type_,
            VmValue::BuiltinFunction(BuiltinFunction::new(name, *func)),
        );
        module.write(name, func_ref);
    }
}
