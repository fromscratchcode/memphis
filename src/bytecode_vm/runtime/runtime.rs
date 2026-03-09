use std::collections::HashMap;

use crate::{
    bytecode_vm::{
        runtime::{
            modules::{asyncio, builtins},
            types::Module,
            BuiltinFn, BuiltinFunction, BuiltinInstances, BuiltinTypes, Heap, HeapObject,
        },
        VmValue,
    },
    core::Container,
    domain::ModuleName,
};

pub struct Runtime {
    pub heap: Heap,

    module_store: HashMap<ModuleName, Container<Module>>,

    pub builtin_types: BuiltinTypes,

    pub builtin_instances: BuiltinInstances,
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
        };

        runtime.init_modules();

        runtime
    }

    fn init_modules(&mut self) {
        let builtin_mod = builtins::init_module(self);
        let async_mod = asyncio::init_module(self);

        let _ = self.create_module(&ModuleName::main());

        self.store_module(Container::new(builtin_mod));
        self.store_module(Container::new(async_mod));
    }

    pub fn read_module(&self, name: &ModuleName) -> Option<Container<Module>> {
        self.module_store.get(name).cloned()
    }

    pub fn store_module(&mut self, module: Container<Module>) {
        let name = module.borrow().name().to_owned();
        self.module_store.insert(name, module);
    }

    /// Create a new empty `Module` of a given name and store it in the `Runtime`.
    pub fn create_module(&mut self, name: &ModuleName) -> Container<Module> {
        let module = Container::new(Module::new(name.clone()));
        self.store_module(module.clone());
        module
    }
}

pub fn register_builtin_funcs(
    runtime: &mut Runtime,
    module: &mut Module,
    builtins: &[(&str, BuiltinFn)],
) {
    for (name, func) in builtins {
        let obj = HeapObject::new(
            runtime.builtin_types.builtin_function,
            VmValue::BuiltinFunction(BuiltinFunction::new(name, *func)),
        );
        let func_ref = runtime.heap.allocate(obj);
        module.write(name, func_ref);
    }
}
