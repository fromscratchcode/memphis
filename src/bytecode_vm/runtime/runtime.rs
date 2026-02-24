use std::collections::HashMap;

use crate::{
    bytecode_vm::{
        runtime::{
            modules::{asyncio, builtins},
            types::Module,
            BuiltinFn, BuiltinFunction, Heap,
        },
        VmValue,
    },
    core::Container,
    domain::ModuleName,
};

pub struct Runtime {
    pub heap: Heap,

    module_store: HashMap<ModuleName, Container<Module>>,
}

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new();

        let builtin_mod = builtins::init_module(&mut heap);
        let async_mod = asyncio::init_module(&mut heap);

        let mut runtime = Self {
            heap,
            module_store: HashMap::new(),
        };

        let _ = runtime.create_module(&ModuleName::main());
        runtime.store_module(Container::new(builtin_mod));
        runtime.store_module(Container::new(async_mod));

        runtime
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
    heap: &mut Heap,
    module: &mut Module,
    builtins: &[(&str, BuiltinFn)],
) {
    for (name, func) in builtins {
        let func_ref = heap.allocate(VmValue::BuiltinFunction(BuiltinFunction::new(name, *func)));
        module.write(name, func_ref);
    }
}
