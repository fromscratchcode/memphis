use std::collections::HashMap;

use crate::{
    bytecode_vm::{
        runtime::{
            modules::{asyncio, builtins},
            types::{Class, Module},
            BuiltinFn, BuiltinFunction, Heap, Reference,
        },
        VmValue,
    },
    core::Container,
    domain::{ModuleName, Type},
};

struct BuiltinTypes {
    int: Reference,
    bool: Reference,
    list: Reference,
}

impl BuiltinTypes {
    fn from_map(mut type_map: HashMap<Type, Reference>) -> BuiltinTypes {
        BuiltinTypes {
            int: type_map.remove(&Type::Int).unwrap(),
            bool: type_map.remove(&Type::Bool).unwrap(),
            list: type_map.remove(&Type::List).unwrap(),
        }
    }
}

fn init_type_classes(heap: &mut Heap) -> HashMap<Type, Reference> {
    let mut type_map = HashMap::new();

    let class_ref = heap.allocate(VmValue::Class(Class::new_builtin(Type::Type.to_string())));
    type_map.insert(Type::Type, class_ref);

    let class_ref = heap.allocate(VmValue::Class(Class::new_builtin(Type::Object.to_string())));
    type_map.insert(Type::Object, class_ref);

    for type_ in Type::all()
        .iter()
        // these are handled separately
        .filter(|t| !matches!(t, Type::Type | Type::Object))
    {
        let class_ref = heap.allocate(VmValue::Class(Class::new_builtin(type_.to_string())));
        type_map.insert(*type_, class_ref);
    }

    type_map
}

pub struct Runtime {
    pub heap: Heap,

    module_store: HashMap<ModuleName, Container<Module>>,

    builtin_types: BuiltinTypes,
}

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new();

        // The type_map is a bootstrap-only registry to help us construct BuiltinType and
        // initialize the global scope in the builtins module.
        let type_map = init_type_classes(&mut heap);
        let builtin_mod = builtins::init_module(&mut heap, &type_map);
        let builtin_types = BuiltinTypes::from_map(type_map);

        let async_mod = asyncio::init_module(&mut heap);

        let mut runtime = Self {
            heap,
            builtin_types,
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
