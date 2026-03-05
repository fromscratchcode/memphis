use std::collections::HashMap;

use crate::{
    bytecode_vm::{
        runtime::{
            modules::{asyncio, builtins},
            types::{Class, Module},
            BuiltinFn, BuiltinFunction, Heap, HeapObject, Reference,
        },
        VmValue,
    },
    core::Container,
    domain::{Dunder, ModuleName, Type},
};

pub struct BuiltinInstances {
    pub none: Reference,
    pub true_obj: Reference,
    pub false_obj: Reference,
}

pub struct BuiltinTypes {
    pub none: Reference,
    pub int: Reference,
    pub float: Reference,
    pub str: Reference,
    pub bool: Reference,
    pub list: Reference,
    pub dict: Reference,
    pub tuple: Reference,
    pub range: Reference,
    pub list_iter: Reference,
    pub tuple_iter: Reference,
    pub range_iter: Reference,
    pub function: Reference,
    pub method: Reference,
    pub builtin_function: Reference,
    pub generator: Reference,
    pub coroutine: Reference,
    pub module: Reference,
    pub exception: Reference,
    pub object: Reference,
    pub r#type: Reference,
    pub code: Reference,
}

impl BuiltinTypes {
    fn from_map(mut type_map: HashMap<Type, Reference>) -> BuiltinTypes {
        BuiltinTypes {
            none: type_map.remove(&Type::None).unwrap(),
            int: type_map.remove(&Type::Int).unwrap(),
            float: type_map.remove(&Type::Float).unwrap(),
            str: type_map.remove(&Type::Str).unwrap(),
            bool: type_map.remove(&Type::Bool).unwrap(),
            list: type_map.remove(&Type::List).unwrap(),
            dict: type_map.remove(&Type::Dict).unwrap(),
            tuple: type_map.remove(&Type::Tuple).unwrap(),
            range: type_map.remove(&Type::Range).unwrap(),
            list_iter: type_map.remove(&Type::ListIter).unwrap(),
            tuple_iter: type_map.remove(&Type::TupleIter).unwrap(),
            range_iter: type_map.remove(&Type::RangeIter).unwrap(),
            function: type_map.remove(&Type::Function).unwrap(),
            method: type_map.remove(&Type::Method).unwrap(),
            builtin_function: type_map.remove(&Type::BuiltinFunction).unwrap(),
            generator: type_map.remove(&Type::Generator).unwrap(),
            coroutine: type_map.remove(&Type::Coroutine).unwrap(),
            module: type_map.remove(&Type::Module).unwrap(),
            exception: type_map.remove(&Type::Exception).unwrap(),
            object: type_map.remove(&Type::Object).unwrap(),
            r#type: type_map.remove(&Type::Type).unwrap(),
            code: type_map.remove(&Type::Code).unwrap(),
        }
    }
}

fn init_type_classes(heap: &mut Heap) -> HashMap<Type, Reference> {
    let mut type_map = HashMap::new();

    let type_ref = heap.allocate_type();
    type_map.insert(Type::Type, type_ref);

    let obj = HeapObject::new(
        type_ref,
        VmValue::Class(Class::new_builtin(Type::Object.to_string())),
    );
    let object_ref = heap.allocate(obj);
    type_map.insert(Type::Object, object_ref);

    for type_ in Type::all()
        .iter()
        // these are handled separately
        .filter(|t| !matches!(t, Type::Type | Type::Object))
    {
        let obj = HeapObject::new(
            type_ref,
            VmValue::Class(Class::new_builtin(type_.to_string())),
        );
        let class_ref = heap.allocate(obj);
        type_map.insert(*type_, class_ref);
    }

    type_map
}

fn write_name_to_type_classes(type_map: &HashMap<Type, Reference>, heap: &mut Heap) {
    let str_ref = *type_map.get(&Type::Str).unwrap();
    for type_ in Type::all().iter() {
        let name_ref = heap.allocate(HeapObject::new(str_ref, VmValue::Str(type_.to_string())));

        let type_ref = *type_map.get(type_).unwrap();
        heap.get_mut(type_ref)
            .unwrap()
            .payload
            .as_class_mut()
            .unwrap()
            .write(Dunder::Name, name_ref);
    }
}

pub struct Runtime {
    pub heap: Heap,

    module_store: HashMap<ModuleName, Container<Module>>,

    pub builtin_types: BuiltinTypes,

    pub builtin_instances: BuiltinInstances,
}

impl Runtime {
    pub fn new() -> Self {
        let mut heap = Heap::new();

        // The type_map is a bootstrap-only registry to help us construct BuiltinType and
        // initialize the global scope in the builtins module.
        let type_map = init_type_classes(&mut heap);
        write_name_to_type_classes(&type_map, &mut heap);
        let builtin_mod = builtins::init_module(&mut heap, &type_map);
        let builtin_types = BuiltinTypes::from_map(type_map);

        let none = heap.allocate(HeapObject::new(builtin_types.none, VmValue::None));
        let true_obj = heap.allocate(HeapObject::new(builtin_types.bool, VmValue::Bool(true)));
        let false_obj = heap.allocate(HeapObject::new(builtin_types.bool, VmValue::Bool(false)));

        let builtin_instances = BuiltinInstances {
            none,
            true_obj,
            false_obj,
        };

        let async_mod = asyncio::init_module(&mut heap, builtin_types.builtin_function);

        let mut runtime = Self {
            heap,
            builtin_types,
            builtin_instances,
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
    class: Reference,
    builtins: &[(&str, BuiltinFn)],
) {
    for (name, func) in builtins {
        let obj = HeapObject::new(
            class,
            VmValue::BuiltinFunction(BuiltinFunction::new(name, *func)),
        );
        let func_ref = heap.allocate(obj);
        module.write(name, func_ref);
    }
}
