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
    pub ellipsis: Reference,
    pub notimplemented: Reference,
    pub int: Reference,
    pub float: Reference,
    pub str: Reference,
    pub bool: Reference,
    pub list: Reference,
    pub set: Reference,
    pub frozenset: Reference,
    pub zip: Reference,
    pub dict: Reference,
    pub dict_items: Reference,
    pub dict_keys: Reference,
    pub dict_values: Reference,
    pub mappingproxy: Reference,
    pub tuple: Reference,
    pub range: Reference,
    pub slice: Reference,
    pub complex: Reference,
    pub bytes: Reference,
    pub bytearray: Reference,
    pub memoryview: Reference,
    pub list_iter: Reference,
    pub tuple_iter: Reference,
    pub range_iter: Reference,
    pub str_iter: Reference,
    pub reversed: Reference,
    pub set_iter: Reference,
    pub dict_itemiter: Reference,
    pub dict_keyiter: Reference,
    pub dict_valueiter: Reference,
    pub bytes_iter: Reference,
    pub byte_array_iter: Reference,
    pub function: Reference,
    pub method: Reference,
    pub builtin_function: Reference,
    pub builtin_method: Reference,
    pub generator: Reference,
    pub coroutine: Reference,
    pub module: Reference,
    pub object: Reference,
    pub r#type: Reference,
    pub code: Reference,
    pub cell: Reference,
    pub frame: Reference,
    pub classmethod: Reference,
    pub staticmethod: Reference,
    pub super_: Reference,
    pub getset_descriptor: Reference,
    pub member_descriptor: Reference,
    pub property: Reference,
    pub traceback: Reference,
    pub baseexception: Reference,
    pub exception: Reference,
    pub stopiteration: Reference,
    pub typeerror: Reference,
    pub zerodivisionerror: Reference,
    pub runtimeerror: Reference,
    pub importerror: Reference,
    pub lookuperror: Reference,
    pub keyerror: Reference,
    pub indexerror: Reference,
    pub valueerror: Reference,
    pub nameerror: Reference,
    pub attributeerror: Reference,
    pub assertionerror: Reference,
    pub syntaxerror: Reference,
    pub ioerror: Reference,
}

impl BuiltinTypes {
    fn from_map(mut type_map: HashMap<Type, Reference>) -> BuiltinTypes {
        BuiltinTypes {
            none: type_map.remove(&Type::None).unwrap(),
            ellipsis: type_map.remove(&Type::Ellipsis).unwrap(),
            notimplemented: type_map.remove(&Type::NotImplemented).unwrap(),
            int: type_map.remove(&Type::Int).unwrap(),
            float: type_map.remove(&Type::Float).unwrap(),
            str: type_map.remove(&Type::Str).unwrap(),
            bool: type_map.remove(&Type::Bool).unwrap(),
            list: type_map.remove(&Type::List).unwrap(),
            set: type_map.remove(&Type::Set).unwrap(),
            frozenset: type_map.remove(&Type::FrozenSet).unwrap(),
            zip: type_map.remove(&Type::Zip).unwrap(),
            dict: type_map.remove(&Type::Dict).unwrap(),
            dict_items: type_map.remove(&Type::DictItems).unwrap(),
            dict_keys: type_map.remove(&Type::DictKeys).unwrap(),
            dict_values: type_map.remove(&Type::DictValues).unwrap(),
            mappingproxy: type_map.remove(&Type::MappingProxy).unwrap(),
            tuple: type_map.remove(&Type::Tuple).unwrap(),
            range: type_map.remove(&Type::Range).unwrap(),
            complex: type_map.remove(&Type::Complex).unwrap(),
            slice: type_map.remove(&Type::Slice).unwrap(),
            bytes: type_map.remove(&Type::Bytes).unwrap(),
            bytearray: type_map.remove(&Type::ByteArray).unwrap(),
            list_iter: type_map.remove(&Type::ListIter).unwrap(),
            tuple_iter: type_map.remove(&Type::TupleIter).unwrap(),
            range_iter: type_map.remove(&Type::RangeIter).unwrap(),
            bytes_iter: type_map.remove(&Type::BytesIter).unwrap(),
            byte_array_iter: type_map.remove(&Type::ByteArrayIter).unwrap(),
            str_iter: type_map.remove(&Type::StrIter).unwrap(),
            set_iter: type_map.remove(&Type::SetIter).unwrap(),
            dict_itemiter: type_map.remove(&Type::DictItemIter).unwrap(),
            dict_keyiter: type_map.remove(&Type::DictKeyIter).unwrap(),
            dict_valueiter: type_map.remove(&Type::DictValueIter).unwrap(),
            reversed: type_map.remove(&Type::ReversedIter).unwrap(),
            function: type_map.remove(&Type::Function).unwrap(),
            method: type_map.remove(&Type::Method).unwrap(),
            builtin_function: type_map.remove(&Type::BuiltinFunction).unwrap(),
            builtin_method: type_map.remove(&Type::BuiltinMethod).unwrap(),
            getset_descriptor: type_map.remove(&Type::GetSetDescriptor).unwrap(),
            member_descriptor: type_map.remove(&Type::MemberDescriptor).unwrap(),
            property: type_map.remove(&Type::Property).unwrap(),
            super_: type_map.remove(&Type::Super).unwrap(),
            classmethod: type_map.remove(&Type::Classmethod).unwrap(),
            staticmethod: type_map.remove(&Type::Staticmethod).unwrap(),
            generator: type_map.remove(&Type::Generator).unwrap(),
            coroutine: type_map.remove(&Type::Coroutine).unwrap(),
            module: type_map.remove(&Type::Module).unwrap(),
            object: type_map.remove(&Type::Object).unwrap(),
            r#type: type_map.remove(&Type::Type).unwrap(),
            code: type_map.remove(&Type::Code).unwrap(),
            cell: type_map.remove(&Type::Cell).unwrap(),
            frame: type_map.remove(&Type::Frame).unwrap(),
            traceback: type_map.remove(&Type::Traceback).unwrap(),
            memoryview: type_map.remove(&Type::Memoryview).unwrap(),
            baseexception: type_map.remove(&Type::BaseException).unwrap(),
            exception: type_map.remove(&Type::Exception).unwrap(),
            typeerror: type_map.remove(&Type::TypeError).unwrap(),
            zerodivisionerror: type_map.remove(&Type::ZeroDivisionError).unwrap(),
            stopiteration: type_map.remove(&Type::StopIteration).unwrap(),
            attributeerror: type_map.remove(&Type::AttributeError).unwrap(),
            assertionerror: type_map.remove(&Type::AssertionError).unwrap(),
            importerror: type_map.remove(&Type::ImportError).unwrap(),
            indexerror: type_map.remove(&Type::IndexError).unwrap(),
            ioerror: type_map.remove(&Type::IOError).unwrap(),
            keyerror: type_map.remove(&Type::KeyError).unwrap(),
            lookuperror: type_map.remove(&Type::LookupError).unwrap(),
            runtimeerror: type_map.remove(&Type::RuntimeError).unwrap(),
            valueerror: type_map.remove(&Type::ValueError).unwrap(),
            nameerror: type_map.remove(&Type::NameError).unwrap(),
            syntaxerror: type_map.remove(&Type::SyntaxError).unwrap(),
        }
    }

    pub fn get(&self, type_: &Type) -> Reference {
        match type_ {
            Type::Type => self.r#type,
            Type::TypeMeta => self.r#type,
            Type::Object => self.object,
            Type::ObjectMeta => self.object,
            Type::Super => self.super_,
            Type::GetSetDescriptor => self.getset_descriptor,
            Type::MemberDescriptor => self.member_descriptor,
            Type::Method => self.method,
            Type::Function => self.function,
            Type::BuiltinFunction => self.builtin_function,
            Type::BuiltinMethod => self.builtin_method,
            Type::Generator => self.generator,
            Type::Coroutine => self.coroutine,
            Type::Ellipsis => self.ellipsis,
            Type::None => self.none,
            Type::NotImplemented => self.notimplemented,
            Type::Bool => self.bool,
            Type::Int => self.int,
            Type::Float => self.float,
            Type::Str => self.str,
            Type::List => self.list,
            Type::Set => self.set,
            Type::FrozenSet => self.frozenset,
            Type::Zip => self.zip,
            Type::Tuple => self.tuple,
            Type::Range => self.range,
            Type::Slice => self.slice,
            Type::Complex => self.complex,
            Type::Bytes => self.bytes,
            Type::ByteArray => self.bytearray,
            Type::Memoryview => self.memoryview,
            Type::Dict => self.dict,
            Type::DictItems => self.dict_items,
            Type::DictKeys => self.dict_keys,
            Type::DictValues => self.dict_values,
            Type::MappingProxy => self.mappingproxy,
            Type::StrIter => self.str_iter,
            Type::ListIter => self.list_iter,
            Type::ReversedIter => self.reversed,
            Type::SetIter => self.set_iter,
            Type::TupleIter => self.tuple_iter,
            Type::DictItemIter => self.dict_itemiter,
            Type::DictKeyIter => self.dict_keyiter,
            Type::DictValueIter => self.dict_valueiter,
            Type::BytesIter => self.bytes_iter,
            Type::ByteArrayIter => self.byte_array_iter,
            Type::RangeIter => self.range_iter,
            Type::Module => self.module,
            Type::Cell => self.cell,
            Type::Code => self.code,
            Type::Classmethod => self.classmethod,
            Type::Staticmethod => self.staticmethod,
            Type::Property => self.property,
            Type::Traceback => self.traceback,
            Type::Frame => self.frame,
            Type::BaseException => self.baseexception,
            Type::Exception => self.exception,
            Type::StopIteration => self.stopiteration,
            Type::TypeError => self.typeerror,
            Type::ZeroDivisionError => self.zerodivisionerror,
            Type::RuntimeError => self.runtimeerror,
            Type::ImportError => self.importerror,
            Type::LookupError => self.lookuperror,
            Type::KeyError => self.keyerror,
            Type::IndexError => self.indexerror,
            Type::ValueError => self.valueerror,
            Type::NameError => self.nameerror,
            Type::AttributeError => self.attributeerror,
            Type::AssertionError => self.assertionerror,
            Type::SyntaxError => self.syntaxerror,
            Type::IOError => self.ioerror,
        }
    }
}

fn init_builtin_types(heap: &mut Heap) -> BuiltinTypes {
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

    BuiltinTypes::from_map(type_map)
}

fn write_name_to_type_classes(builtin_types: &BuiltinTypes, heap: &mut Heap) {
    for type_ in Type::all().iter() {
        let name_ref = heap.allocate(HeapObject::new(
            builtin_types.str,
            VmValue::Str(type_.to_string()),
        ));

        let type_ref = builtin_types.get(type_);
        heap.get_mut(type_ref)
            .unwrap()
            .payload
            .as_class_mut()
            .unwrap()
            .write(Dunder::Name, name_ref);
    }
}

fn init_builtin_instances(heap: &mut Heap, builtin_types: &BuiltinTypes) -> BuiltinInstances {
    let none = heap.allocate(HeapObject::new(builtin_types.none, VmValue::None));
    let true_obj = heap.allocate(HeapObject::new(builtin_types.bool, VmValue::Bool(true)));
    let false_obj = heap.allocate(HeapObject::new(builtin_types.bool, VmValue::Bool(false)));

    BuiltinInstances {
        none,
        true_obj,
        false_obj,
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

        let builtin_types = init_builtin_types(&mut heap);
        write_name_to_type_classes(&builtin_types, &mut heap);

        let builtin_instances = init_builtin_instances(&mut heap, &builtin_types);

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
