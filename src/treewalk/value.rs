use std::{
    cell::{Ref, RefMut},
    cmp::Ordering,
    fmt::{Debug, Error, Formatter},
    hash::{Hash, Hasher},
    ptr,
};

#[cfg(feature = "c_stdlib")]
use crate::treewalk::types::cpython::{CPythonClass, CPythonModule, CPythonObject};
use crate::{
    core::{floats_equal, Container},
    domain::{MemphisValue, Type},
    treewalk::{
        protocols::MemberRead,
        type_system::{
            CloneableCallable, CloneableDataDescriptor, CloneableIterable,
            CloneableNonDataDescriptor,
        },
        types::{
            iterators::{
                DictItemsIter, DictKeysIter, DictValuesIter, GeneratorIter, ListIter, RangeIter,
                ReversedIter, SetIter, StrIter, TupleIter, ZipIterator,
            },
            ByteArray, Cell, Class, Classmethod, Code, Complex, Coroutine, Dict, DictItems,
            DictKeys, DictValues, Exception, FrozenSet, Function, List, MappingProxy, Method,
            Module, Object, Property, Range, Set, Slice, Staticmethod, Str, Super, Traceback,
            Tuple,
        },
        typing::TypeExpr,
        utils::HashKey,
        DomainResult, SymbolTable,
    },
};

#[derive(Clone)]
pub enum TreewalkValue {
    None,
    Ellipsis,
    NotImplemented,
    Int(i64),
    Float(f64),
    Str(Str),
    Class(Container<Class>),
    Object(Container<Object>),
    Module(Container<Module>),
    Super(Super),
    Classmethod(Classmethod),
    Staticmethod(Staticmethod),
    Property(Property),
    DataDescriptor(Box<dyn CloneableDataDescriptor>),
    NonDataDescriptor(Box<dyn CloneableNonDataDescriptor>),
    Function(Container<Function>),
    Method(Container<Method>),
    BuiltinFunction(Box<dyn CloneableCallable>),
    BuiltinMethod(Box<dyn CloneableCallable>),
    Generator(GeneratorIter),
    Coroutine(Container<Coroutine>),
    Code(Code),
    Cell(Container<Cell>),
    Bytes(Vec<u8>),
    ByteArray(Container<ByteArray>),
    Bool(bool),
    List(Container<List>),
    Set(Container<Set>),
    FrozenSet(FrozenSet),
    Zip(ZipIterator),
    Slice(Slice),
    Complex(Complex),
    Dict(Container<Dict>),
    DictItems(DictItems),
    DictKeys(DictKeys),
    DictValues(DictValues),
    MappingProxy(MappingProxy),
    Range(Range),
    Tuple(Tuple),
    Exception(Exception),
    Traceback(Traceback),
    Frame,
    ListIter(ListIter),
    ReversedIter(ReversedIter),
    SetIter(SetIter),
    DictItemsIter(DictItemsIter),
    DictKeysIter(DictKeysIter),
    DictValuesIter(DictValuesIter),
    RangeIter(RangeIter),
    TupleIter(TupleIter),
    StrIter(StrIter),
    BytesIter(Vec<u8>),
    ByteArrayIter(Vec<u8>),
    TypeNode(TypeExpr),
    #[cfg(feature = "c_stdlib")]
    CPythonModule(Container<CPythonModule>),
    #[cfg(feature = "c_stdlib")]
    CPythonObject(CPythonObject),
    #[cfg(feature = "c_stdlib")]
    CPythonClass(CPythonClass),
}

/// Implement PartialEq manually because Py<PyAny> does not implement PartialEq.
impl PartialEq for TreewalkValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TreewalkValue::None, TreewalkValue::None) => true,
            (TreewalkValue::Int(a), TreewalkValue::Int(b)) => a == b,
            (TreewalkValue::Float(a), TreewalkValue::Float(b)) => floats_equal(*a, *b),
            (TreewalkValue::Int(a), TreewalkValue::Float(b)) => floats_equal(*a as f64, *b),
            (TreewalkValue::Float(a), TreewalkValue::Int(b)) => floats_equal(*a, *b as f64),
            (TreewalkValue::Str(a), TreewalkValue::Str(b)) => a == b,
            (TreewalkValue::Bytes(a), TreewalkValue::Bytes(b)) => a == b,
            (TreewalkValue::ByteArray(a), TreewalkValue::ByteArray(b)) => a == b,
            (TreewalkValue::Bool(a), TreewalkValue::Bool(b)) => a == b,
            (TreewalkValue::List(a), TreewalkValue::List(b)) => a == b,
            (TreewalkValue::Set(a), TreewalkValue::Set(b)) => a == b,
            (TreewalkValue::FrozenSet(a), TreewalkValue::FrozenSet(b)) => a == b,
            (TreewalkValue::Complex(a), TreewalkValue::Complex(b)) => a == b,
            (TreewalkValue::Dict(a), TreewalkValue::Dict(b)) => a == b,
            (TreewalkValue::MappingProxy(a), TreewalkValue::MappingProxy(b)) => a == b,
            (TreewalkValue::DictItems(a), TreewalkValue::DictItems(b)) => a == b,
            (TreewalkValue::DictKeys(a), TreewalkValue::DictKeys(b)) => a == b,
            (TreewalkValue::DictValues(a), TreewalkValue::DictValues(b)) => a == b,
            (TreewalkValue::Range(a), TreewalkValue::Range(b)) => a == b,
            (TreewalkValue::Tuple(a), TreewalkValue::Tuple(b)) => a == b,
            (TreewalkValue::Function(a), TreewalkValue::Function(b)) => a == b,
            (TreewalkValue::Class(a), TreewalkValue::Class(b)) => a == b,
            (TreewalkValue::Object(a), TreewalkValue::Object(b)) => a.same_identity(b),
            (TreewalkValue::Exception(a), TreewalkValue::Exception(b)) => a == b,
            (TreewalkValue::BuiltinMethod(a), TreewalkValue::BuiltinMethod(b)) => {
                ptr::eq(a.as_ref(), b.as_ref())
            }
            (TreewalkValue::DataDescriptor(a), TreewalkValue::DataDescriptor(b)) => {
                ptr::eq(a.as_ref(), b.as_ref())
            }
            (TreewalkValue::NonDataDescriptor(a), TreewalkValue::NonDataDescriptor(b)) => {
                ptr::eq(a.as_ref(), b.as_ref())
            }
            _ => false,
        }
    }
}

// This is a marker trait. We are confirming that PartialEq fully satisfies equality semantics.
// We cannot derive Eq because it is not implemented for f64 because of NaN weirdness.
impl Eq for TreewalkValue {}

impl Hash for TreewalkValue {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if let TreewalkValue::Set(set) = self {
            for i in set.borrow().iter() {
                i.as_int().unwrap().hash(state)
            }
        }
    }
}

impl Ord for TreewalkValue {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (TreewalkValue::Str(s1), TreewalkValue::Str(s2)) => s1.cmp(s2),
            (TreewalkValue::Int(n1), TreewalkValue::Int(n2)) => n1.cmp(n2),
            _ => todo!(),
        }
    }
}

// Implement the PartialOrd trait, required by Ord
impl PartialOrd for TreewalkValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TreewalkValue {
    pub fn as_hash_key(&self) -> DomainResult<HashKey> {
        match self {
            TreewalkValue::Int(i) => Ok(HashKey::Int(*i)),
            TreewalkValue::Str(s) => Ok(HashKey::Str(s.to_string())),
            _ => Err(Exception::type_error("unhashable type")),
        }
    }

    pub fn hash(&self) -> usize {
        match self {
            TreewalkValue::Object(o) => o.address(),
            TreewalkValue::Class(o) => o.address(),
            TreewalkValue::Int(i) => *i as usize,
            _ => 0,
        }
    }

    /// Check for object identity, as opposed to object value evaluated in `PartialEq` above.
    pub fn is(&self, other: &Self) -> bool {
        match (self, other) {
            (TreewalkValue::None, TreewalkValue::None) => true,
            (TreewalkValue::None, _) | (_, TreewalkValue::None) => false,
            (TreewalkValue::Object(ref a), TreewalkValue::Object(ref b)) => a.same_identity(b),
            _ => unimplemented!(), // Different variants or not both TreewalkValue::Object
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            TreewalkValue::None => Type::None,
            TreewalkValue::Ellipsis => Type::Ellipsis,
            TreewalkValue::NotImplemented => Type::NotImplemented,
            TreewalkValue::Class(_) => Type::Type,
            TreewalkValue::Object(_) => Type::Object,
            TreewalkValue::Super(_) => Type::Super,
            TreewalkValue::Classmethod(_) => Type::Classmethod,
            TreewalkValue::Staticmethod(_) => Type::Staticmethod,
            TreewalkValue::Property(_) => Type::Property,
            TreewalkValue::DataDescriptor(_) => Type::GetSetDescriptor,
            TreewalkValue::NonDataDescriptor(_) => Type::MemberDescriptor,
            TreewalkValue::Method(_) => Type::Method,
            TreewalkValue::Function(_) => Type::Function,
            TreewalkValue::BuiltinFunction(_) => Type::BuiltinFunction,
            TreewalkValue::BuiltinMethod(_) => Type::BuiltinMethod,
            TreewalkValue::Generator(_) => Type::Generator,
            TreewalkValue::Coroutine(_) => Type::Coroutine,
            TreewalkValue::Int(_) => Type::Int,
            TreewalkValue::Float(_) => Type::Float,
            TreewalkValue::Bytes(_) => Type::Bytes,
            TreewalkValue::ByteArray(_) => Type::ByteArray,
            TreewalkValue::Bool(_) => Type::Bool,
            TreewalkValue::Str(_) => Type::Str,
            TreewalkValue::List(_) => Type::List,
            TreewalkValue::Set(_) => Type::Set,
            TreewalkValue::FrozenSet(_) => Type::FrozenSet,
            TreewalkValue::Zip(_) => Type::Zip,
            TreewalkValue::Tuple(_) => Type::Tuple,
            TreewalkValue::Range(_) => Type::Range,
            TreewalkValue::Slice(_) => Type::Slice,
            TreewalkValue::Complex(_) => Type::Complex,
            TreewalkValue::Dict(_) => Type::Dict,
            TreewalkValue::DictItems(_) => Type::DictItems,
            TreewalkValue::DictKeys(_) => Type::DictKeys,
            TreewalkValue::DictValues(_) => Type::DictValues,
            TreewalkValue::MappingProxy(_) => Type::MappingProxy,
            TreewalkValue::BytesIter(_) => Type::BytesIter,
            TreewalkValue::ByteArrayIter(_) => Type::ByteArrayIter,
            TreewalkValue::RangeIter(_) => Type::RangeIter,
            TreewalkValue::StrIter(_) => Type::StrIter,
            TreewalkValue::ListIter(_) => Type::ListIter,
            TreewalkValue::ReversedIter(_) => Type::ReversedIter,
            TreewalkValue::SetIter(_) => Type::SetIter,
            TreewalkValue::TupleIter(_) => Type::TupleIter,
            TreewalkValue::DictItemsIter(_) => Type::DictItemIter,
            TreewalkValue::DictKeysIter(_) => Type::DictKeyIter,
            TreewalkValue::DictValuesIter(_) => Type::DictValueIter,
            TreewalkValue::TypeNode(_) => Type::Type,
            TreewalkValue::Cell(_) => Type::Cell,
            TreewalkValue::Code(_) => Type::Code,
            TreewalkValue::Module(_) => Type::Module,
            TreewalkValue::Exception(e) => e.get_type(),
            TreewalkValue::Traceback(_) => Type::Traceback,
            TreewalkValue::Frame => Type::Frame,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(_) => Type::Module,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonObject(_) => Type::Object,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonClass(_) => unimplemented!(),
        }
    }

    /// Ensure this value is iterable, then convert it to an iterator (if needed).
    ///
    /// This mimics the behavior of calling `iter(x)` in Python.
    /// Used when interpreting `for x in y:` or any construct that expects an iterable.
    /// Raises a TypeError if the object is not iterable.
    pub fn as_iterator(&self) -> DomainResult<Box<dyn CloneableIterable>> {
        self.clone().as_iterable()?.as_iterator_strict()
    }

    /// Convert a value into its iterator form, consuming the value in the process.
    ///
    /// This corresponds to the behavior of Python’s `iter()`:
    /// for a list, string, set, etc., it returns a corresponding iterator.
    /// If the value is already an iterator, it is returned as-is.
    /// If the value is not iterable, returns None.
    #[allow(clippy::wrong_self_convention)]
    pub fn as_iterable(self) -> DomainResult<TreewalkValue> {
        let value = match self {
            TreewalkValue::List(list) => TreewalkValue::ListIter(list.into_iter()),
            TreewalkValue::ListIter(_) => self,
            TreewalkValue::Str(s) => TreewalkValue::StrIter(s.into_iter()),
            TreewalkValue::StrIter(_) => self,
            TreewalkValue::Set(set) => TreewalkValue::SetIter(set.into_iter()),
            TreewalkValue::FrozenSet(set) => TreewalkValue::SetIter(set.into_iter()),
            TreewalkValue::SetIter(_) => self,
            TreewalkValue::Tuple(tuple) => TreewalkValue::TupleIter(tuple.into_iter()),
            TreewalkValue::TupleIter(_) => self,
            TreewalkValue::Dict(dict) => TreewalkValue::DictKeysIter(dict.into_iter()),
            TreewalkValue::DictItems(dict) => TreewalkValue::DictItemsIter(dict.into_iter()),
            TreewalkValue::DictItemsIter(_) => self,
            TreewalkValue::DictKeys(dict) => TreewalkValue::DictKeysIter(dict.into_iter()),
            TreewalkValue::DictKeysIter(_) => self,
            TreewalkValue::DictValues(dict) => TreewalkValue::DictValuesIter(dict.into_iter()),
            TreewalkValue::DictValuesIter(_) => self,
            TreewalkValue::Bytes(b) => TreewalkValue::BytesIter(b),
            TreewalkValue::BytesIter(_) => self,
            TreewalkValue::ByteArray(b) => TreewalkValue::ByteArrayIter(b.borrow().raw().to_vec()),
            TreewalkValue::ByteArrayIter(_) => self,
            TreewalkValue::Range(r) => TreewalkValue::RangeIter(r.into_iter()),
            TreewalkValue::RangeIter(_) => self,
            TreewalkValue::Generator(_) => self,
            TreewalkValue::ReversedIter(_) => self,
            TreewalkValue::Zip(_) => self,
            _ => {
                return Err(Exception::type_error(format!(
                    "'{}' object is not iterable",
                    self.get_type()
                )))
            }
        };

        Ok(value)
    }

    pub fn coerce_to_int(&self) -> DomainResult<i64> {
        match self {
            TreewalkValue::Int(i) => Ok(*i),
            TreewalkValue::Str(s) => s
                .parse::<i64>()
                .map_err(|_| Exception::value_error("Invalid int literal")),
            _ => Err(Exception::type_error("Cannot coerce to an int")),
        }
    }

    pub fn coerce_to_float(&self) -> DomainResult<f64> {
        match self {
            TreewalkValue::Float(i) => Ok(*i),
            TreewalkValue::Int(i) => Ok(*i as f64),
            _ => Err(Exception::type_error("Cannot coerce to a float")),
        }
    }

    pub fn coerce_to_boolean(&self) -> bool {
        match self {
            TreewalkValue::Bool(i) => *i,
            TreewalkValue::List(i) => !i.borrow().is_empty(),
            TreewalkValue::Tuple(i) => !i.is_empty(),
            TreewalkValue::Str(i) => !i.is_empty(),
            TreewalkValue::Int(i) => *i != 0,
            TreewalkValue::Float(i) => *i != 0.0,
            TreewalkValue::None => false,
            _ => true,
        }
    }

    pub fn as_native_object<T: 'static>(&self) -> DomainResult<Ref<'_, T>> {
        match self {
            TreewalkValue::Object(obj) => {
                let binding = obj.borrow();
                Ref::filter_map(binding, |any| any.downcast_ref::<T>()).map_err(|_| {
                    Exception::type_error(format!(
                        "Expected native object of type {}",
                        std::any::type_name::<T>()
                    ))
                })
            }
            _ => Err(Exception::type_error(format!(
                "Expected native object of type {}",
                std::any::type_name::<T>()
            ))),
        }
    }

    pub fn as_native_object_mut<T: 'static>(&self) -> DomainResult<RefMut<'_, T>> {
        match self {
            TreewalkValue::Object(obj) => {
                let binding = obj.borrow_mut();
                RefMut::filter_map(binding, |any| any.downcast_mut::<T>()).map_err(|_| {
                    Exception::type_error(format!(
                        "Expected native object of type {}",
                        std::any::type_name::<T>()
                    ))
                })
            }
            _ => Err(Exception::type_error(format!(
                "Expected native object of type {}",
                std::any::type_name::<T>()
            ))),
        }
    }

    pub fn as_module(&self) -> DomainResult<Box<dyn MemberRead>> {
        match self {
            TreewalkValue::Module(i) => Ok(Box::new(i.borrow().clone())),
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(i) => Ok(Box::new(i.borrow().clone())),
            _ => Err(Exception::type_error("Expected an int")),
        }
    }

    pub fn as_symbol_table(&self) -> DomainResult<SymbolTable> {
        match self {
            TreewalkValue::Dict(dict) => Ok(dict.borrow().to_symbol_table()?),
            _ => Err(Exception::type_error("Expected a dict with str keys")),
        }
    }

    pub fn as_int(&self) -> DomainResult<i64> {
        match self {
            TreewalkValue::Int(i) => Ok(*i),
            _ => Err(Exception::type_error("Expected an int")),
        }
    }

    pub fn as_float(&self) -> DomainResult<f64> {
        match self {
            TreewalkValue::Float(i) => Ok(*i),
            _ => Err(Exception::type_error("Expected a float")),
        }
    }

    pub fn as_tuple(&self) -> DomainResult<Tuple> {
        match self {
            TreewalkValue::Tuple(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a tuple")),
        }
    }

    pub fn as_str(&self) -> DomainResult<String> {
        match self {
            TreewalkValue::Str(i) => Ok(i.to_string()),
            _ => Err(Exception::type_error("Expected a string")),
        }
    }

    pub fn as_dict(&self) -> DomainResult<Container<Dict>> {
        match self {
            TreewalkValue::Dict(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a dict")),
        }
    }

    pub fn as_class(&self) -> DomainResult<Container<Class>> {
        match self {
            TreewalkValue::Class(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a class")),
        }
    }

    pub fn as_coroutine(&self) -> DomainResult<Container<Coroutine>> {
        match self {
            TreewalkValue::Coroutine(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a coroutine")),
        }
    }

    pub fn as_function(&self) -> DomainResult<Container<Function>> {
        match self {
            TreewalkValue::Function(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a function")),
        }
    }

    pub fn as_list(&self) -> DomainResult<Container<List>> {
        match self {
            TreewalkValue::List(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a list")),
        }
    }

    pub fn as_object(&self) -> DomainResult<Container<Object>> {
        match self {
            TreewalkValue::Object(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected an object")),
        }
    }

    pub fn as_set(&self) -> DomainResult<Container<Set>> {
        match self {
            TreewalkValue::Set(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected a set")),
        }
    }

    pub fn as_exception(&self) -> DomainResult<Exception> {
        match self {
            TreewalkValue::Exception(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected an exception")),
        }
    }

    pub fn as_bytes(&self) -> DomainResult<Vec<u8>> {
        match self {
            TreewalkValue::Bytes(i) => Ok(i.clone()),
            _ => Err(Exception::type_error("Expected bytes")),
        }
    }

    pub fn negated(&self) -> Option<Self> {
        match self {
            TreewalkValue::Float(i) => Some(TreewalkValue::Float(-i)),
            TreewalkValue::Int(i) => Some(TreewalkValue::Int(-i)),
            _ => None,
        }
    }

    pub fn not(&self) -> Self {
        TreewalkValue::Bool(!self.coerce_to_boolean())
    }
}

impl Debug for TreewalkValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", MemphisValue::from(self.clone()))
    }
}

impl From<TreewalkValue> for MemphisValue {
    fn from(value: TreewalkValue) -> Self {
        match value {
            TreewalkValue::None => MemphisValue::None,
            TreewalkValue::Int(i) => MemphisValue::Integer(i),
            TreewalkValue::Float(i) => MemphisValue::Float(i),
            TreewalkValue::Str(s) => MemphisValue::Str(s.as_str().to_string()),
            TreewalkValue::Bool(val) => MemphisValue::Boolean(val),
            TreewalkValue::List(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::List(items)
            }
            TreewalkValue::Tuple(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::Tuple(items)
            }
            TreewalkValue::Set(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::Set(items)
            }
            TreewalkValue::FrozenSet(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::FrozenSet(items)
            }
            TreewalkValue::Ellipsis => MemphisValue::Ellipsis,
            TreewalkValue::NotImplemented => MemphisValue::NotImplemented,
            TreewalkValue::Class(c) => MemphisValue::Class(c.borrow().name().to_string()),
            TreewalkValue::Object(o) => {
                MemphisValue::Object(o.borrow().class().borrow().name().to_string())
            }
            TreewalkValue::Module(m) => MemphisValue::Module(m.borrow().name().to_string()),
            TreewalkValue::Super(_) => MemphisValue::Super,
            TreewalkValue::Classmethod(_) => MemphisValue::Classmethod,
            TreewalkValue::Staticmethod(_) => MemphisValue::Staticmethod,
            TreewalkValue::Property(_) => MemphisValue::Property,
            TreewalkValue::DataDescriptor(_) => MemphisValue::DataDescriptor,
            TreewalkValue::NonDataDescriptor(_) => MemphisValue::NonDataDescriptor,
            TreewalkValue::Function(f) => MemphisValue::Function(f.borrow().name().to_string()),
            TreewalkValue::Method(m) => MemphisValue::Method(m.borrow().name()),
            TreewalkValue::BuiltinFunction(f) => MemphisValue::BuiltinFunction(f.name()),
            TreewalkValue::BuiltinMethod(f) => MemphisValue::BuiltinMethod(f.name()),
            TreewalkValue::Generator(_) => MemphisValue::Generator,
            TreewalkValue::Coroutine(_) => MemphisValue::Coroutine,
            TreewalkValue::Code(_) => MemphisValue::Code,
            TreewalkValue::Cell(_) => MemphisValue::Cell,
            TreewalkValue::Bytes(b) => MemphisValue::Bytes(b),
            TreewalkValue::ByteArray(b) => MemphisValue::ByteArray(b.borrow().raw().to_vec()),
            TreewalkValue::Zip(_) => MemphisValue::Zip,
            TreewalkValue::Slice(s) => MemphisValue::Slice(s.start, s.stop, s.step),
            TreewalkValue::Range(r) => MemphisValue::Range(r.start, r.stop, r.step),
            TreewalkValue::Complex(c) => MemphisValue::Complex(c.re, c.im),
            TreewalkValue::Dict(i) => {
                let items = i
                    .borrow()
                    .items()
                    .items()
                    .iter()
                    .map(|(key, value)| (key.clone().into(), value.clone().into()))
                    .collect();
                MemphisValue::Dict(items)
            }
            TreewalkValue::DictItems(i) => {
                let items = i
                    .items()
                    .iter()
                    .map(|(key, value)| (key.clone().into(), value.clone().into()))
                    .collect();
                MemphisValue::DictItems(items)
            }
            TreewalkValue::DictKeys(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::DictKeys(items)
            }
            TreewalkValue::DictValues(i) => {
                let items = i
                    .into_iter()
                    .map(|item| item.into())
                    .collect::<Vec<MemphisValue>>();
                MemphisValue::DictValues(items)
            }
            TreewalkValue::MappingProxy(i) => {
                let items = i
                    .to_items()
                    .items()
                    .iter()
                    .map(|(key, value)| (key.clone().into(), value.clone().into()))
                    .collect();
                MemphisValue::MappingProxy(items)
            }
            TreewalkValue::Exception(_) => MemphisValue::Exception,
            TreewalkValue::Traceback(_) => MemphisValue::Traceback,
            TreewalkValue::Frame => MemphisValue::Frame,
            TreewalkValue::ListIter(_) => MemphisValue::ListIter,
            TreewalkValue::ReversedIter(_) => MemphisValue::ReversedIter,
            TreewalkValue::SetIter(_) => MemphisValue::SetIter,
            TreewalkValue::DictItemsIter(_) => MemphisValue::DictItemsIter,
            TreewalkValue::DictKeysIter(_) => MemphisValue::DictKeysIter,
            TreewalkValue::DictValuesIter(_) => MemphisValue::DictValuesIter,
            TreewalkValue::RangeIter(_) => MemphisValue::RangeIter,
            TreewalkValue::TupleIter(_) => MemphisValue::TupleIter,
            TreewalkValue::StrIter(_) => MemphisValue::StrIter,
            TreewalkValue::BytesIter(_) => MemphisValue::BytesIter,
            TreewalkValue::ByteArrayIter(_) => MemphisValue::ByteArrayIter,
            TreewalkValue::TypeNode(_) => MemphisValue::TypeNode,
            #[cfg(feature = "c_stdlib")]
            TreewalkValue::CPythonModule(_)
            | TreewalkValue::CPythonObject(_)
            | TreewalkValue::CPythonClass(_) => unimplemented!(),
        }
    }
}
