use crate::{
    core::Container,
    domain::{Dunder, ExceptionKind, MemphisException, Type},
    treewalk::{
        macros::*,
        protocols::NonDataDescriptor,
        types::{Class, Str, Traceback, Tuple},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct Exception {
    pub kind: ExceptionKind,
    pub payload: Vec<TreewalkValue>,
}

impl Exception {
    pub fn new(kind: ExceptionKind, payload: Vec<TreewalkValue>) -> Self {
        Self { kind, payload }
    }

    fn new_from_str(kind: ExceptionKind, msg: impl Into<String>) -> Self {
        Self::new(kind, vec![TreewalkValue::Str(Str::new(&msg.into()))])
    }

    fn new_empty(kind: ExceptionKind) -> Self {
        Self::new(kind, vec![])
    }

    pub fn get_type(&self) -> Type {
        self.kind.get_type()
    }

    pub fn runtime_error() -> Self {
        Self::new_empty(ExceptionKind::RuntimeError)
    }

    pub fn runtime_error_with(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::RuntimeError, msg)
    }

    pub fn type_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::TypeError, msg)
    }

    pub fn type_error_must_inherit_base_exception() -> Self {
        Self::new_from_str(
            ExceptionKind::TypeError,
            "catching classes that do not inherit from BaseException is not allowed",
        )
    }

    pub fn import_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::ImportError, msg)
    }

    pub fn syntax_error() -> Self {
        Self::new_empty(ExceptionKind::SyntaxError)
    }

    pub fn stop_iteration() -> Self {
        Self::new_empty(ExceptionKind::StopIteration)
    }

    pub fn stop_iteration_with(obj: TreewalkValue) -> Self {
        Self::new(ExceptionKind::StopIteration, vec![obj])
    }

    pub fn value_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::ValueError, msg)
    }

    pub fn name_error(name: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::NameError, name)
    }

    pub fn key_error(key: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::KeyError, key)
    }

    pub fn attribute_error(object_type: impl Into<String>, attr: impl Into<String>) -> Self {
        // We could store the real object here in the future, but right now looking up its type
        // name requires a `TreewalkInterpreter` so we do it before this.
        Self::new(
            ExceptionKind::AttributeError,
            vec![
                TreewalkValue::Str(Str::new(&object_type.into())),
                TreewalkValue::Str(Str::new(&attr.into())),
            ],
        )
    }

    pub fn assertion_error() -> Self {
        Self::new_empty(ExceptionKind::AssertionError)
    }

    pub fn div_by_zero_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::DivisionByZero, msg.into())
    }

    pub fn lookup_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::LookupError, msg)
    }

    pub fn unknown_encoding(encoding: impl Into<String>) -> Self {
        Self::lookup_error(format!("unknown encoding: {}", encoding.into()))
    }

    pub fn first_arg_or_none(&self) -> TreewalkValue {
        self.payload.first().cloned().unwrap_or(TreewalkValue::None)
    }
}

impl From<Exception> for MemphisException {
    fn from(value: Exception) -> Self {
        let payload = value.payload.iter().map(|i| (*i).clone().into()).collect();
        MemphisException::new(value.kind.clone(), payload)
    }
}

impl_typed!(Exception, Type::Exception);
impl_descriptor_provider!(Exception, [ArgsAttribute, TracebackAttribute]);

#[derive(Clone)]
struct ArgsAttribute;
#[derive(Clone)]
struct TracebackAttribute;

impl NonDataDescriptor for ArgsAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(TreewalkValue::Exception(e)) => {
                TreewalkValue::Tuple(Tuple::new(e.payload.clone()))
            }
            _ => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        "args".into()
    }
}

impl NonDataDescriptor for TracebackAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Traceback(Traceback))
    }

    fn name(&self) -> String {
        Dunder::Traceback.into()
    }
}
