use crate::{
    core::Container,
    domain::{Dunder, ExceptionKind, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, NonDataDescriptor},
        result::Raise,
        types::{Class, Exception},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl_typed!(StopIteration, Type::StopIteration);
impl_method_provider!(StopIteration, [NewBuiltin,]);
impl_descriptor_provider!(StopIteration, [ValueAttribute,]);

#[derive(Clone)]
pub struct StopIteration;

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len >= 1).raise(interpreter)?;

        // The first arg to Dunder::New will be the class itself, which should not become part of
        // the exception payload.
        let payload = args.args()[1..].to_vec();
        Ok(TreewalkValue::Exception(Exception::new(
            ExceptionKind::StopIteration,
            payload,
        )))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

#[derive(Clone)]
struct ValueAttribute;

impl NonDataDescriptor for ValueAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(TreewalkValue::Exception(exception))
                if exception.kind == ExceptionKind::StopIteration =>
            {
                exception.first_arg_or_none()
            }
            _ => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        "value".to_string()
    }
}
