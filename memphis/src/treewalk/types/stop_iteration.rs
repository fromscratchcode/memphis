use crate::{
    core::Container,
    domain::{ExceptionKind, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue, macros::*,
        protocols::NonDataDescriptor, types::Class,
    },
};

impl_typed!(StopIteration, Type::StopIteration);
impl_descriptor_provider!(StopIteration, [ValueAttribute,]);

#[derive(Clone)]
pub struct StopIteration;

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
