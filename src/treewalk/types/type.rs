use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, NonDataDescriptor},
        result::Raise,
        types::{Class, Exception, MappingProxy, Tuple},
        utils::Args,
        DomainResult, Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// This represents the callable class `type` in Python. For an enum of all the builtin types, see
/// `types::interpreter::Type`.
pub struct TypeClass;

impl_typed!(TypeClass, Type::Type);
impl_method_provider!(TypeClass, [NewBuiltin]);
impl_descriptor_provider!(TypeClass, [DictAttribute, MroAttribute]);

#[derive(Clone)]
struct DictAttribute;
#[derive(Clone)]
struct MroAttribute;

impl NonDataDescriptor for DictAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let scope = match instance {
            Some(instance) => instance
                .as_class()
                .raise(interpreter)?
                .borrow()
                .scope
                .clone(),
            None => owner.borrow().scope.clone(),
        };

        Ok(TreewalkValue::MappingProxy(MappingProxy::new(
            Container::new(scope.to_runtime_dict()),
        )))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}

impl NonDataDescriptor for MroAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let mro = match instance {
            Some(instance) => instance
                .as_class()
                .raise(interpreter)?
                .mro()
                .iter()
                .cloned()
                .map(TreewalkValue::Class)
                .collect(),
            None => owner
                .mro()
                .iter()
                .cloned()
                .map(TreewalkValue::Class)
                .collect(),
        };
        Ok(TreewalkValue::Tuple(Tuple::new(mro)))
    }

    fn name(&self) -> String {
        Dunder::Mro.into()
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        match args.len() {
            2 => unreachable!("type() with 1 arg is special-cased and handled eslewhere"),
            4 => {
                let mcls = args.get_arg(0).as_class().raise(interpreter)?;
                let name = args.get_arg(1).as_str().raise(interpreter)?;
                // Default to the `Type::Object` class.
                let parent_classes = args
                    .get_arg(2)
                    .as_tuple()
                    .raise(interpreter)?
                    .into_iter()
                    .map(|c| c.as_class())
                    .collect::<DomainResult<Vec<_>>>()
                    .raise(interpreter)?;

                let parent_classes = if parent_classes.is_empty() {
                    vec![interpreter.state.class_of_type(&Type::Object)]
                } else {
                    parent_classes
                };

                let symbol_table = args.get_arg(3).as_symbol_table().raise(interpreter)?;

                let mut class = Class::new_direct(name, Some(mcls), parent_classes);
                class.scope = Scope::new(symbol_table);
                Ok(TreewalkValue::Class(Container::new(class)))
            }
            _ => Exception::type_error("type() takes 1 or 3 arguments").raise(interpreter),
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
