use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::{Callable, NonDataDescriptor},
        result::Raise,
        types::{Class, Dict, MappingProxy, Tuple},
        utils::{BoundArgs, Signature},
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
        let symbol_table = match instance {
            Some(instance) => instance
                .as_class()
                .raise(interpreter)?
                .borrow()
                .symbol_table()
                .clone(),
            None => owner.borrow().symbol_table().clone(),
        };

        let dict = Dict::from_symbol_table(&symbol_table);
        Ok(TreewalkValue::MappingProxy(MappingProxy::new(
            Container::new(dict),
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
    fn signature(&self) -> Signature {
        // type() with 1 arg is special-cased and by Callable for Container<Class>
        Signature::positional_only(["mcls", "name", "bases", "dict"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let mcls = args.get("mcls").as_class().raise(interpreter)?;
        let name = args.get("name").as_string().raise(interpreter)?;
        let bases = args
            .get("bases")
            .as_tuple()
            .raise(interpreter)?
            .into_iter()
            .map(|c| c.as_class())
            .collect::<DomainResult<Vec<_>>>()
            .raise(interpreter)?;

        // Default to the `Type::Object` class.
        let parent_classes = if bases.is_empty() {
            vec![interpreter.state.class_of_type(&Type::Object)]
        } else {
            bases
        };

        let dict = args.get("dict").as_symbol_table().raise(interpreter)?;

        let class = Class::new_direct(name, Some(mcls), parent_classes, dict);
        Ok(TreewalkValue::Class(Container::new(class)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
