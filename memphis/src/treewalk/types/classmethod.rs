use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::{Callable, NonDataDescriptor},
        result::Raise,
        type_system::CloneableCallable,
        types::{Class, Method},
        utils::{BoundArgs, Signature},
    },
};

#[derive(Clone)]
pub struct Classmethod(Box<dyn CloneableCallable>);

impl_typed!(Classmethod, Type::Classmethod);
impl_method_provider!(Classmethod, [NewBuiltin]);

impl Classmethod {
    fn new(func: Box<dyn CloneableCallable>) -> Self {
        Self(func)
    }
}

#[derive(Clone)]
pub struct NewBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["cls", "func"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        // This is a workaround for Generic type behavior found in _collections_abc.py.
        // _weakrefset.py also uses this.
        //
        // ```
        // GenericAlias = type(list[int])
        // __class_getitem__ = classmethod(GenericAlias)
        // ```
        if args.get("func").as_class().is_ok() {
            return Ok(TreewalkValue::None);
        }

        let function = args.get("func").clone().as_callable().raise(interpreter)?;
        Ok(TreewalkValue::Classmethod(Classmethod::new(function)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Classmethod {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Method(Container::new(Method::new(
            TreewalkValue::Class(owner),
            self.0.clone(),
        ))))
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
