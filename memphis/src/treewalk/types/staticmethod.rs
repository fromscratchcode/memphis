use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::{Callable, NonDataDescriptor},
        types::Class,
        utils::{BoundArgs, Signature},
    },
};

#[derive(Clone)]
pub struct Staticmethod(Box<TreewalkValue>);

impl_typed!(Staticmethod, Type::Staticmethod);
impl_method_provider!(Staticmethod, [NewBuiltin]);

impl Staticmethod {
    fn new(func: Box<TreewalkValue>) -> Self {
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
        _interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let function = args.get("func");
        Ok(TreewalkValue::Staticmethod(Staticmethod::new(Box::new(
            function.clone(),
        ))))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Staticmethod {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(*self.0.clone())
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
