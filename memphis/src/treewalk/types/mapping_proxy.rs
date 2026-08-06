use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::{Dict, DictItems},
        utils::{BoundArgs, Signature},
    },
};

/// A read-only view into a `Dict`. This is used by Python for things like `Dunder::Dict`.
#[derive(Clone, PartialEq)]
pub struct MappingProxy(Container<Dict>);

impl_typed!(MappingProxy, Type::MappingProxy);
impl_method_provider!(MappingProxy, [GetItemBuiltin,]);

impl MappingProxy {
    pub fn new(dict: Container<Dict>) -> Self {
        Self(dict)
    }

    pub fn to_items(&self) -> DictItems {
        self.0.borrow().items()
    }
}

#[derive(Clone)]
struct GetItemBuiltin;

impl Callable for GetItemBuiltin {
    fn signature(&self) -> crate::treewalk::utils::Signature {
        Signature::positional_only(["self", "index"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("self").as_mapping_proxy().raise(interpreter)?;
        let index = args.get("index");
        let value = object.0.borrow().getitem(index).raise(interpreter)?;
        Ok(value)
    }

    fn name(&self) -> String {
        Dunder::GetItem.into()
    }
}
