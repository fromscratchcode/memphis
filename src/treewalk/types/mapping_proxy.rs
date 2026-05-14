use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        types::{Dict, DictItems},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
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
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let object = args
            .get_self()
            .raise(interpreter)?
            .as_mapping_proxy()
            .raise(interpreter)?;
        let index = args.get_arg(0);

        let value = object.0.borrow().getitem(&index).raise(interpreter)?;
        Ok(value)
    }

    fn name(&self) -> String {
        Dunder::GetItem.into()
    }
}
