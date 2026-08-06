use crate::{
    domain::{Dunder, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        utils::{BoundArgs, Parameter, Signature},
    },
};

pub struct Bool;

impl_typed!(Bool, Type::Bool);
impl_method_provider!(Bool, [NewBuiltin]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional("value", TreewalkValue::Bool(false)),
        ])
    }
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let input = args.get("value").coerce_to_bool();
        Ok(TreewalkValue::Bool(input))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
