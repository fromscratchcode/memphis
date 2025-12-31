use crate::{
    domain::{Dunder, ExceptionKind, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct TypeError;

impl_typed!(TypeError, Type::TypeError);
impl_method_provider!(TypeError, [NewBuiltin,]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len >= 1).raise(interpreter)?;

        // The first arg to Dunder::New will be the class itself, which should not become part of
        // the exception payload.
        let payload = args.args()[1..].to_vec();
        Ok(TreewalkValue::Exception(Exception::new(
            ExceptionKind::TypeError,
            payload,
        )))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
