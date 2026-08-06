use crate::{
    domain::{Dunder, ExceptionKind, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{BoundArgs, Signature},
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct BaseException;

impl_typed!(BaseException, Type::BaseException);
impl_method_provider!(BaseException, [NewBuiltin,]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["cls"]).with_varargs("args")
    }
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let class = args.get("cls").as_class().raise(interpreter)?;
        let class_ref = class.borrow();
        // TODO this path currently panics for user-defined exception classes. The full fix here is
        // to store the Container<Class> on the Exception instead of the enum ExceptionKind.
        let type_ = class_ref.builtin_type();
        let kind = ExceptionKind::from_type(type_);

        // The first arg to Dunder::New will be the class itself, which should not become part of
        // the exception payload.
        let payload = args.get_varargs("args").items().to_vec();
        Ok(TreewalkValue::Exception(Exception::new(kind, payload)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
