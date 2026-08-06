use crate::{
    domain::{Dunder, Encoding, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::{Exception, Str},
        utils::{BoundArgs, Parameter, Signature},
    },
};

/// A immutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Bytes;

impl_typed!(Bytes, Type::Bytes);
impl_method_provider!(Bytes, [NewBuiltin, DecodeBuiltin]);

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct DecodeBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional("source", TreewalkValue::Bytes(vec![])).positional_only(),
            Parameter::optional_without_default("encoding"),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let source = args.get("source").clone();
        let encoding = args.get_optional("encoding");

        let bytes = match (source, encoding) {
            (TreewalkValue::Bytes(_), Some(_)) => {
                return Exception::type_error("encoding without a string argument")
                    .raise(interpreter);
            }
            (TreewalkValue::Bytes(b), None) => b,
            (TreewalkValue::Str(s), Some(encoding)) => {
                let encoding_str = encoding.as_string().raise(interpreter)?;
                let encoding = Encoding::try_from(encoding_str.as_str()).raise(interpreter)?;
                s.encode(encoding)
            }
            (TreewalkValue::Str(_), None) => {
                return Exception::type_error("string argument without an encoding")
                    .raise(interpreter);
            }
            _ => return Exception::type_error("cannot convert object to bytes").raise(interpreter),
        };

        Ok(TreewalkValue::Bytes(bytes))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for DecodeBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("self").positional_only(),
            Parameter::optional(
                "encoding",
                TreewalkValue::Str(Str::new(Encoding::default().to_string())),
            ),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let bytes = args.get("self").as_bytes().raise(interpreter)?;
        let encoding_str = args.get("encoding").as_string().raise(interpreter)?;
        let encoding = Encoding::try_from(encoding_str.as_str()).raise(interpreter)?;
        let str_value = Str::decode(&bytes, encoding).raise(interpreter)?;
        Ok(TreewalkValue::Str(str_value))
    }

    fn name(&self) -> String {
        "decode".into()
    }
}
