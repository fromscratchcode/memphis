use crate::{
    core::Container,
    domain::{Dunder, Encoding, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{BoundArgs, Parameter, Signature},
    },
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct ByteArray(Vec<u8>);

impl_typed!(ByteArray, Type::ByteArray);
impl_method_provider!(ByteArray, [NewBuiltin]);

impl ByteArray {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub fn raw(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Clone)]
struct NewBuiltin;

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
        Ok(TreewalkValue::ByteArray(Container::new(ByteArray::new(
            bytes,
        ))))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
