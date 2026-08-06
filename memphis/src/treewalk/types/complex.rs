use crate::{
    domain::{Dunder, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{BoundArgs, Parameter, Signature},
    },
};

const DEFAULT_RE: f64 = 0.0;
const DEFAULT_IM: f64 = 0.0;

#[derive(Clone, PartialEq)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

impl_typed!(Complex, Type::Complex);
impl_method_provider!(Complex, [NewBuiltin]);

impl Complex {
    pub fn new(re: f64, im: f64) -> Self {
        Self { re, im }
    }

    fn from_str(input: &str) -> Option<Self> {
        // Remove the trailing 'j' character
        if !input.ends_with('j') {
            return None;
        }
        let input = &input[..input.len() - 1];

        // Find the position of the '+' or '-' sign for the imaginary part
        let mut split_pos = None;
        for (i, c) in input.char_indices().rev() {
            if c == '+' || c == '-' {
                split_pos = Some(i);
                break;
            }
        }

        let (real_str, imag_str) = input.split_at(split_pos?);

        let real_part = real_str.parse::<f64>().ok()?;
        let imag_part = imag_str.parse::<f64>().ok()?;

        Some(Self::new(real_part, imag_part))
    }
}

/// The __new__ method directly creates a complex number with the given parameters. For an
/// immutable built-in type like complex, the __init__ method typically does nothing so we do not
/// need to add it here. This is because the complex object is already fully initialized by the
/// time __init__ is called, and since it is immutable, its state cannot be changed after creation.
#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional("real_or_str", TreewalkValue::Float(DEFAULT_RE)),
            Parameter::optional_without_default("imag"),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let real = args.get("real_or_str");
        let imag = args.get_optional("imag");
        let complex = match (real, imag) {
            (TreewalkValue::Str(_), Some(_)) => {
                return Exception::type_error(
                    "complex() can't take second arg if first is a string",
                )
                .raise(interpreter);
            }
            (TreewalkValue::Str(s), None) => Complex::from_str(s)
                .ok_or_else(|| Exception::type_error("Expected a complex number"))
                .raise(interpreter)?,
            (real, Some(imag)) => Complex::new(
                real.coerce_to_float().raise(interpreter)?,
                imag.coerce_to_float().raise(interpreter)?,
            ),
            (real, None) => Complex::new(real.coerce_to_float().raise(interpreter)?, DEFAULT_IM),
        };

        Ok(TreewalkValue::Complex(complex))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
