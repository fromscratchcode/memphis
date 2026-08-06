use crate::{
    domain::{Dunder, Type},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::Exception,
        utils::{BoundArgs, Parameter, Signature},
    },
};

pub struct Int;

impl_typed!(Int, Type::Int);
impl_method_provider!(
    Int,
    [
        NewBuiltin,
        AddBuiltin,
        SubBuiltin,
        MulBuiltin,
        TruedivBuiltin,
        FloordivBuiltin,
        ModBuiltin,
        AndBuiltin,
        OrBuiltin,
        XorBuiltin,
        LshiftBuiltin,
        RshiftBuiltin,
        PowBuiltin,
        LtBuiltin,
        LeBuiltin,
        GtBuiltin,
        GeBuiltin,
    ]
);

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct SubBuiltin;
#[derive(Clone)]
struct MulBuiltin;
#[derive(Clone)]
struct TruedivBuiltin;
#[derive(Clone)]
struct FloordivBuiltin;
#[derive(Clone)]
struct ModBuiltin;
#[derive(Clone)]
struct AndBuiltin;
#[derive(Clone)]
struct OrBuiltin;
#[derive(Clone)]
struct XorBuiltin;
#[derive(Clone)]
struct LshiftBuiltin;
#[derive(Clone)]
struct RshiftBuiltin;
#[derive(Clone)]
struct PowBuiltin;
#[derive(Clone)]
struct LtBuiltin;
#[derive(Clone)]
struct LeBuiltin;
#[derive(Clone)]
struct GtBuiltin;
#[derive(Clone)]
struct GeBuiltin;

fn parse_int_constructor_arg(val: &TreewalkValue) -> DomainResult<i64> {
    match val {
        TreewalkValue::Int(i) => Ok(*i),
        TreewalkValue::Float(f) => Ok(*f as i64),
        TreewalkValue::Str(s) => s
            .parse::<i64>()
            .map_err(|_| Exception::value_error("Invalid int literal")),
        _ => Err(Exception::type_error("Cannot coerce to an int")),
    }
}

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional("val", TreewalkValue::Int(0)),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let int = parse_int_constructor_arg(args.get("val")).raise(interpreter)?;
        Ok(TreewalkValue::Int(int))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AddBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a + b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float((a as f64) + b))
        } else {
            Exception::type_error("unsupported operand type(s) for +").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for SubBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a - b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float((a as f64) - b))
        } else {
            Exception::type_error("unsupported operand type(s) for -").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Sub.into()
    }
}

impl Callable for MulBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a * b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Float((a as f64) * b))
        } else {
            Exception::type_error("unsupported operand type(s) for *").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
    }
}

impl Callable for TruedivBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Exception::div_by_zero_error("integer division or modulo by zero")
                    .raise(interpreter);
            }
            Ok(TreewalkValue::Float((a as f64) / (b as f64)))
        } else if let TreewalkValue::Float(b) = b {
            if b == 0.0 {
                return Exception::div_by_zero_error("integer division or modulo by zero")
                    .raise(interpreter);
            }
            Ok(TreewalkValue::Float((a as f64) / b))
        } else {
            Exception::type_error("unsupported operand type(s) for /").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Truediv.into()
    }
}

impl Callable for FloordivBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Exception::div_by_zero_error("integer division or modulo by zero")
                    .raise(interpreter);
            }
            Ok(TreewalkValue::Int(a / b))
        } else if let TreewalkValue::Float(b) = b {
            if b == 0.0 {
                return Exception::div_by_zero_error("integer division or modulo by zero")
                    .raise(interpreter);
            }
            Ok(TreewalkValue::Float((a as f64 / b).floor()))
        } else {
            Exception::type_error("unsupported operand type(s) for //").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Floordiv.into()
    }
}

impl Callable for ModBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            if b == 0 {
                return Exception::div_by_zero_error("integer division or modulo by zero")
                    .raise(interpreter);
            }
            Ok(TreewalkValue::Int(a % b))
        } else {
            Exception::type_error("unsupported operand type(s) for %").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Mod.into()
    }
}

impl Callable for AndBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a & b))
        } else {
            Exception::type_error("unsupported operand type(s) for &").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::And.into()
    }
}

impl Callable for OrBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a | b))
        } else {
            Exception::type_error("unsupported operand type(s) for |").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Or.into()
    }
}

impl Callable for XorBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a ^ b))
        } else {
            Exception::type_error("unsupported operand type(s) for ^").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Xor.into()
    }
}

impl Callable for LshiftBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            if b > 100 {
                // TODO support long ranges. This is found in _collections_abc.py
                // longrange_iterator = type(iter(range(1 << 1000)))
                Ok(TreewalkValue::Int(a << 10))
            } else {
                Ok(TreewalkValue::Int(a << b))
            }
        } else {
            Exception::type_error("unsupported operand type(s) for <<").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Lshift.into()
    }
}

impl Callable for RshiftBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Int(a >> b))
        } else {
            Exception::type_error("unsupported operand type(s) for >>").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Rshift.into()
    }
}

impl Callable for PowBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            if b >= 0 {
                Ok(TreewalkValue::Int(a.pow(b as u32)))
            } else {
                Ok(TreewalkValue::Float((a as f64).powi(b as i32)))
            }
        } else {
            Exception::type_error("unsupported operand type(s) for **").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Pow.into()
    }
}

impl Callable for LtBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a < b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) < b))
        } else {
            Exception::type_error("unsupported operand type(s) for <").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Lt.into()
    }
}

impl Callable for LeBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a <= b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) <= b))
        } else {
            Exception::type_error("unsupported operand type(s) for <=").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Le.into()
    }
}

impl Callable for GtBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a > b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) > b))
        } else {
            Exception::type_error("unsupported operand type(s) for >").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Gt.into()
    }
}

impl Callable for GeBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_int().raise(interpreter)?;
        let b = args.get("b").clone();

        if let TreewalkValue::Int(b) = b {
            Ok(TreewalkValue::Bool(a >= b))
        } else if let TreewalkValue::Float(b) = b {
            Ok(TreewalkValue::Bool((a as f64) >= b))
        } else {
            Exception::type_error("unsupported operand type(s) for >=").raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::Ge.into()
    }
}
