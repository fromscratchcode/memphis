use std::fmt::{Display, Formatter, Result};

use crate::{
    core::floats_equal,
    domain::{
        utils::{format_bytes, format_comma_separated, format_comma_separated_with},
        MemphisException,
    },
};

/// A common implementation to represent the return value of a Python expression for use in tests,
/// REPL, or other read-only contexts. This frees each engine up to implement their return values
/// as they like, provided the [`From`] trait is implemented.
#[derive(Clone, Debug)]
pub enum MemphisValue {
    None,
    Int(i64),
    Float(f64),
    Complex(f64, f64),
    Str(String),
    Bool(bool),
    List(Vec<MemphisValue>),
    Tuple(Vec<MemphisValue>),
    Set(Vec<MemphisValue>),
    FrozenSet(Vec<MemphisValue>),
    Dict(Vec<(MemphisValue, MemphisValue)>),
    DictItems(Vec<(MemphisValue, MemphisValue)>),
    DictKeys(Vec<MemphisValue>),
    DictValues(Vec<MemphisValue>),
    MappingProxy(Vec<(MemphisValue, MemphisValue)>),
    Range(i64, i64, i64),
    Slice(Option<i64>, Option<i64>, Option<i64>),
    Bytes(Vec<u8>),
    ByteArray(Vec<u8>),
    Class(String),
    Object(String),
    Module(String),
    Ellipsis,
    NotImplemented,
    Super,
    Classmethod,
    Staticmethod,
    Property,
    StrIter,
    BytesIter,
    ByteArrayIter,
    ListIter,
    ReversedIter,
    SetIter,
    DictItemsIter,
    DictKeysIter,
    DictValuesIter,
    RangeIter,
    TupleIter,
    Zip,
    Code,
    Cell,
    Exception(MemphisException),
    Traceback,
    Frame,
    Generator,
    Coroutine,
    DataDescriptor,
    NonDataDescriptor,
    TypeNode,
    BuiltinFunction(String),
    BuiltinMethod(String),
    Function(String),
    Method(String),
}

impl MemphisValue {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn as_exception(&self) -> Option<&MemphisException> {
        match self {
            Self::Exception(e) => Some(e),
            _ => None,
        }
    }
}

impl PartialEq for MemphisValue {
    /// Equality for `MemphisValue` is intentionally **restricted**.
    ///
    /// This implementation only supports equality for primitive and value-like variants (e.g.
    /// numbers, strings, lists, tuples, ranges).
    /// These comparisons are used primarily at the **UI / REPL / testing boundary** to validate
    /// Memphis behavior.
    ///
    /// For non-primitive values (functions, iterators, frames, generators, descriptors, etc.),
    /// equality is **not defined**. Attempting to compare such values will panic loudly rather
    /// than silently returning `false`.
    ///
    /// This is a deliberate design choice:
    /// - It avoids misleading identity-based equality.
    /// - It prevents accidental deep comparisons of runtime objects.
    /// - It forces tests to be explicit about what kinds of values may be compared.
    ///
    /// In short: if two values can be meaningfully compared in Memphis, regardless their
    /// originating engine, equality works. Otherwise, it fails fast.
    fn eq(&self, other: &Self) -> bool {
        use MemphisValue::*;

        match (self, other) {
            (None, None) => true,
            (Bool(a), Bool(b)) => a == b,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => floats_equal(*a, *b),
            (Complex(ar, ai), Complex(br, bi)) => ar == br && ai == bi,
            (Str(a), Str(b)) => a == b,
            (List(a), List(b)) => a == b,
            (Dict(a), Dict(b)) => a == b,
            (Tuple(a), Tuple(b)) => a == b,
            (Set(a), Set(b)) => a == b,
            (Range(a1, a2, a3), Range(b1, b2, b3)) => a1 == b1 && a2 == b2 && a3 == b3,
            (Slice(a1, a2, a3), Slice(b1, b2, b3)) => a1 == b1 && a2 == b2 && a3 == b3,
            _ => panic!(
                "PartialEq not supported for MemphisValue variants: {:?} == {:?}",
                self, other
            ),
        }
    }
}

impl Display for MemphisValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MemphisValue::None => write!(f, "None"),
            MemphisValue::Int(i) => write!(f, "{i}"),
            MemphisValue::Float(i) => {
                let f_val = if i.fract() == 0.0 {
                    // integer-like float, force ".0"
                    format!("{}.0", i.trunc())
                } else {
                    format!("{}", i)
                };
                write!(f, "{f_val}")
            }
            MemphisValue::Complex(re, im) => {
                if *re == 0.0 {
                    write!(f, "{}j", im)
                } else {
                    write!(f, "({}+{}j)", re, im)
                }
            }
            // TODO this should be wrapped by '' in REPL mode but not for f-strings and the
            // print builtin. we'll need a way to differentiate those contexts
            MemphisValue::Str(s) => write!(f, "{s}"),
            MemphisValue::Bool(b) => match b {
                true => write!(f, "True"),
                false => write!(f, "False"),
            },
            MemphisValue::List(i) => {
                write!(f, "[{}]", format_comma_separated(i))
            }
            MemphisValue::Tuple(i) => {
                write!(f, "({})", format_comma_separated(i))
            }
            MemphisValue::Set(i) => {
                write!(f, "{{{}}}", format_comma_separated(i))
            }
            MemphisValue::FrozenSet(i) => {
                write!(f, "frozenset({{{}}})", format_comma_separated(i))
            }
            MemphisValue::Dict(i) => {
                let formatted =
                    format_comma_separated_with(i, |pair| format!("'{}': {}", pair.0, pair.1));
                write!(f, "{{{formatted}}}")
            }
            MemphisValue::DictItems(i) => {
                let formatted =
                    format_comma_separated_with(i, |pair| format!("('{}', {})", pair.0, pair.1));
                write!(f, "dict_items([{formatted}])")
            }
            MemphisValue::DictKeys(i) => {
                let formatted = format_comma_separated_with(i, |key| format!("'{key}'"));
                write!(f, "dict_keys([{formatted}])")
            }
            MemphisValue::DictValues(i) => {
                write!(f, "dict_values([{}])", format_comma_separated(i))
            }
            MemphisValue::MappingProxy(i) => {
                let formatted =
                    format_comma_separated_with(i, |pair| format!("'{}': {}", pair.0, pair.1));
                write!(f, "mappingproxy({{{formatted}}})")
            }
            MemphisValue::Range(start, stop, step) => {
                if *step == 1 {
                    write!(f, "range({}, {})", start, stop)
                } else {
                    write!(f, "range({}, {}, {})", start, stop, step)
                }
            }
            MemphisValue::Slice(start, stop, step) => {
                let format_val =
                    |val: &Option<i64>| val.map_or("None".to_string(), |number| number.to_string());

                let start = format_val(start);
                let stop = format_val(stop);
                let step = format_val(step);

                write!(f, "slice({start}, {stop}, {step})")
            }
            MemphisValue::Bytes(b) => {
                write!(f, "{}", format_bytes(b))
            }
            MemphisValue::ByteArray(b) => {
                write!(f, "bytearray({})", format_bytes(b))
            }
            MemphisValue::Class(name) => write!(f, "<class '{}'>", name),
            MemphisValue::Object(name) => write!(f, "<object {}>", name),
            MemphisValue::Module(name) => write!(f, "<module '{}'>", name),
            MemphisValue::Ellipsis => write!(f, "Ellipsis"),
            MemphisValue::NotImplemented => write!(f, "NotImplemented"),
            MemphisValue::Super => write!(f, "<super>"),
            MemphisValue::Classmethod => write!(f, "<classmethod>"),
            MemphisValue::Staticmethod => write!(f, "<staticmethod>"),
            MemphisValue::Property => write!(f, "<property>"),
            MemphisValue::StrIter => write!(f, "<str_ascii_iterator>"),
            MemphisValue::BytesIter => write!(f, "<bytes_iterator>"),
            MemphisValue::ByteArrayIter => write!(f, "<byte_array_iterator>"),
            MemphisValue::ListIter => write!(f, "<list_iterator>"),
            MemphisValue::ReversedIter => write!(f, "<list_reverseiterator>"),
            MemphisValue::SetIter => write!(f, "<set_iterator>"),
            MemphisValue::DictItemsIter => write!(f, "<dict_itemiterator>"),
            MemphisValue::DictKeysIter => write!(f, "<dict_keyiterator>"),
            MemphisValue::DictValuesIter => write!(f, "<dict_valueiterator>"),
            MemphisValue::RangeIter => write!(f, "<range_iterator>"),
            MemphisValue::TupleIter => write!(f, "<tuple_iterator>"),
            MemphisValue::Zip => write!(f, "<zip>"),
            MemphisValue::Code => write!(f, "<code object>"),
            MemphisValue::Cell => write!(f, "<cell>"),
            MemphisValue::Exception(e) => {
                write!(f, "{:?}({})", e.kind, format_comma_separated(&e.payload))
            }
            MemphisValue::Traceback => write!(f, "<traceback>"),
            MemphisValue::Frame => write!(f, "<frame>"),
            MemphisValue::Generator => write!(f, "<generator object>"),
            MemphisValue::Coroutine => write!(f, "<coroutine object>"),
            MemphisValue::DataDescriptor => write!(f, "<attribute>"),
            MemphisValue::NonDataDescriptor => write!(f, "<non-data attribute>"),
            MemphisValue::TypeNode => write!(f, "<type>"),
            MemphisValue::BuiltinFunction(name) => write!(f, "<built-in function {}>", name),
            MemphisValue::BuiltinMethod(name) => write!(f, "<built-in method {}>", name),
            MemphisValue::Function(name) => write!(f, "<function {}>", name),
            MemphisValue::Method(name) => write!(f, "<bound method {}>", name),
        }
    }
}
