use std::{ops::Deref, str};

use crate::{
    core::Container,
    domain::{utils::normalize_index, Dunder, Encoding, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        types::{Exception, List, Slice},
        utils::{check_args, Args},
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct Str(String);

impl_typed!(Str, Type::Str);
impl_method_provider!(
    Str,
    [
        AddBuiltin,
        MulBuiltin,
        LtBuiltin,
        ContainsBuiltin,
        JoinBuiltin,
        SplitBuiltin,
        LowerBuiltin,
        EncodeBuiltin,
        GetItemBuiltin,
    ]
);
impl_iterable!(StrIter);

impl Str {
    pub fn new(str: &str) -> Self {
        Self(str.to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn decode(bytes: &[u8], encoding: Encoding) -> DomainResult<Self> {
        let str = match encoding {
            Encoding::Utf8 => str::from_utf8(bytes).map_err(|_| {
                Exception::value_error(format!("failed to decode with encoding '{encoding}'"))
            })?,
        };

        Ok(Self::new(str))
    }

    pub fn encode(&self, encoding: Encoding) -> Vec<u8> {
        if encoding != Encoding::Utf8 {
            unimplemented!("Rust only supports utf-8 in std");
        }

        self.0.as_bytes().to_vec()
    }

    fn len(&self) -> usize {
        self.0.chars().count()
    }

    fn get(&self, index: usize) -> Option<Self> {
        self.get_char(index).map(|c| c.to_string()).map(Str::from)
    }

    fn get_char(&self, index: usize) -> Option<char> {
        self.0.chars().nth(index)
    }

    fn get_normalized(&self, index: i64) -> Option<Self> {
        normalize_index(index, self.len()).and_then(|idx| self.get(idx))
    }

    fn slice(&self, slice: &Slice) -> Self {
        let sliced_string = slice
            .apply(self.len(), |i| {
                self.get_char(i as usize).map(|c| c.to_string())
            })
            .join("");
        Str::from(sliced_string)
    }
}

impl From<String> for Str {
    fn from(s: String) -> Self {
        Str(s)
    }
}

impl Deref for Str {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl IntoIterator for Str {
    type Item = TreewalkValue;
    type IntoIter = StrIter;

    fn into_iter(self) -> Self::IntoIter {
        StrIter::new(self)
    }
}

#[derive(Clone)]
pub struct StrIter {
    string: String,
    position: usize,
}

impl StrIter {
    pub fn new(string: Str) -> Self {
        Self {
            string: string.0.clone(),
            position: 0,
        }
    }
}

impl Iterator for StrIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.string[self.position..].chars().next()?;
        self.position += result.len_utf8();
        Some(TreewalkValue::Str(Str::from(result.to_string())))
    }
}

#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct MulBuiltin;
#[derive(Clone)]
struct LtBuiltin;
#[derive(Clone)]
struct ContainsBuiltin;
#[derive(Clone)]
struct JoinBuiltin;
#[derive(Clone)]
struct SplitBuiltin;
#[derive(Clone)]
struct LowerBuiltin;
#[derive(Clone)]
struct EncodeBuiltin;
#[derive(Clone)]
struct GetItemBuiltin;

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        // implements a + b
        let a = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;
        let b = args.get_arg(0).as_string().raise(interpreter)?;

        Ok(TreewalkValue::Str(Str::from(a + &b)))
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for MulBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;
        let n = args.get_arg(0).as_int().raise(interpreter)?;

        Ok(TreewalkValue::Str(Str::from(a.repeat(n as usize))))
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
    }
}

impl Callable for LtBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;
        let b = args.get_arg(0).as_string().raise(interpreter)?;
        Ok(TreewalkValue::Bool(a < b))
    }

    fn name(&self) -> String {
        Dunder::Lt.into()
    }
}

impl Callable for ContainsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let a = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;
        let b = args.get_arg(0).as_string().raise(interpreter)?;

        Ok(TreewalkValue::Bool(a.contains(&b)))
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}

impl Callable for JoinBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let delim = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;
        let items = args.get_arg(0).as_list().raise(interpreter)?;
        let joined = items.borrow().join(&delim).raise(interpreter)?;

        Ok(TreewalkValue::Str(Str::from(joined)))
    }

    fn name(&self) -> String {
        "join".into()
    }
}

fn collect_parts<'a>(iter: impl Iterator<Item = &'a str>) -> Vec<TreewalkValue> {
    iter.map(|s| TreewalkValue::Str(Str::new(s))).collect()
}

impl Callable for SplitBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [0, 1, 2].contains(&len)).raise(interpreter)?;

        let text = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;

        // This first clause is essentially saying the first positional arg has a default.
        // str.split(sep=None, maxsplit=-1)
        // But we don't have a great way to model that on builtins right now.
        let parts = if args.is_empty() || args.get_arg(0).is(&TreewalkValue::None) {
            collect_parts(text.split_whitespace())
        } else {
            let delim = args.get_arg(0).as_string().raise(interpreter)?;
            if delim.is_empty() {
                return Exception::value_error("empty separator").raise(interpreter);
            }

            // TODO this whole thing would be a lot simpler if we had a way to handle default
            // arguments on builtin function signatures
            let max_split = args
                .get_arg_optional(1)
                .map(|i| i.as_int())
                .transpose()
                .raise(interpreter)?;
            match max_split {
                // Negative values for max split are ignored
                None | Some(..=-1) => collect_parts(text.split(&delim)),
                Some(max_split) => {
                    // Python's value for maxsplit is the number of splits done, while Rust
                    // interprets it as the number of items in the resulting list. Therefore,
                    // we must add one.
                    collect_parts(text.splitn((max_split as usize) + 1, &delim))
                }
            }
        };

        Ok(TreewalkValue::List(Container::new(List::new(parts))))
    }

    fn name(&self) -> String {
        "split".into()
    }
}

impl Callable for LowerBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let text = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;
        Ok(TreewalkValue::Str(Str::from(text.to_lowercase())))
    }

    fn name(&self) -> String {
        "lower".into()
    }
}

impl Callable for EncodeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [0, 1].contains(&len)).raise(interpreter)?;
        let text = args
            .get_self()
            .raise(interpreter)?
            .as_string()
            .raise(interpreter)?;

        let encoding = match args.len() {
            0 => Encoding::default(),
            1 => {
                let encoding_str = args.get_arg(0).as_string().raise(interpreter)?;
                Encoding::try_from(encoding_str.as_str()).raise(interpreter)?
            }
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Bytes(Str::from(text).encode(encoding)))
    }

    fn name(&self) -> String {
        "encode".into()
    }
}

impl Callable for GetItemBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let object = args
            .get_self()
            .raise(interpreter)?
            .as_str()
            .raise(interpreter)?;
        let index = args.get_arg(0);

        let value = match index {
            TreewalkValue::Int(i) => object
                .get_normalized(i)
                .map(TreewalkValue::Str)
                .ok_or_else(|| Exception::index_error("string index out of range"))
                .raise(interpreter)?,
            TreewalkValue::Slice(s) => TreewalkValue::Str(object.slice(&s)),
            _ => {
                return Exception::type_error(format!(
                    "string indices must be integers, not '{}'",
                    interpreter.state.type_name(&index)
                ))
                .raise(interpreter)
            }
        };

        Ok(value)
    }

    fn name(&self) -> String {
        Dunder::GetItem.into()
    }
}
