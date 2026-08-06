use std::{ops::Deref, str};

use crate::{
    core::Container,
    domain::{Dunder, Encoding, Type, utils::normalize_index},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        types::{Exception, List, Slice},
        utils::{BoundArgs, Parameter, Signature},
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
        UpperBuiltin,
        EncodeBuiltin,
        GetItemBuiltin,
    ]
);
impl_iterable!(StrIter);

impl Str {
    pub fn new(str: impl Into<String>) -> Self {
        Self(str.into())
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
struct UpperBuiltin;
#[derive(Clone)]
struct EncodeBuiltin;
#[derive(Clone)]
struct GetItemBuiltin;

impl Callable for AddBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "b"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        // implements a + b
        let a = args.get("self").as_string().raise(interpreter)?;
        let b = args.get("b").as_string().raise(interpreter)?;
        Ok(TreewalkValue::Str(Str::from(a + &b)))
    }

    fn name(&self) -> String {
        Dunder::Add.into()
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
        let a = args.get("self").as_string().raise(interpreter)?;
        let b = args.get("b").as_int().raise(interpreter)?;
        Ok(TreewalkValue::Str(Str::from(a.repeat(b as usize))))
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
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
        let a = args.get("self").as_string().raise(interpreter)?;
        let b = args.get("b").as_string().raise(interpreter)?;
        Ok(TreewalkValue::Bool(a < b))
    }

    fn name(&self) -> String {
        Dunder::Lt.into()
    }
}

impl Callable for ContainsBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "item"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let a = args.get("self").as_string().raise(interpreter)?;
        let b = args.get("item").as_string().raise(interpreter)?;
        Ok(TreewalkValue::Bool(a.contains(&b)))
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}

impl Callable for JoinBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "iterable"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let delim = args.get("self").as_string().raise(interpreter)?;
        let items = args.get("iterable").as_list().raise(interpreter)?;
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
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("self").positional_only(),
            Parameter::optional("sep", TreewalkValue::None),
            Parameter::optional("maxsplit", TreewalkValue::Int(-1)),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let text = args.get("self").as_string().raise(interpreter)?;
        let max_split = args.get("maxsplit").as_int().raise(interpreter)?;

        // str.split(sep=None, maxsplit=-1)
        if args.get("sep").is(&TreewalkValue::None) {
            let parts = if max_split < 0 {
                collect_parts(text.split_whitespace())
            } else {
                let mut parts = Vec::new();
                let mut remaining = text.as_str();

                for _ in 0..max_split {
                    let trimmed = remaining.trim_start_matches(char::is_whitespace);
                    if trimmed.is_empty() {
                        break;
                    }

                    let word_end = trimmed.find(char::is_whitespace).unwrap_or(trimmed.len());
                    parts.push(TreewalkValue::Str(Str::new(&trimmed[..word_end])));
                    remaining = &trimmed[word_end..];
                }

                let final_part = remaining.trim_start_matches(char::is_whitespace);
                if !final_part.is_empty() {
                    parts.push(TreewalkValue::Str(Str::new(final_part)));
                }
                parts
            };

            return Ok(TreewalkValue::List(Container::new(List::new(parts))));
        }

        let delim = args.get("sep").as_string().raise(interpreter)?;
        if delim.is_empty() {
            return Exception::value_error("empty separator").raise(interpreter);
        }

        let parts = if max_split < 0 {
            // Negative values for max split are ignored
            collect_parts(text.split(&delim))
        } else {
            // Python's value for maxsplit is the number of splits done, while Rust
            // interprets it as the number of items in the resulting list. Therefore,
            // we must add one.
            collect_parts(text.splitn((max_split as usize) + 1, &delim))
        };

        Ok(TreewalkValue::List(Container::new(List::new(parts))))
    }

    fn name(&self) -> String {
        "split".into()
    }
}

impl Callable for LowerBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let text = args.get("self").as_string().raise(interpreter)?;
        Ok(TreewalkValue::Str(Str::from(text.to_lowercase())))
    }

    fn name(&self) -> String {
        "lower".into()
    }
}

impl Callable for UpperBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let text = args.get("self").as_string().raise(interpreter)?;
        Ok(TreewalkValue::Str(Str::from(text.to_uppercase())))
    }

    fn name(&self) -> String {
        "upper".into()
    }
}

impl Callable for EncodeBuiltin {
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
        let text = args.get("self").as_string().raise(interpreter)?;
        let encoding_str = args.get("encoding").as_string().raise(interpreter)?;
        let encoding = Encoding::try_from(encoding_str.as_str()).raise(interpreter)?;
        Ok(TreewalkValue::Bytes(Str::from(text).encode(encoding)))
    }

    fn name(&self) -> String {
        "encode".into()
    }
}

impl Callable for GetItemBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "subscript"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("self").as_str().raise(interpreter)?;
        let index = args.get("subscript");

        let value = match index {
            TreewalkValue::Int(i) => object
                .get_normalized(*i)
                .map(TreewalkValue::Str)
                .ok_or_else(|| Exception::index_error("string index out of range"))
                .raise(interpreter)?,
            TreewalkValue::Slice(s) => TreewalkValue::Str(object.slice(s)),
            _ => {
                return Exception::type_error(format!(
                    "string indices must be integers, not '{}'",
                    interpreter.state.type_name(index)
                ))
                .raise(interpreter);
            }
        };

        Ok(value)
    }

    fn name(&self) -> String {
        Dunder::GetItem.into()
    }
}
