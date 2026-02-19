use std::collections::HashMap;

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead, IndexWrite, TryEvalFrom},
        result::Raise,
        type_system::CloneableIterable,
        types::{iterators::DictKeysIter, DictItems, DictKeys, DictValues, Exception},
        utils::{check_args, Args, HashKey},
        DomainResult, SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, PartialEq, Clone)]
pub struct Dict {
    items: HashMap<HashKey, (TreewalkValue, TreewalkValue)>,
}

impl_typed!(Dict, Type::Dict);
impl_method_provider!(
    Dict,
    [
        NewBuiltin,
        InitBuiltin,
        GetBuiltin,
        DictKeysBuiltin,
        DictValuesBuiltin,
        DictItemsBuiltin,
    ]
);

impl Dict {
    pub fn from_items(items: Vec<(TreewalkValue, TreewalkValue)>) -> DomainResult<Self> {
        let mut dict = Dict::default();
        for (k, v) in items {
            dict.insert(k, v)?;
        }
        Ok(dict)
    }

    pub fn insert(&mut self, key: TreewalkValue, value: TreewalkValue) -> DomainResult<()> {
        let hashed_key = key.as_hash_key()?;
        self.items.insert(hashed_key, (key, value));
        Ok(())
    }

    fn get(&self, key: &TreewalkValue) -> Option<TreewalkValue> {
        let key = key.as_hash_key().expect("Unhashable key");
        if let Some((_, val)) = self.items.get(&key) {
            Some(val.clone())
        } else {
            None
        }
    }

    pub fn has(&self, key: &TreewalkValue) -> bool {
        let key = key.as_hash_key().expect("Unhashable key");
        self.items.contains_key(&key)
    }

    /// Convert this to `DictItems`, which can subsequently become `DictKeys` or `DictValues`. This
    /// currently sorts the items before returning the object, which doesn't technically match
    /// Python's implementation, but makes our lives way easier.
    pub fn items(&self) -> DictItems {
        let mut items = Vec::with_capacity(self.items.len());

        for (key, value) in self.items.values() {
            items.push((key.clone(), value.clone()));
        }

        items.sort();
        DictItems::new(items)
    }

    pub fn keys(&self) -> DictKeys {
        let mut keys: Vec<_> = self.items.values().map(|(key, _)| key.clone()).collect();
        keys.sort();
        DictKeys::new(keys)
    }

    pub fn values(&self) -> DictValues {
        let mut values: Vec<_> = self
            .items
            .values()
            .map(|(_, value)| value.clone())
            .collect();
        values.sort();
        DictValues::new(values)
    }

    /// Turn this `Dict` into a `SymbolTable`, which is another key-value store but where the keys
    /// are all confirmed to be valid Python identifiers.
    pub fn to_symbol_table(&self) -> DomainResult<SymbolTable> {
        let mut table = HashMap::new();

        let dict_items = self.items();
        for pair in dict_items {
            let tuple = pair.as_tuple()?;
            let key = tuple.first().as_str()?;
            let value = tuple.second();
            table.insert(key, value);
        }

        Ok(table)
    }

    pub fn extend(&mut self, other: &Dict) {
        for (key, value) in &other.items {
            self.items.insert(key.clone(), value.clone());
        }
    }
}

impl TryEvalFrom for Container<Dict> {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        match value {
            TreewalkValue::Dict(i) => Ok(i.clone()),
            val if val.clone().as_iterable().is_ok() => {
                let iter = val.as_iterator().raise(interpreter)?;
                let items = build_dict_items_from_iterable(iter).raise(interpreter)?;
                let dict = Dict::from_items(items).raise(interpreter)?;
                Ok(Container::new(dict))
            }
            _ => Exception::type_error("Expected a dict").raise(interpreter),
        }
    }
}

fn build_dict_items_from_iterable(
    iter: Box<dyn CloneableIterable>,
) -> DomainResult<Vec<(TreewalkValue, TreewalkValue)>> {
    let mut pairs: Vec<(TreewalkValue, TreewalkValue)> = vec![];
    for (index, item) in iter.enumerate() {
        // The item is often a tuple, but can really be any iterable which yields 2 values.
        let pair: Vec<_> = item.as_iterator()?.collect();

        // We cannot convert directly from a Vec to a tuple, we must first attempt to convert
        // to an array of a known and fixed length of 2.
        let pair_arr: [TreewalkValue; 2] = pair.clone().try_into().map_err(|_| {
            Exception::value_error(format!(
                "dictionary update sequence element #{} has length {}; 2 is required",
                index,
                pair.len()
            ))
        })?;

        pairs.push(pair_arr.into());
    }

    Ok(pairs)
}

/// We can reuse `DictKeysIterator` here because an iterator over a `Dict` will just return its
/// keys by default.
impl IntoIterator for Container<Dict> {
    type Item = TreewalkValue;
    type IntoIter = DictKeysIter;

    fn into_iter(self) -> Self::IntoIter {
        let dict_keys = self.borrow().keys();
        DictKeysIter::new(dict_keys)
    }
}

impl IndexRead for Container<Dict> {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        let value = self
            .borrow()
            .get(&index)
            .ok_or_else(|| Exception::key_error(&index))
            .raise(interpreter)?;
        Ok(value)
    }
}

impl IndexWrite for Container<Dict> {
    fn setitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        let key = index.as_hash_key().raise(interpreter)?;
        self.borrow_mut().items.insert(key, (index, value));
        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<()> {
        let key = index.as_hash_key().raise(interpreter)?;
        self.borrow_mut().items.remove(&key);
        Ok(())
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct InitBuiltin;
#[derive(Clone)]
struct GetBuiltin;
#[derive(Clone)]
struct DictItemsBuiltin;
#[derive(Clone)]
struct DictKeysBuiltin;
#[derive(Clone)]
struct DictValuesBuiltin;

impl Callable for DictItemsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;
        let dict_items = dict.borrow().items();
        Ok(TreewalkValue::DictItems(dict_items))
    }

    fn name(&self) -> String {
        "items".into()
    }
}

impl Callable for DictKeysBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;
        let dict_keys = dict.borrow().keys();
        Ok(TreewalkValue::DictKeys(dict_keys))
    }

    fn name(&self) -> String {
        "keys".into()
    }
}

impl Callable for DictValuesBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;
        let dict_values = dict.borrow().values();
        Ok(TreewalkValue::DictValues(dict_values))
    }

    fn name(&self) -> String {
        "values".into()
    }
}

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Dict(Container::new(Dict::default())))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for InitBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [0, 1].contains(&len)).raise(interpreter)?;

        let output = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;

        if let Some(pos_arg) = args.get_arg_optional(0) {
            let input = Container::<Dict>::try_eval_from(pos_arg, interpreter)?;
            output.borrow_mut().extend(&input.borrow());
        }

        if args.has_kwargs() {
            let kwargs = args.kwargs_as_runtime_dict();
            output.borrow_mut().extend(&kwargs);
        }

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

impl Callable for GetBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len)).raise(interpreter)?;

        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;

        let key = args.get_arg(0);

        let d = dict.borrow().clone();
        let value = if let Some(val) = d.get(&key) {
            val
        } else {
            args.get_arg_optional(1).unwrap_or(TreewalkValue::None)
        };

        Ok(value)
    }

    fn name(&self) -> String {
        "get".into()
    }
}
