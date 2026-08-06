use std::collections::HashMap;

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        DomainResult, SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        iterator::{collect, for_each_mut},
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        type_system::CloneableIterable,
        types::{DictItems, DictKeys, DictValues, Exception, Str, iterators::DictKeysIter},
        utils::{BoundArgs, HashKey, Parameter, Signature},
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Dict {
    items: HashMap<HashKey, (TreewalkValue, TreewalkValue)>,
    order: Vec<HashKey>,
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
        GetItemBuiltin,
        SetItemBuiltin,
        DelItemBuiltin,
    ]
);

impl Dict {
    pub fn from_symbol_table(table: &SymbolTable) -> Self {
        let items = table
            .iter()
            .map(|(key, value)| (TreewalkValue::Str(Str::new(key)), value.clone()))
            .collect();

        Dict::from_items(items).expect("All keys should be hashable strings here.")
    }

    pub fn from_items(items: Vec<(TreewalkValue, TreewalkValue)>) -> DomainResult<Self> {
        let mut dict = Dict::default();
        for (k, v) in items {
            dict.insert(k, v)?;
        }
        Ok(dict)
    }

    pub fn insert(&mut self, key: TreewalkValue, value: TreewalkValue) -> DomainResult<()> {
        let hash = key.as_hash_key()?;
        if !self.items.contains_key(&hash) {
            self.order.push(hash.clone());
        }
        self.items.insert(hash, (key, value));
        Ok(())
    }

    pub fn delete(&mut self, key: &TreewalkValue) -> DomainResult<()> {
        let hash = key.as_hash_key()?;
        self.items.remove(&hash);
        self.order.retain(|item| item != &hash);
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

    pub fn getitem(&self, index: &TreewalkValue) -> DomainResult<TreewalkValue> {
        self.get(index).ok_or_else(|| Exception::key_error(index))
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
        for key in &self.order {
            let (k, v) = self.items.get(key).unwrap();
            items.push((k.clone(), v.clone()));
        }
        DictItems::new(items)
    }

    pub fn keys(&self) -> DictKeys {
        let mut keys = Vec::with_capacity(self.items.len());
        for key in &self.order {
            let (k, _) = self.items.get(key).unwrap();
            keys.push(k.clone());
        }
        DictKeys::new(keys)
    }

    pub fn values(&self) -> DictValues {
        let mut values = Vec::with_capacity(self.items.len());
        for key in &self.order {
            let (_, v) = self.items.get(key).unwrap();
            values.push(v.clone());
        }
        DictValues::new(values)
    }

    /// Turn this `Dict` into a `SymbolTable`, which is another key-value store but where the keys
    /// are all confirmed to be valid Python identifiers.
    pub fn to_symbol_table(&self) -> DomainResult<SymbolTable> {
        let mut table = SymbolTable::default();

        let dict_items = self.items();
        for pair in dict_items {
            let tuple = pair.as_tuple()?;
            let key = tuple.first().as_string()?;
            let value = tuple.second();
            table.insert(&key, value);
        }

        Ok(table)
    }

    pub fn extend(&mut self, other: &Dict) -> DomainResult<()> {
        for key in &other.order {
            let (k, v) = other.items.get(key).unwrap();
            self.insert(k.clone(), v.clone())?;
        }
        Ok(())
    }

    pub fn equals(&self, other: &Dict) -> bool {
        // Insertion order does not affect dict equality
        self.items == other.items
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
                let items = build_dict_items_from_iterable(iter, interpreter)?;
                let dict = Dict::from_items(items).raise(interpreter)?;
                Ok(Container::new(dict))
            }
            _ => Exception::type_error("Expected a dict").raise(interpreter),
        }
    }
}

fn build_dict_items_from_iterable(
    iter: Box<dyn CloneableIterable>,
    interpreter: &TreewalkInterpreter,
) -> TreewalkResult<Vec<(TreewalkValue, TreewalkValue)>> {
    let mut pairs: Vec<(TreewalkValue, TreewalkValue)> = vec![];
    let mut index = 0;
    for_each_mut(iter, &mut |item| {
        // The item is often a tuple, but can really be any iterable which yields 2 values.
        let inner_iter = item.as_iterator().raise(interpreter)?;
        let pair = collect(inner_iter)?;

        // We cannot convert directly from a Vec to a tuple, we must first attempt to convert
        // to an array of a known and fixed length of 2.
        let pair_arr: [TreewalkValue; 2] = pair
            .clone()
            .try_into()
            .map_err(|_| {
                Exception::value_error(format!(
                    "dictionary update sequence element #{} has length {}; 2 is required",
                    index,
                    pair.len()
                ))
            })
            .raise(interpreter)?;

        pairs.push(pair_arr.into());
        index += 1;
        Ok(())
    })?;

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
#[derive(Clone)]
struct GetItemBuiltin;
#[derive(Clone)]
struct SetItemBuiltin;
#[derive(Clone)]
struct DelItemBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional_without_default("iterable").positional_only(),
        ])
        .with_varkwargs("kwargs")
    }

    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Dict(Container::new(Dict::default())))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for InitBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("self").positional_only(),
            Parameter::optional_without_default("iterable").positional_only(),
        ])
        .with_varkwargs("kwargs")
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let output = args.get("self").as_dict().raise(interpreter)?;

        if let Some(iterable) = args.get_optional("iterable") {
            let input = Container::<Dict>::try_eval_from(iterable.clone(), interpreter)?;
            output
                .borrow_mut()
                .extend(&input.borrow())
                .raise(interpreter)?;
        }

        let kwargs = args.get_varkwargs("kwargs");
        output
            .borrow_mut()
            .extend(&kwargs.borrow())
            .raise(interpreter)?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

impl Callable for DictItemsBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let dict = args.get("self").as_dict().raise(interpreter)?;
        let dict_items = dict.borrow().items();
        Ok(TreewalkValue::DictItems(dict_items))
    }

    fn name(&self) -> String {
        "items".into()
    }
}

impl Callable for DictKeysBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let dict = args.get("self").as_dict().raise(interpreter)?;
        let dict_keys = dict.borrow().keys();
        Ok(TreewalkValue::DictKeys(dict_keys))
    }

    fn name(&self) -> String {
        "keys".into()
    }
}

impl Callable for DictValuesBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let dict = args.get("self").as_dict().raise(interpreter)?;
        let dict_values = dict.borrow().values();
        Ok(TreewalkValue::DictValues(dict_values))
    }

    fn name(&self) -> String {
        "values".into()
    }
}

impl Callable for GetBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("self").positional_only(),
            Parameter::required("key").positional_only(),
            Parameter::optional("default", TreewalkValue::None).positional_only(),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let dict = args.get("self").as_dict().raise(interpreter)?;
        let key = args.get("key");

        let value = if let Some(val) = dict.borrow().get(key) {
            val
        } else {
            args.get("default").clone()
        };

        Ok(value)
    }

    fn name(&self) -> String {
        "get".into()
    }
}

impl Callable for GetItemBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "key"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("self").as_dict().raise(interpreter)?;
        let index = args.get("key");
        let value = object.borrow().getitem(index).raise(interpreter)?;
        Ok(value)
    }

    fn name(&self) -> String {
        Dunder::GetItem.into()
    }
}

impl Callable for SetItemBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "key", "value"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("self").as_dict().raise(interpreter)?;
        let index = args.get("key").clone();
        let value = args.get("value").clone();
        object
            .borrow_mut()
            .insert(index, value)
            .raise(interpreter)?;
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::SetItem.into()
    }
}

impl Callable for DelItemBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "key"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("self").as_dict().raise(interpreter)?;
        let key = args.get("key");
        object.borrow_mut().delete(key).raise(interpreter)?;
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::DelItem.into()
    }
}
