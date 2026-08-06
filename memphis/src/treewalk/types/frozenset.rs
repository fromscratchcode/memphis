use std::collections::HashMap;

use crate::{
    domain::{Dunder, Type},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        iterator::collect,
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        types::{Tuple, iterators::SetIter},
        utils::{BoundArgs, HashKey, Parameter, Signature},
    },
};

#[derive(Default, PartialEq, Clone)]
pub struct FrozenSet {
    items: HashMap<HashKey, TreewalkValue>,
}

impl_typed!(FrozenSet, Type::FrozenSet);
impl_method_provider!(FrozenSet, [NewBuiltin, ContainsBuiltin]);

impl FrozenSet {
    pub fn from_items(items: Vec<TreewalkValue>) -> DomainResult<Self> {
        let mut set = FrozenSet::default();
        for item in items {
            set.add(item)?;
        }
        Ok(set)
    }

    fn add(&mut self, item: TreewalkValue) -> DomainResult<bool> {
        let key = item.as_hash_key()?;
        Ok(self.items.insert(key, item).is_none())
    }
}

impl TryEvalFrom for FrozenSet {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let iter = value.as_iterator().raise(interpreter)?;
        let items = collect(iter)?;
        let set = FrozenSet::from_items(items).raise(interpreter)?;
        Ok(set)
    }
}

impl IntoIterator for FrozenSet {
    type Item = TreewalkValue;
    type IntoIter = SetIter;

    fn into_iter(self) -> Self::IntoIter {
        let items: Vec<TreewalkValue> = self.items.values().cloned().collect();
        SetIter::new(items)
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct ContainsBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional("iterable", TreewalkValue::Tuple(Tuple::default()))
                .positional_only(),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let frozen_set = FrozenSet::try_eval_from(args.get("iterable").clone(), interpreter)?;
        Ok(TreewalkValue::FrozenSet(frozen_set))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for ContainsBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["self", "object"])
    }

    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!();
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}
