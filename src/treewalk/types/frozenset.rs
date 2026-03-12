use std::collections::HashMap;

use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        types::iterators::SetIter,
        utils::{check_args, Args, HashKey},
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
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
        let items: Vec<_> = value.as_iterator().raise(interpreter)?.collect();
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
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len)).raise(interpreter)?;

        let frozen_set = match args.len() {
            1 => FrozenSet::default(),
            2 => FrozenSet::try_eval_from(args.get_arg(1), interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::FrozenSet(frozen_set))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for ContainsBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!();
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}
