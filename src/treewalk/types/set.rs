use std::collections::HashMap;

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        utils::{check_args, Args, HashKey},
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, PartialEq, Clone)]
pub struct Set {
    items: HashMap<HashKey, TreewalkValue>,
}

impl_typed!(Set, Type::Set);
impl_method_provider!(Set, [AddBuiltin, LeBuiltin, NewBuiltin]);
impl_iterable!(SetIter);

impl Set {
    pub fn from_items(items: Vec<TreewalkValue>) -> DomainResult<Self> {
        let mut set = Set::default();
        for item in items {
            set.add(item)?;
        }
        Ok(set)
    }

    pub fn add(&mut self, item: TreewalkValue) -> DomainResult<bool> {
        let key = item.as_hash_key()?;
        Ok(self.items.insert(key, item).is_none())
    }

    pub fn subset(&self, other: &Set) -> bool {
        self.items.keys().all(|k| other.items.contains_key(k))
    }
}

impl TryEvalFrom for Set {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let items: Vec<_> = value.as_iterator().raise(interpreter)?.collect();
        let set = Set::from_items(items).raise(interpreter)?;
        Ok(set)
    }
}

impl IntoIterator for Container<Set> {
    type Item = TreewalkValue;
    type IntoIter = SetIter;

    fn into_iter(self) -> Self::IntoIter {
        let items: Vec<TreewalkValue> = self.borrow().items.values().cloned().collect();
        SetIter::new(items)
    }
}

#[derive(Clone)]
pub struct SetIter {
    items: Vec<TreewalkValue>,
    current_index: usize,
}

impl SetIter {
    pub fn new(list_ref: Vec<TreewalkValue>) -> Self {
        Self {
            items: list_ref,
            current_index: 0,
        }
    }
}

impl Iterator for SetIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.items.len() {
            None
        } else {
            self.current_index += 1;
            self.items.get(self.current_index - 1).cloned()
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct LeBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len)).raise(interpreter)?;

        let set = match args.len() {
            1 => Set::default(),
            2 => Set::try_eval_from(args.get_arg(1), interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Set(Container::new(set)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let set = args
            .get_self()
            .raise(interpreter)?
            .as_set()
            .raise(interpreter)?;
        let result = set.borrow_mut().add(args.get_arg(0)).raise(interpreter)?;

        Ok(TreewalkValue::Bool(result))
    }

    fn name(&self) -> String {
        "add".into()
    }
}

impl Callable for LeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let left_set = args
            .get_self()
            .raise(interpreter)?
            .as_set()
            .raise(interpreter)?;
        let right_set = args.get_arg(0).as_set().raise(interpreter)?;
        let l = left_set.borrow().clone();
        let r = right_set.borrow().clone();

        Ok(TreewalkValue::Bool(l.subset(&r)))
    }

    fn name(&self) -> String {
        Dunder::Le.into()
    }
}
