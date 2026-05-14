use std::{collections::VecDeque, ops::Add};

use crate::{
    core::Container,
    domain::{utils::normalize_index, Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        type_system::CloneableIterable,
        types::{Exception, Slice},
        utils::{check_args, Args},
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct List {
    items: Vec<TreewalkValue>,
}

impl_typed!(List, Type::List);
impl_method_provider!(
    List,
    [
        NewBuiltin,
        AddBuiltin,
        AppendBuiltin,
        ExtendBuiltin,
        GetItemBuiltin,
        SetItemBuiltin,
        DelItemBuiltin,
    ]
);
impl_iterable!(ListIter);

impl List {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn append(&mut self, item: TreewalkValue) {
        self.items.push(item)
    }

    pub fn join(&self, delim: &str) -> DomainResult<String> {
        Ok(self
            .items
            .iter()
            .map(|v| v.as_string())
            .collect::<DomainResult<Vec<_>>>()?
            .join(delim))
    }

    pub fn extend(&mut self, items: Box<dyn CloneableIterable>) {
        self.items.extend(items)
    }

    /// Use this when you need a `pop_front` method.
    pub fn as_queue(&self) -> VecDeque<TreewalkValue> {
        self.items.clone().into()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<TreewalkValue> {
        self.items.get(index).cloned()
    }

    pub fn set(&mut self, index: usize, value: TreewalkValue) {
        self.items[index] = value;
    }

    fn get_normalized(&self, index: i64) -> Option<TreewalkValue> {
        normalize_index(index, self.len()).and_then(|idx| self.get(idx).clone())
    }

    fn slice(&self, slice: &Slice) -> Self {
        let sliced_items = slice.apply(self.len(), |i| self.get(i as usize));
        List::new(sliced_items)
    }
}

impl Add for List {
    type Output = List;

    fn add(self, other: List) -> List {
        List {
            items: [self.items, other.items].concat(),
        }
    }
}

impl TryEvalFrom for List {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let iter = value.as_iterator().raise(interpreter)?;
        Ok(List::new(iter.collect()))
    }
}

impl IntoIterator for Container<List> {
    type Item = TreewalkValue;
    type IntoIter = ListIter;

    fn into_iter(self) -> Self::IntoIter {
        ListIter::new(self)
    }
}

#[derive(Clone)]
pub struct ListIter {
    list_ref: Container<List>,
    current_index: Container<usize>,
}

impl ListIter {
    pub fn new(list_ref: Container<List>) -> Self {
        Self {
            list_ref,
            current_index: Container::new(0),
        }
    }
}

impl Iterator for ListIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if *self.current_index.borrow() == self.list_ref.borrow().len() {
            None
        } else {
            *self.current_index.borrow_mut() += 1;
            self.list_ref
                .borrow()
                .items
                .get(*self.current_index.borrow() - 1)
                .cloned()
        }
    }
}

impl ExactSizeIterator for ListIter {
    fn len(&self) -> usize {
        self.list_ref.borrow().len() - *self.current_index.borrow()
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct AppendBuiltin;
#[derive(Clone)]
struct ExtendBuiltin;
#[derive(Clone)]
struct GetItemBuiltin;
#[derive(Clone)]
struct SetItemBuiltin;
#[derive(Clone)]
struct DelItemBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len)).raise(interpreter)?;

        let list = match args.len() {
            1 => List::default(),
            2 => List::try_eval_from(args.get_arg(1), interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::List(Container::new(list)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let left_list = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        let right_list = args.get_arg(0).as_list().raise(interpreter)?;
        let l = left_list.borrow().clone();
        let r = right_list.borrow().clone();

        Ok(TreewalkValue::List(Container::new(l + r)))
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for AppendBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let list = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        list.borrow_mut().append(args.get_arg(0).clone());

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "append".into()
    }
}

impl Callable for ExtendBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let list = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        list.borrow_mut().extend(
            args.get_arg(0)
                .as_iterable()
                .raise(interpreter)?
                .into_iter(),
        );

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "extend".into()
    }
}

impl Callable for GetItemBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let object = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        let index = args.get_arg(0);

        let value = match index {
            TreewalkValue::Int(i) => object
                .borrow()
                .get_normalized(i)
                .ok_or_else(|| Exception::index_error("list index out of range"))
                .raise(interpreter)?,
            TreewalkValue::Slice(s) => {
                TreewalkValue::List(Container::new(object.borrow().slice(&s)))
            }
            _ => {
                return Exception::type_error(format!(
                    "list indices must be integers or slices, not {}",
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

impl Callable for SetItemBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2).raise(interpreter)?;

        let object = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        let index = args.get_arg(0);
        let value = args.get_arg(1);

        match index {
            TreewalkValue::Int(i) => {
                let i = normalize_index(i, object.borrow().len())
                    .ok_or_else(|| Exception::index_error("list assignment index out of range"))
                    .raise(interpreter)?;
                object.borrow_mut().set(i, value);
            }
            _ => {
                return Exception::type_error(format!(
                    "list indices must be integers or slices, not {}",
                    interpreter.state.type_name(&index)
                ))
                .raise(interpreter)
            }
        }

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::SetItem.into()
    }
}

impl Callable for DelItemBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let object = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        let index = args.get_arg(0);

        // TODO what if this is a slice
        let i = index.as_int().raise(interpreter)?;
        object.borrow_mut().items.remove(i as usize);
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::DelItem.into()
    }
}
