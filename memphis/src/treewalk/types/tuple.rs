use crate::{
    core::Container,
    domain::{Dunder, Type, utils::normalize_index},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        iterator::collect,
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        types::{Exception, Slice},
        utils::{BoundArgs, Parameter, Signature},
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Tuple {
    items: Vec<TreewalkValue>,
}

impl_typed!(Tuple, Type::Tuple);
impl_method_provider!(Tuple, [NewBuiltin, GetItemBuiltin]);
impl_iterable!(TupleIter);

impl Tuple {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[TreewalkValue] {
        &self.items
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    fn get(&self, index: usize) -> Option<TreewalkValue> {
        self.items.get(index).cloned()
    }

    fn get_normalized(&self, index: i64) -> Option<TreewalkValue> {
        normalize_index(index, self.len()).map(|idx| self.items[idx].clone())
    }

    pub fn first(&self) -> TreewalkValue {
        self.get(0).expect("No first tuple element!")
    }

    pub fn second(&self) -> TreewalkValue {
        self.get(1).expect("No second tuple element!")
    }

    fn slice(&self, slice: &Slice) -> Self {
        let sliced_items = slice.apply(self.len(), |i| self.get(i as usize));
        Self::new(sliced_items)
    }
}

impl TryEvalFrom for Tuple {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let iter = value.as_iterator().raise(interpreter)?;
        let items = collect(iter)?;
        Ok(Tuple::new(items))
    }
}

impl IntoIterator for Tuple {
    type Item = TreewalkValue;
    type IntoIter = TupleIter;

    fn into_iter(self) -> Self::IntoIter {
        TupleIter::new(self)
    }
}

#[derive(Clone)]
pub struct TupleIter {
    list_ref: Tuple,
    current_index: Container<usize>,
}

impl TupleIter {
    pub fn new(list_ref: Tuple) -> Self {
        Self {
            list_ref,
            current_index: Container::new(0),
        }
    }
}

impl Iterator for TupleIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if *self.current_index.borrow() == self.list_ref.len() {
            None
        } else {
            *self.current_index.borrow_mut() += 1;
            self.list_ref
                .items
                .get(*self.current_index.borrow() - 1)
                .cloned()
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct GetItemBuiltin;

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
        let tuple = Tuple::try_eval_from(args.get("iterable").clone(), interpreter)?;
        Ok(TreewalkValue::Tuple(tuple))
    }

    fn name(&self) -> String {
        Dunder::New.into()
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
        let object = args.get("self").as_tuple().raise(interpreter)?;
        let index = args.get("key");

        let value = match index {
            TreewalkValue::Int(i) => object
                .get_normalized(*i)
                .ok_or_else(|| Exception::index_error("tuple index out of range"))
                .raise(interpreter)?,
            TreewalkValue::Slice(s) => TreewalkValue::Tuple(object.slice(s)),
            _ => {
                return Exception::type_error(format!(
                    "tuple indices must be integers or slices, not {}",
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
