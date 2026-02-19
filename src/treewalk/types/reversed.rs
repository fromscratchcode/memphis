use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        types::List,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct ReversedIter {
    list_ref: Container<List>,
    current_index: usize,
}

impl_typed!(ReversedIter, Type::ReversedIter);
impl_method_provider!(ReversedIter, [NewBuiltin]);
impl_iterable!(ReversedIter);

impl ReversedIter {
    pub fn new(list_ref: Container<List>) -> Self {
        let current_index = list_ref.borrow().len();
        Self {
            list_ref,
            current_index,
        }
    }
}

impl Iterator for ReversedIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == 0 {
            None
        } else {
            self.current_index -= 1;
            self.list_ref.borrow().get(self.current_index)
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2).raise(interpreter)?;
        let list = args.get_arg(1).as_list().raise(interpreter)?;
        Ok(TreewalkValue::ReversedIter(ReversedIter::new(list.clone())))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
