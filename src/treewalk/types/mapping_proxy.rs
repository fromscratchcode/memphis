use crate::{
    core::Container,
    treewalk::{
        protocols::IndexRead,
        types::{Dict, DictItems},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// A read-only view into a `Dict`. This is used by Python for things like `Dunder::Dict`.
#[derive(Clone, PartialEq)]
pub struct MappingProxy(Container<Dict>);

impl MappingProxy {
    pub fn new(dict: Container<Dict>) -> Self {
        Self(dict)
    }

    pub fn to_items(&self) -> DictItems {
        self.0.borrow().items()
    }
}

impl IndexRead for MappingProxy {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        self.0.getitem(interpreter, index)
    }
}
