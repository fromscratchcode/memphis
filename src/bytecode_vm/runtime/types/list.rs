use crate::{
    bytecode_vm::{
        runtime::{types::Exception, Reference},
        VirtualMachine, VmResult, VmValue,
    },
    domain::utils::normalize_index,
};

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    // TODO this is currently public because we need it in some tests to call deference on the
    // elements. We'll eventually make this a slice accessor or an iterator or something.
    pub items: Vec<Reference>,
}

impl List {
    pub fn new(items: Vec<Reference>) -> Self {
        Self { items }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(self) -> ListIter {
        ListIter {
            inner: self.items.into_iter(),
        }
    }

    pub fn _get(&self, index: usize) -> Option<Reference> {
        self.items.get(index).cloned()
    }

    fn get_normalized(&self, index: i64) -> Option<Reference> {
        let len = self.items.len() as i64;
        normalize_index(index, len).map(|idx| self.items[idx])
    }

    pub fn getitem(&self, vm: &mut VirtualMachine, index: VmValue) -> VmResult<Reference> {
        let value = match index {
            VmValue::Int(i) => {
                if let Some(val) = self.get_normalized(i) {
                    val
                } else {
                    let msg = vm.intern_string("list index out of range");
                    let exp = Exception::index_error(msg);
                    return Err(vm.raise(exp));
                }
            }
            _ => {
                let msg = vm.intern_string("list indices must be integers or slices, not TODO");
                let exp = Exception::type_error(msg);
                return Err(vm.raise(exp));
            }
        };

        Ok(value)
    }

    #[cfg(test)]
    pub fn resolved_items(&self, vm: &VirtualMachine) -> Vec<VmValue> {
        self.items.iter().map(|r| vm.deref(*r)).collect()
    }
}

#[derive(Clone, Debug)]
pub struct ListIter {
    inner: std::vec::IntoIter<Reference>,
}

impl Iterator for ListIter {
    type Item = Reference;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
