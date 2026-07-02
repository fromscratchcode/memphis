use crate::{
    bytecode_vm::{
        runtime::{types::Exception, Reference},
        VirtualMachine, VmResult, VmValue,
    },
    core::Container,
    domain::utils::normalize_index,
};

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    items: Vec<Reference>,
}

impl List {
    pub fn new(items: Vec<Reference>) -> Self {
        Self { items }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, index: usize) -> Option<Reference> {
        self.items.get(index).cloned()
    }

    pub fn set(&mut self, index: usize, value: Reference) {
        self.items[index] = value;
    }

    fn get_normalized(&self, index: i64) -> Option<Reference> {
        normalize_index(index, self.len()).map(|idx| self.items[idx])
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
                let type_name = vm.type_name(&index);
                let msg = vm.intern_string(&format!(
                    "list indices must be integers or slices, not {}",
                    type_name
                ));
                let exp = Exception::type_error(msg);
                return Err(vm.raise(exp));
            }
        };

        Ok(value)
    }

    pub fn setitem(
        &mut self,
        vm: &mut VirtualMachine,
        index: Reference,
        value: Reference,
    ) -> VmResult<()> {
        let index = vm.deref(index);
        match index {
            VmValue::Int(i) => {
                if let Some(i) = normalize_index(i, self.len()) {
                    self.set(i, value);
                } else {
                    let msg = vm.intern_string("list assignment index out of range");
                    let exp = Exception::index_error(msg);
                    return Err(vm.raise(exp));
                }
            }
            _ => {
                let type_name = vm.type_name(&index);
                let msg = vm.intern_string(&format!(
                    "list indices must be integers or slices, not {}",
                    type_name
                ));
                let exp = Exception::type_error(msg);
                return Err(vm.raise(exp));
            }
        }
        Ok(())
    }
}

impl IntoIterator for Container<List> {
    type Item = Reference;
    type IntoIter = ListIter;

    fn into_iter(self) -> Self::IntoIter {
        ListIter::new(self)
    }
}

#[derive(Debug, Clone)]
pub struct ListIter {
    list_ref: Container<List>,
    current_index: usize,
}

impl ListIter {
    pub fn new(list_ref: Container<List>) -> Self {
        Self {
            list_ref,
            current_index: 0,
        }
    }
}

impl Iterator for ListIter {
    type Item = Reference;

    fn next(&mut self) -> Option<Self::Item> {
        let list = self.list_ref.borrow();
        if self.current_index == list.len() {
            None
        } else {
            self.current_index += 1;
            list.get(self.current_index - 1)
        }
    }
}
