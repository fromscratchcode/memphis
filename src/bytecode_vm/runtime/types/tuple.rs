use crate::{
    bytecode_vm::{
        runtime::{types::Exception, Reference},
        VirtualMachine, VmResult, VmValue,
    },
    domain::utils::normalize_index,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    pub items: Vec<Reference>,
}

impl Tuple {
    pub fn new(items: Vec<Reference>) -> Self {
        Self { items }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(self) -> TupleIter {
        TupleIter {
            inner: self.items.into_iter(),
        }
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
                    let msg = vm.intern_string("tuple index out of range");
                    let exp = Exception::index_error(msg);
                    return Err(vm.raise(exp));
                }
            }
            _ => {
                let msg = vm.intern_string("tuple indices must be integers or slices, not TODO");
                let exp = Exception::type_error(msg);
                return Err(vm.raise(exp));
            }
        };

        Ok(value)
    }
}

#[derive(Clone, Debug)]
pub struct TupleIter {
    inner: std::vec::IntoIter<Reference>,
}

impl Iterator for TupleIter {
    type Item = Reference;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
