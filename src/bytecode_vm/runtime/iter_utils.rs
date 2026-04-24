use crate::{
    bytecode_vm::{
        result::Raise,
        runtime::{types::Exception, Reference, VirtualMachine},
        VmResult, VmValue,
    },
    core::Container,
};

pub enum NextResult {
    Yielded(Reference),
    Exhausted(Option<Reference>),
}

/// Internal method used by GET_ITER.
/// For the public-facing builtin `iter(obj)`, use the builtin.
pub fn iter_internal(vm: &mut VirtualMachine, obj_ref: Reference) -> VmResult<Reference> {
    let value = vm.deref(obj_ref);
    let iterator_ref = match value {
        VmValue::Generator(_) => obj_ref,
        VmValue::List(list) => {
            let type_ = vm.runtime.borrow().builtin_types.list_iter;
            vm.new_object(type_, VmValue::ListIter(Container::new(list.into_iter())))
        }
        VmValue::Tuple(tuple) => {
            let type_ = vm.runtime.borrow().builtin_types.tuple_iter;
            vm.new_object(type_, VmValue::TupleIter(Container::new(tuple.iter())))
        }
        VmValue::Range(range) => {
            let type_ = vm.runtime.borrow().builtin_types.range_iter;
            vm.new_object(type_, VmValue::RangeIter(Container::new(range.iter())))
        }
        _ => {
            let msg = vm.intern_string(&format!("'{}' object is not iterable", value.get_type()));
            return Exception::type_error(msg).raise(vm);
        }
    };

    Ok(iterator_ref)
}

/// Internal method used by FOR_ITER and YIELD_FROM.
pub fn next_internal(vm: &mut VirtualMachine, iter_ref: Reference) -> VmResult<NextResult> {
    let iter_value = vm.deref(iter_ref);
    match iter_value {
        VmValue::Generator(ref generator) => Ok(vm.resume_generator(generator.clone())),
        VmValue::ListIter(ref list_iter) => Ok(match list_iter.borrow_mut().next() {
            Some(value) => NextResult::Yielded(value),
            None => NextResult::Exhausted(None),
        }),
        VmValue::TupleIter(ref list_iter) => Ok(match list_iter.borrow_mut().next() {
            Some(value) => NextResult::Yielded(value),
            None => NextResult::Exhausted(None),
        }),
        VmValue::RangeIter(ref range_iter) => Ok(match range_iter.borrow_mut().next() {
            Some(i) => {
                // TODO it doesn't feel like this should be necessary, what if the range iter
                // returned full objects here.
                let type_ = vm.runtime.borrow().builtin_types.int;
                let value = vm.new_object(type_, VmValue::Int(i));
                NextResult::Yielded(value)
            }
            None => NextResult::Exhausted(None),
        }),
        _ => {
            let msg = vm.intern_string(&format!(
                "'{}' object is not an iterator",
                iter_value.get_type()
            ));
            Exception::type_error(msg).raise(vm)
        }
    }
}
