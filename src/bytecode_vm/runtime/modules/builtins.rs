use crate::{
    bytecode_vm::{
        result::Raise,
        runtime::{
            runtime::register_builtin_funcs,
            types::{Class, Exception, FunctionObject, List, Module, Range, Tuple},
            BuiltinFn, Frame, Reference,
        },
        Runtime, VirtualMachine, VmResult, VmValue,
    },
    core::Container,
    domain::{Dunder, ModuleName},
};

static BUILTINS: [(&str, BuiltinFn); 8] = [
    ("bool", bool),
    ("int", int),
    ("list", list),
    ("tuple", tuple),
    ("range", range),
    ("print", print),
    ("iter", iter),
    ("next", next),
];

pub fn init_module(runtime: &mut Runtime) {
    let mut mod_ = Module::new(ModuleName::from_segments(&[Dunder::Builtins]));
    register_builtin_funcs(runtime, &mut mod_, &BUILTINS);
    runtime.store_module(Container::new(mod_));
}

/// This is intended to be functionally equivalent to `__build_class__` in CPython.
pub fn build_class(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let code_value = vm.deref(args[0]);
    let code = code_value
        .as_code()
        .ok_or_else(Exception::runtime_error)
        .raise(vm)?;
    let name = code.name().to_string();

    let function = FunctionObject::new(code.clone());
    let module = vm
        .read_module(&function.code_object.module_name)
        .raise(vm)?;
    let frame = Frame::new(function, vec![], module);

    let frame = vm.call_and_return_frame(frame);
    Ok(vm.heapify(VmValue::Class(Class::new(name, frame.namespace()))))
}

/// Given a reference to an object, build a collection over its iterator.
fn collect_iterable(vm: &mut VirtualMachine, obj_ref: Reference) -> VmResult<Vec<Reference>> {
    let obj = vm.deref(obj_ref);
    let iter_ref = iter_internal(vm, obj)?;

    let mut collected = vec![];
    while let Some(item_ref) = next_internal(vm, iter_ref)? {
        collected.push(item_ref);
    }

    Ok(collected)
}

fn list(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let items = match args.len() {
        0 => vec![],
        1 => collect_iterable(vm, args[0])?,
        _ => {
            let msg = VmValue::String(format!(
                "list expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    Ok(vm.heapify(VmValue::List(List::new(items))))
}

fn tuple(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let items = match args.len() {
        0 => vec![],
        1 => collect_iterable(vm, args[0])?,
        _ => {
            let msg = VmValue::String(format!(
                "tuple expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    Ok(vm.heapify(VmValue::Tuple(Tuple::new(items))))
}

fn bool(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let value = match args.len() {
        0 => false,
        1 => vm.deref(args[0]).to_boolean(),
        _ => {
            let msg = VmValue::String(format!(
                "bool expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    Ok(vm.to_heapified_bool(value))
}

fn int(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let value = match args.len() {
        0 => 0,
        1 => vm.coerce_to_int(&vm.deref(args[0])).raise(vm)?,
        _ => {
            let msg = VmValue::String(format!(
                "int expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    Ok(vm.heapify(VmValue::Int(value)))
}

fn range(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let range = match args.len() {
        1 => {
            let stop = expect_integer_or_raise(vm, &vm.deref(args[0]))?;
            Range::with_stop(stop)
        }
        2 => {
            let start = expect_integer_or_raise(vm, &vm.deref(args[0]))?;
            let stop = expect_integer_or_raise(vm, &vm.deref(args[1]))?;
            Range::with_start_stop(start, stop)
        }
        3 => {
            let start = expect_integer_or_raise(vm, &vm.deref(args[0]))?;
            let stop = expect_integer_or_raise(vm, &vm.deref(args[1]))?;
            let step = expect_integer_or_raise(vm, &vm.deref(args[2]))?;
            Range::new(start, stop, step)
        }
        0 => {
            let msg = VmValue::String(format!(
                "range expected at least 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
        _ => {
            let msg = VmValue::String(format!(
                "range expected at most 3 arguments, got {}",
                args.len()
            ));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    Ok(vm.heapify(VmValue::Range(range)))
}

fn expect_integer_or_raise(vm: &mut VirtualMachine, value: &VmValue) -> VmResult<i64> {
    match value.as_integer() {
        Some(i) => Ok(i),
        None => {
            let msg = VmValue::String("Expected an integer".to_string());
            Exception::type_error(vm.heapify(msg)).raise(vm)
        }
    }
}

/// Internal method used by GET_ITER
/// For the public-facing builtin `iter(obj)`, use `iter`.
pub fn iter_internal(vm: &mut VirtualMachine, obj: VmValue) -> VmResult<Reference> {
    let iterator = match obj {
        VmValue::Generator(_) => obj,
        VmValue::List(list) => VmValue::ListIter(Container::new(list.iter())),
        VmValue::Tuple(tuple) => VmValue::TupleIter(Container::new(tuple.iter())),
        VmValue::Range(range) => VmValue::RangeIter(Container::new(range.iter())),
        _ => {
            let msg = VmValue::String(format!("'{}' object is not iterable", obj.get_type()));
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    Ok(vm.heapify(iterator))
}

fn iter(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let iterable_ref = match args.len() {
        1 => args[0],
        _ => {
            let msg = VmValue::String("iter expected exactly 1 argument".to_string());
            return Exception::type_error(vm.heapify(msg)).raise(vm);
        }
    };

    let iterable_value = vm.deref(iterable_ref);
    iter_internal(vm, iterable_value)
}

/// Internal method used by FOR_ITER
/// For the public-facing builtin `next(it)`, we must return a StopIterator error to the user.
pub fn next_internal(vm: &mut VirtualMachine, iter_ref: Reference) -> VmResult<Option<Reference>> {
    let iter_value = vm.deref(iter_ref);
    match iter_value {
        VmValue::Generator(ref generator) => Ok(vm.resume_generator(generator.clone())),
        VmValue::ListIter(ref list_iter) => Ok(list_iter.borrow_mut().next()),
        VmValue::TupleIter(ref list_iter) => Ok(list_iter.borrow_mut().next()),
        VmValue::RangeIter(ref range_iter) => Ok(range_iter
            .borrow_mut()
            .next()
            .map(|i| vm.heapify(VmValue::Int(i)))),
        _ => {
            let msg = VmValue::String(format!(
                "'{}' object is not an iterator",
                iter_value.get_type()
            ));
            Exception::type_error(vm.heapify(msg)).raise(vm)
        }
    }
}

fn next(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    if args.len() != 1 {
        let msg = VmValue::String(format!("next() expected 1 argument, got {}", args.len()));
        return Exception::type_error(vm.heapify(msg)).raise(vm);
    }

    match next_internal(vm, args[0])? {
        Some(val) => Ok(val),
        None => Exception::stop_iteration().raise(vm),
    }
}

fn print(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let rendered: Vec<String> = args
        .iter()
        .map(|arg| vm.normalize_vm_ref(*arg).to_string())
        .collect();

    println!("{}", rendered.join(" "));

    Ok(vm.none())
}

#[cfg(test)]
mod tests {
    use crate::bytecode_vm::runtime::runtime::register_builtin_funcs;

    use super::*;

    #[test]
    fn register_builtins_inserts_list() {
        let mut runtime = Runtime::default();
        let mut module = Module::new(ModuleName::from_segments(&["test_module"]));
        register_builtin_funcs(&mut runtime, &mut module, &BUILTINS);
        assert!(module.global_store().contains_key("list"));
        assert!(!module.global_store().contains_key("dict"));
    }
}
