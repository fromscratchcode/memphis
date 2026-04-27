use crate::{
    bytecode_vm::{
        result::Raise,
        runtime::{
            iter_internal, next_internal,
            runtime::register_builtin_funcs,
            types::{Class, Exception, List, Module, Range, Tuple},
            BuiltinFn, NextResult, Reference,
        },
        Runtime, VirtualMachine, VmResult, VmValue,
    },
    core::Container,
    domain::{Dunder, ModuleName, Type},
};

static BUILTINS: [(&str, BuiltinFn); 9] = [
    ("type", type_fn),
    ("bool", bool),
    ("int", int),
    ("list", list),
    ("tuple", tuple),
    ("range", range),
    ("print", print),
    ("iter", iter),
    ("next", next),
];

fn register_builtin_types(runtime: &Runtime, module: &mut Module) {
    for type_ in Type::all()
        .iter()
        .filter(|t| t.exported_in_builtins())
        // TODO this is a hack, we can delete this once we stop treating these as builtin fns
        .filter(|t| {
            !matches!(
                t,
                Type::Bool | Type::Int | Type::List | Type::Tuple | Type::Range | Type::Type
            )
        })
    {
        let class_ref = runtime.builtin_types.get(type_);
        module.write(&type_.to_string(), class_ref);
    }
}

pub fn init_module(runtime: &mut Runtime) -> Module {
    let mut module = Module::new(ModuleName::from_segments(&[Dunder::Builtins]));

    register_builtin_funcs(runtime, &mut module, &BUILTINS);
    register_builtin_types(runtime, &mut module);

    module
}

/// This is intended to be functionally equivalent to `__build_class__` in CPython.
pub fn build_class(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let code_value = vm.deref(args[0]);
    let code = code_value
        .as_code()
        .ok_or_else(|| {
            let msg = vm.intern_string("Expected code");
            Exception::runtime_error(msg)
        })
        .raise(vm)?;
    let name = code.name().to_string();

    let frame = vm.frame_for_code(code.clone());
    let frame = vm.call_and_return_frame(frame);

    let type_ = vm.runtime.borrow().builtin_types.r#type;
    let obj = vm.new_object(type_, VmValue::Class(Class::new(name, frame.namespace())));
    Ok(obj)
}

/// Given a reference to an object, build a collection over its iterator.
fn collect_iterable(vm: &mut VirtualMachine, obj_ref: Reference) -> VmResult<Vec<Reference>> {
    let iter_ref = iter_internal(vm, obj_ref)?;

    let mut collected = vec![];
    while let NextResult::Yielded(item_ref) = next_internal(vm, iter_ref)? {
        collected.push(item_ref);
    }

    Ok(collected)
}

fn type_fn(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let obj = match args.len() {
        1 => vm.deref_object(args[0]),
        _ => {
            let msg = vm.intern_string(&format!(
                "type expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
    };

    Ok(obj.class)
}

fn list(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let items = match args.len() {
        0 => vec![],
        1 => collect_iterable(vm, args[0])?,
        _ => {
            let msg = vm.intern_string(&format!(
                "list expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
    };

    let type_ = vm.runtime.borrow().builtin_types.list;
    let obj = vm.new_object(type_, VmValue::List(Container::new(List::new(items))));
    Ok(obj)
}

fn tuple(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let items = match args.len() {
        0 => vec![],
        1 => collect_iterable(vm, args[0])?,
        _ => {
            let msg = vm.intern_string(&format!(
                "tuple expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
    };

    let type_ = vm.runtime.borrow().builtin_types.tuple;
    let obj = vm.new_object(type_, VmValue::Tuple(Tuple::new(items)));
    Ok(obj)
}

fn bool(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let value = match args.len() {
        0 => false,
        1 => vm.deref(args[0]).to_boolean(),
        _ => {
            let msg = vm.intern_string(&format!(
                "bool expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
    };

    Ok(vm.to_heapified_bool(value))
}

fn int(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let value = match args.len() {
        0 => 0,
        1 => vm.coerce_to_int(&vm.deref(args[0])).raise(vm)?,
        _ => {
            let msg = vm.intern_string(&format!(
                "int expected at most 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
    };

    let type_ = vm.runtime.borrow().builtin_types.int;
    let obj = vm.new_object(type_, VmValue::Int(value));
    Ok(obj)
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
            let msg = vm.intern_string(&format!(
                "range expected at least 1 argument, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
        _ => {
            let msg = vm.intern_string(&format!(
                "range expected at most 3 arguments, got {}",
                args.len()
            ));
            return Exception::type_error(msg).raise(vm);
        }
    };

    let type_ = vm.runtime.borrow().builtin_types.range;
    let obj = vm.new_object(type_, VmValue::Range(range));
    Ok(obj)
}

fn expect_integer_or_raise(vm: &mut VirtualMachine, value: &VmValue) -> VmResult<i64> {
    match value.as_integer() {
        Some(i) => Ok(i),
        None => {
            let msg = vm.intern_string("Expected an integer");
            Exception::type_error(msg).raise(vm)
        }
    }
}

fn iter(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let iterable_ref = match args.len() {
        1 => args[0],
        _ => {
            let msg = vm.intern_string("iter expected exactly 1 argument");
            return Exception::type_error(msg).raise(vm);
        }
    };

    iter_internal(vm, iterable_ref)
}

fn next(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    if args.len() != 1 {
        let msg = vm.intern_string(&format!("next() expected 1 argument, got {}", args.len()));
        return Exception::type_error(msg).raise(vm);
    }

    match next_internal(vm, args[0])? {
        NextResult::Yielded(val) => Ok(val),
        NextResult::Exhausted(_) => Exception::stop_iteration().raise(vm),
    }
}

fn print(vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
    let value = args
        .iter()
        .map(|arg| vm.normalize_vm_ref(*arg).to_string())
        .collect::<Vec<_>>()
        .join(" ");
    vm.state.borrow_mut().io.print_line(&value);
    Ok(vm.none())
}

#[cfg(test)]
mod tests {
    use crate::bytecode_vm::{runtime::runtime::register_builtin_funcs, Runtime};

    use super::*;

    #[test]
    fn register_builtins_inserts_list() {
        let mut runtime = Runtime::new();
        let mut module = Module::new(ModuleName::from_segments(&["test_module"]));
        register_builtin_funcs(&mut runtime, &mut module, &BUILTINS);
        assert!(module.global_store().contains_key("list"));
        assert!(!module.global_store().contains_key("dict"));
    }
}
