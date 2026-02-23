use crate::{
    bytecode_vm::{
        runtime::{types::Exception, Reference},
        VirtualMachine, VmResult, VmValue,
    },
    domain::utils::normalize_index,
};

fn get_char(the_str: &str, index: usize) -> Option<char> {
    the_str.chars().nth(index)
}

fn get(the_str: &str, index: usize) -> Option<String> {
    get_char(the_str, index).map(|c| c.to_string())
}

fn len(the_str: &str) -> usize {
    the_str.chars().count()
}

fn get_normalized(the_str: &str, index: i64) -> Option<String> {
    normalize_index(index, len(the_str)).and_then(|idx| get(the_str, idx))
}

// TODO this should probably become a real Str type
pub fn str_getitem(the_str: &str, vm: &mut VirtualMachine, index: VmValue) -> VmResult<Reference> {
    let value = match index {
        VmValue::Int(i) => {
            if let Some(val) = get_normalized(the_str, i) {
                vm.intern_string(&val)
            } else {
                let msg = vm.intern_string("string index out of range");
                let exp = Exception::index_error(msg);
                return Err(vm.raise(exp));
            }
        }
        _ => {
            let msg = vm.intern_string("string indices must be integers or slices, not TODO");
            let exp = Exception::type_error(msg);
            return Err(vm.raise(exp));
        }
    };

    Ok(value)
}
