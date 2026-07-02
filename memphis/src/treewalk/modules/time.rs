use std::time::Duration;

use crate::{
    core::Container,
    domain::ModuleName,
    treewalk::{
        protocols::Callable,
        result::Raise,
        type_system::CloneableCallable,
        types::Module,
        utils::{check_args, Args},
        ModuleStore, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct TimeSleepBuiltin;

impl Callable for TimeSleepBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let duration_in_s = args.get_arg(0).as_float().raise(interpreter)?;
        let micros = duration_in_s * 1_000_000.0;
        let dur = Duration::from_micros(micros as u64);
        std::thread::sleep(dur);
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "sleep".into()
    }
}

fn builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![Box::new(TimeSleepBuiltin)]
}

fn init() -> Module {
    let mut mod_ = Module::new_builtin(ModuleName::from_segments(&["time"]));
    for builtin in builtins() {
        mod_.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }
    mod_
}

pub fn import(module_store: &mut ModuleStore) {
    let mod_ = init();
    module_store.store_module(Container::new(mod_));
}
