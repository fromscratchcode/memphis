use std::time::Duration;

use crate::{
    core::Container,
    domain::ModuleName,
    treewalk::{
        ModuleStore, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        protocols::Callable,
        result::Raise,
        type_system::CloneableCallable,
        types::Module,
        utils::{BoundArgs, Signature},
    },
};

#[derive(Clone)]
pub struct TimeSleepBuiltin;

impl Callable for TimeSleepBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["duration"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let duration_in_s = args.get("duration").as_float().raise(interpreter)?;
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
