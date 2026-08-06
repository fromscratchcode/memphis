use crate::{
    core::Container,
    domain::ModuleName,
    treewalk::{
        ModuleStore, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal,
        TreewalkValue,
        protocols::Callable,
        result::Raise,
        type_system::CloneableCallable,
        types::Module,
        utils::{BoundArgs, Parameter, Signature},
    },
};

#[derive(Clone)]
pub struct AsyncioRunBuiltin;
#[derive(Clone)]
pub struct AsyncioSleepBuiltin;
#[derive(Clone)]
pub struct AsyncioCreateTaskBuiltin;

impl Callable for AsyncioRunBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([Parameter::required("main")])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let coroutine = args.get("main").as_coroutine().raise(interpreter)?;
        interpreter.with_executor(|exec| exec.run(interpreter, coroutine))
    }

    fn name(&self) -> String {
        "run".into()
    }
}

impl Callable for AsyncioSleepBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([Parameter::required("delay")])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let duration = args.get("delay").coerce_to_float().raise(interpreter)?;
        Err(TreewalkDisruption::Signal(TreewalkSignal::Sleep(duration)))
    }

    fn name(&self) -> String {
        "sleep".into()
    }
}

impl Callable for AsyncioCreateTaskBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([Parameter::required("coro")])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let coroutine = args.get("coro").as_coroutine().raise(interpreter)?;
        interpreter.with_executor(|exec| exec.spawn(coroutine))
    }

    fn name(&self) -> String {
        "create_task".into()
    }
}

fn builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![
        Box::new(AsyncioRunBuiltin),
        Box::new(AsyncioSleepBuiltin),
        Box::new(AsyncioCreateTaskBuiltin),
    ]
}

fn init() -> Module {
    let mut mod_ = Module::new_builtin(ModuleName::from_segments(&["asyncio"]));
    for builtin in builtins() {
        mod_.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }
    mod_
}

pub fn import(module_store: &mut ModuleStore) {
    let asyncio_mod = init();
    module_store.store_module(Container::new(asyncio_mod));
}
