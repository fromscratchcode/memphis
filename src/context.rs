use crate::{
    bytecode_vm::VmContext,
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleOrigin, Source, Text},
    runtime::MemphisState,
    treewalk::TreewalkContext,
    Engine, Interpreter,
};

pub struct MemphisContext {
    context: Box<dyn Interpreter>,
    state: Container<MemphisState>,
}

impl MemphisContext {
    pub fn stdin(engine: Engine) -> Self {
        Self::new(engine, ModuleOrigin::Stdin)
    }

    pub fn script(engine: Engine, source: Source) -> Self {
        Self::new(engine, ModuleOrigin::File(source.path().to_path_buf()))
    }

    pub fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        self.context.eval(text)
    }

    pub fn enable_capture(&mut self) {
        self.state.borrow_mut().io.enable_capture();
    }

    pub fn take_output(&mut self) -> Option<String> {
        self.state.borrow_mut().io.take_output()
    }

    fn new(engine: Engine, origin: ModuleOrigin) -> Self {
        let state = Container::new(MemphisState::init(&origin));
        let context: Box<dyn Interpreter> = match engine {
            Engine::Treewalk => Box::new(TreewalkContext::new(state.clone(), origin)),
            Engine::BytecodeVm => Box::new(VmContext::new(state.clone(), origin)),
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => todo!(),
        };
        Self { state, context }
    }
}
