use crate::{
    Engine,
    bytecode_vm::VmContext,
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleOrigin, ScriptPath, Text},
    interpreter::Interpreter,
    runtime::MemphisState,
    treewalk::TreewalkContext,
};

pub struct MemphisContext {
    engine: Engine,
    context: Box<dyn Interpreter>,
    state: Container<MemphisState>,
}

impl MemphisContext {
    pub fn stdin(engine: Engine) -> Self {
        Self::new(engine, ModuleOrigin::Stdin)
    }

    pub fn script(engine: Engine, path: ScriptPath) -> Self {
        Self::new(engine, ModuleOrigin::File(path))
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

    pub fn engine(&self) -> &Engine {
        &self.engine
    }

    fn new(engine: Engine, origin: ModuleOrigin) -> Self {
        let state = Container::new(MemphisState::init(&origin));
        let context: Box<dyn Interpreter> = match engine {
            Engine::Treewalk => Box::new(TreewalkContext::new(state.clone(), origin)),
            Engine::BytecodeVm => Box::new(VmContext::new(state.clone(), origin)),
        };
        Self {
            engine,
            state,
            context,
        }
    }
}
