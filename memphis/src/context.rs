use crate::{
    Engine,
    bytecode_vm::VmContext,
    domain::{MemphisResult, MemphisValue, ModuleOrigin, Text},
    interpreter::Interpreter,
    runtime::HostIo,
    treewalk::TreewalkContext,
};

pub struct MemphisContext {
    engine: Engine,
    context: Box<dyn Interpreter>,
}

impl MemphisContext {
    pub fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        self.context.eval(text)
    }

    pub fn engine(&self) -> &Engine {
        &self.engine
    }

    pub fn new(engine: Engine, origin: ModuleOrigin, io: impl HostIo + 'static) -> Self {
        let context: Box<dyn Interpreter> = match engine {
            Engine::Treewalk => Box::new(TreewalkContext::init(origin, io)),
            Engine::BytecodeVm => Box::new(VmContext::init(origin, io)),
        };
        Self { engine, context }
    }
}
