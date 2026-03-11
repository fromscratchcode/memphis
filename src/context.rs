use crate::{
    bytecode_vm::VmContext,
    domain::{MemphisResult, MemphisValue, ModuleOrigin, Source, Text},
    treewalk::TreewalkContext,
    Engine, Interpreter,
};

pub struct MemphisContext {
    context: Box<dyn Interpreter>,
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

    fn new(engine: Engine, origin: ModuleOrigin) -> Self {
        let context: Box<dyn Interpreter> = match engine {
            Engine::Treewalk => Box::new(TreewalkContext::new(origin)),
            Engine::BytecodeVm => Box::new(VmContext::new(origin)),
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => todo!(),
        };
        Self { context }
    }
}
