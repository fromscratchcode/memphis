use crate::{
    bytecode_vm::VmContext,
    core::Interpreter,
    domain::{MemphisResult, MemphisValue, ModuleOrigin, Source, Text},
    treewalk::TreewalkContext,
    Engine,
};

pub struct MemphisContext {
    context: Box<dyn Interpreter>,
}

impl MemphisContext {
    pub fn from_text(engine: Engine, text: Text) -> Self {
        Self::new(engine, text, ModuleOrigin::Stdin)
    }

    pub fn from_source(engine: Engine, source: Source) -> Self {
        Self::new(
            engine,
            source.text().clone(),
            ModuleOrigin::File(source.path().to_path_buf()),
        )
    }

    pub fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.context.run()
    }

    pub fn add_text(&mut self, line: Text) {
        self.context.add_text(line);
    }

    fn new(engine: Engine, text: Text, origin: ModuleOrigin) -> Self {
        let context: Box<dyn Interpreter> = match engine {
            Engine::Treewalk => Box::new(TreewalkContext::new(text, origin)),
            Engine::BytecodeVm => Box::new(VmContext::new(text, origin)),
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => todo!(),
        };
        Self { context }
    }
}
