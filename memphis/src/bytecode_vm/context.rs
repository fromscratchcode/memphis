use crate::{
    HostIo,
    bytecode_vm::{
        Compiler, CompilerError, Runtime, VirtualMachine, VmResult, VmValue, compiler::CodeObject,
    },
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    interpreter::Interpreter,
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmContext {
    module_name: ModuleName,
    package: Option<ModuleName>,
    path_str: String,
    vm: VirtualMachine,
}

impl VmContext {
    pub fn init(origin: ModuleOrigin, io: impl HostIo + 'static) -> Self {
        let state = Container::new(MemphisState::init(&origin, io));
        let runtime = Container::new(Runtime::new());
        Self::from_state(ModuleName::main(), None, origin, state, runtime)
    }

    pub fn from_state(
        module_name: ModuleName,
        package: Option<ModuleName>,
        origin: ModuleOrigin,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> Self {
        Self {
            module_name,
            package,
            path_str: origin.path_str(),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn eval_inner(&mut self, text: Text) -> VmResult<VmValue> {
        let code = self.compile(&text).map_err(|e| {
            let exc = e.into_exception(&mut self.vm);
            self.vm
                .init_and_raise(exc, self.module_name.clone(), &self.path_str)
        })?;
        self.vm.execute(code)
    }

    pub fn compile(&self, text: &Text) -> Result<CodeObject, CompilerError> {
        let mut ast =
            Parser::parse_text(text).map_err(|e| CompilerError::SyntaxError(e.to_string()))?;
        ast.rewrite_last_expr_to_return();

        let mut compiler = Compiler::new(&self.module_name, &self.package, &self.path_str);
        compiler.compile(&ast)
    }

    #[cfg(test)]
    pub fn vm(&self) -> &VirtualMachine {
        &self.vm
    }

    #[cfg(test)]
    pub fn set_module(&mut self, name: ModuleName) {
        self.module_name = name;
    }

    #[cfg(test)]
    pub fn set_pkg(&mut self, name: ModuleName) {
        self.package = Some(name);
    }
}

impl Interpreter for VmContext {
    fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        let value = self.eval_inner(text).map_err(|e| e.normalize(&self.vm))?;
        Ok(self.vm.normalize_vm_value(value))
    }
}
