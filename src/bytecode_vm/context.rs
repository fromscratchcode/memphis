#[cfg(test)]
use crate::domain::Source;
use crate::{
    bytecode_vm::{
        compiler::CodeObject, Compiler, CompilerError, Runtime, VirtualMachine, VmResult, VmValue,
    },
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    Interpreter,
};

pub struct VmContext {
    lexer: Lexer,
    module_name: ModuleName,
    package: Option<ModuleName>,
    path_str: String,
    vm: VirtualMachine,
}

impl VmContext {
    pub fn new(origin: ModuleOrigin) -> Self {
        let state = Container::new(MemphisState::init(origin.clone()));
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
            lexer: Lexer::new(),
            module_name,
            package,
            path_str: origin.path_str(),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn eval_inner(&mut self, text: Text) -> VmResult<VmValue> {
        self.add_text(text);
        self.run()
    }

    fn run(&mut self) -> VmResult<VmValue> {
        let code = self.compile().map_err(|e| {
            let exc = e.into_exception(&mut self.vm);
            self.vm
                .init_and_raise(exc, self.module_name.clone(), &self.path_str)
        })?;
        self.vm.execute(code)
    }

    pub fn compile(&mut self) -> Result<CodeObject, CompilerError> {
        let mut parser = Parser::new(&mut self.lexer);
        let mut ast = parser
            .parse()
            .map_err(|e| CompilerError::SyntaxError(e.to_string()))?;
        ast.rewrite_last_expr_to_return();

        let mut compiler = Compiler::new(&self.module_name, &self.package, &self.path_str);
        compiler.compile(&ast)
    }

    pub fn add_text(&mut self, line: Text) {
        self.lexer.add_text(&line);
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

    #[cfg(test)]
    pub fn enable_capture(&mut self) {
        self.vm.state.borrow_mut().io.enable_capture();
    }

    #[cfg(test)]
    pub fn take_output(&mut self) -> Option<String> {
        self.vm.state.borrow_mut().io.take_output()
    }

    #[cfg(any(test, feature = "wasm"))]
    pub fn stdin() -> Self {
        Self::new(ModuleOrigin::Stdin)
    }

    #[cfg(test)]
    pub fn script(source: Source) -> Self {
        Self::new(ModuleOrigin::File(source.path().to_path_buf()))
    }
}

impl Interpreter for VmContext {
    fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        let value = self.eval_inner(text).map_err(|e| e.normalize(&self.vm))?;
        Ok(self.vm.normalize_vm_value(value))
    }
}
