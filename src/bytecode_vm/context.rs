#[cfg(test)]
use crate::domain::Source;
use crate::{
    bytecode_vm::{
        compiler::CodeObject, Compiler, CompilerError, Runtime, VirtualMachine, VmResult, VmValue,
    },
    core::{Container, Interpreter},
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmContext {
    lexer: Lexer,
    module_name: ModuleName,
    package: ModuleName,
    path_str: String,
    vm: VirtualMachine,
}

impl VmContext {
    pub fn new(text: Text, origin: ModuleOrigin) -> Self {
        let state = Container::new(MemphisState::init(origin.clone()));
        let runtime = Container::new(Runtime::new());
        Self::from_state(
            ModuleName::main(),
            ModuleName::empty(),
            text,
            origin,
            state,
            runtime,
        )
    }

    pub fn from_state(
        module_name: ModuleName,
        package: ModuleName,
        text: Text,
        origin: ModuleOrigin,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> Self {
        Self {
            lexer: Lexer::new(&text),
            module_name,
            package,
            path_str: origin.path_str(),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn run_inner(&mut self) -> VmResult<VmValue> {
        let code = self.compile().map_err(|e| {
            let exc = e.into_exception(&mut self.vm);
            self.vm.raise(exc)
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

    pub fn read_inner(&self, name: &str) -> Option<VmValue> {
        self.vm.read_global(name)
    }

    pub fn add_text_inner(&mut self, line: Text) {
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
        self.package = name;
    }

    #[cfg(any(test, feature = "wasm"))]
    pub fn from_text(text: Text) -> Self {
        Self::new(text, ModuleOrigin::Stdin)
    }

    #[cfg(test)]
    pub fn from_source(source: Source) -> Self {
        Self::new(
            source.text().clone(),
            ModuleOrigin::File(source.path().to_path_buf()),
        )
    }
}

impl Interpreter for VmContext {
    fn run(&mut self) -> MemphisResult<MemphisValue> {
        let value = self.run_inner().map_err(|e| e.normalize(&self.vm))?;
        Ok(self.vm.normalize_vm_value(value))
    }

    fn read(&self, name: &str) -> Option<MemphisValue> {
        let value = self.read_inner(name)?;
        Some(self.vm.normalize_vm_value(value))
    }

    fn add_text(&mut self, line: Text) {
        self.add_text_inner(line);
    }
}
