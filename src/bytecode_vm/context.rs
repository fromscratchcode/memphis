#[cfg(test)]
use crate::domain::Source;
use crate::{
    bytecode_vm::{
        compiler::CodeObject, runtime::types::Exception, Compiler, CompilerError, Runtime,
        VirtualMachine, VmResult, VmValue,
    },
    core::{Container, Interpreter},
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmContext {
    lexer: Lexer,
    compiler: Compiler,
    vm: VirtualMachine,
}

impl VmContext {
    pub fn new(text: Text, origin: ModuleOrigin) -> Self {
        let state = Container::new(MemphisState::init(origin.clone()));
        let runtime = Container::new(Runtime::new());
        Self::from_state(ModuleName::main(), text, origin, state, runtime)
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_state(
        module_name: ModuleName,
        text: Text,
        origin: ModuleOrigin,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
    ) -> Self {
        Self {
            lexer: Lexer::new(&text),
            compiler: Compiler::new(module_name, &origin.path_str()),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn run_inner(&mut self) -> VmResult<VmValue> {
        // TODO use a real syntax error here
        let code = self
            .compile()
            .map_err(|_e| self.vm.raise(Exception::syntax_error()))?;
        dbg!(&code.bytecode);
        self.vm.execute(code)
    }

    pub fn compile(&mut self) -> Result<CodeObject, CompilerError> {
        let mut parser = Parser::new(&mut self.lexer);
        let mut ast = parser
            .parse()
            .map_err(|e| CompilerError::SyntaxError(e.to_string()))?;
        dbg!(&ast);
        ast.rewrite_last_expr_to_return();
        self.compiler.compile(&ast)
    }

    pub fn add_text_inner(&mut self, line: Text) {
        self.lexer.add_text(&line);
    }

    #[cfg(test)]
    pub fn vm(&self) -> &VirtualMachine {
        &self.vm
    }

    #[cfg(test)]
    pub fn set_module_name(&mut self, name: ModuleName) {
        self.compiler.set_module_name(name);
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
        Ok(self.vm.normalize_vm_value(value).unwrap())
    }

    fn add_text(&mut self, line: Text) {
        self.add_text_inner(line);
    }
}
