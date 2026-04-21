#[cfg(test)]
use crate::domain::Source;
use crate::{
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{
        types::{Exception, Module},
        RaisedException, TreewalkInterpreter, TreewalkState, TreewalkValue,
    },
    Interpreter,
};

pub struct TreewalkContext {
    lexer: Lexer,
    interpreter: TreewalkInterpreter,
}

impl TreewalkContext {
    pub fn new(memphis_state: Container<MemphisState>, origin: ModuleOrigin) -> Self {
        let state = Self::init_state(memphis_state.clone(), origin);
        Self::from_state(memphis_state, state)
    }

    pub fn from_state(
        memphis_state: Container<MemphisState>,
        treewalk_state: Container<TreewalkState>,
    ) -> Self {
        Self {
            lexer: Lexer::script(),
            interpreter: TreewalkInterpreter::new(memphis_state, treewalk_state),
        }
    }

    pub fn eval_inner(&mut self, text: Text) -> Result<TreewalkValue, RaisedException> {
        self.add_text(text);
        self.run()
    }

    fn run(&mut self) -> Result<TreewalkValue, RaisedException> {
        // Destructure to break the borrow into disjoint pieces
        let TreewalkContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        let ast = parser
            .parse()
            .map_err(|e| interpreter.raise(Exception::syntax_error(e.to_string())))?;

        interpreter.execute(ast)
    }

    fn add_text(&mut self, line: Text) {
        self.lexer.add_text(&line);
    }

    fn init_state(
        memphis_state: Container<MemphisState>,
        origin: ModuleOrigin,
    ) -> Container<TreewalkState> {
        let treewalk_state = Container::new(TreewalkState::new());

        let module = Container::new(Module::new(ModuleName::main(), None, origin));
        memphis_state.push_stack_frame(&*module.borrow());
        treewalk_state.push_module(module);

        treewalk_state
    }

    #[cfg(test)]
    pub fn interpreter(&self) -> &TreewalkInterpreter {
        &self.interpreter
    }

    #[cfg(test)]
    /// This is deprecated, but we still depend on it in a lot of the tests.
    pub fn read_inner(&self, name: &str) -> Option<TreewalkValue> {
        self.interpreter.load_var(name).ok()
    }

    #[cfg(test)]
    pub fn enable_capture(&mut self) {
        self.interpreter
            .memphis_state
            .borrow_mut()
            .io
            .enable_capture();
    }

    #[cfg(test)]
    pub fn take_output(&mut self) -> Option<String> {
        self.interpreter.memphis_state.borrow_mut().io.take_output()
    }

    #[cfg(test)]
    pub fn stdin() -> Self {
        // We don't need to initialize the ModuleOrigin here because there's no filepath to record.
        let state = Container::new(MemphisState::new());
        Self::new(state, ModuleOrigin::Stdin)
    }

    #[cfg(test)]
    pub fn script(source: Source) -> Self {
        let origin = ModuleOrigin::File(source.path().to_path_buf());
        let state = Container::new(MemphisState::init(&origin));
        Self::new(state, origin)
    }
}

impl Interpreter for TreewalkContext {
    fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        self.eval_inner(text).map(Into::into).map_err(Into::into)
    }
}
