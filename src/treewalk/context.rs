#[cfg(test)]
use crate::domain::Source;
use crate::{
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{types::Module, RaisedException, TreewalkInterpreter, TreewalkState, TreewalkValue},
    Interpreter,
};

pub struct TreewalkContext {
    lexer: Lexer,
    interpreter: TreewalkInterpreter,
}

impl TreewalkContext {
    pub fn new(origin: ModuleOrigin) -> Self {
        let state = Self::init_state(origin);
        Self::from_state(state)
    }

    pub fn from_state(treewalk_state: Container<TreewalkState>) -> Self {
        Self {
            lexer: Lexer::new(),
            interpreter: TreewalkInterpreter::new(treewalk_state),
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
        interpreter.execute(&mut parser)
    }

    fn add_text(&mut self, line: Text) {
        self.lexer.add_text(&line);
    }

    fn init_state(origin: ModuleOrigin) -> Container<TreewalkState> {
        let state = Container::new(MemphisState::init(origin.clone()));
        let treewalk_state = Container::new(TreewalkState::new(state));

        let module = Container::new(Module::new(ModuleName::main(), None, origin));
        treewalk_state.push_module_context(module);

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
    pub fn stdin() -> Self {
        Self::new(ModuleOrigin::Stdin)
    }

    #[cfg(test)]
    pub fn script(source: Source) -> Self {
        Self::new(ModuleOrigin::File(source.path().to_path_buf()))
    }
}

impl Interpreter for TreewalkContext {
    fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        self.eval_inner(text).map(Into::into).map_err(Into::into)
    }
}
