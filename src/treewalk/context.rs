#[cfg(test)]
use crate::domain::Source;
use crate::{
    core::{Container, Interpreter},
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{types::Module, RaisedException, TreewalkInterpreter, TreewalkState, TreewalkValue},
};

pub struct TreewalkContext {
    lexer: Lexer,
    interpreter: TreewalkInterpreter,
}

impl TreewalkContext {
    pub fn new(text: Text, origin: ModuleOrigin) -> Self {
        let state = Self::init_state(origin);
        Self::from_state(text, state)
    }

    pub fn from_state(text: Text, treewalk_state: Container<TreewalkState>) -> Self {
        Self {
            lexer: Lexer::new(&text),
            interpreter: TreewalkInterpreter::new(treewalk_state),
        }
    }

    pub fn run_inner(&mut self) -> Result<TreewalkValue, RaisedException> {
        // Destructure to break the borrow into disjoint pieces
        let TreewalkContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.execute(&mut parser)
    }

    pub fn read_inner(&self, name: &str) -> Option<TreewalkValue> {
        self.interpreter.load_var(name).ok()
    }

    pub fn add_text_inner(&mut self, line: Text) {
        self.lexer.add_text(&line);
    }

    fn init_state(origin: ModuleOrigin) -> Container<TreewalkState> {
        let state = Container::new(MemphisState::init(origin.clone()));
        let treewalk_state = Container::new(TreewalkState::new(state));

        let module = Container::new(Module::new(ModuleName::main(), ModuleName::empty(), origin));
        treewalk_state.push_module_context(module);

        treewalk_state
    }

    #[cfg(test)]
    pub fn interpreter(&self) -> &TreewalkInterpreter {
        &self.interpreter
    }

    #[cfg(test)]
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

impl Interpreter for TreewalkContext {
    fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.run_inner().map(Into::into).map_err(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_inner(name).map(Into::into)
    }

    fn add_text(&mut self, line: Text) {
        self.add_text_inner(line);
    }
}
