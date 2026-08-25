use crate::{
    HostIo,
    core::Container,
    domain::{MemphisResult, MemphisValue, ModuleName, ModuleOrigin, Text},
    interpreter::Interpreter,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{
        RaisedException, TreewalkInterpreter, TreewalkState, TreewalkValue,
        types::{Exception, Module},
    },
};

pub struct TreewalkContext {
    interpreter: TreewalkInterpreter,
}

impl TreewalkContext {
    pub fn init(origin: ModuleOrigin, io: impl HostIo + 'static) -> Self {
        let memphis_state = Container::new(MemphisState::init(&origin, io));
        let state = Self::init_state(memphis_state.clone(), origin);
        Self::from_state(memphis_state, state)
    }

    pub fn from_state(
        memphis_state: Container<MemphisState>,
        treewalk_state: Container<TreewalkState>,
    ) -> Self {
        Self {
            interpreter: TreewalkInterpreter::new(memphis_state, treewalk_state),
        }
    }

    pub fn eval_inner(&mut self, text: Text) -> Result<TreewalkValue, RaisedException> {
        let ast = Parser::parse_text(&text).map_err(|e| {
            self.interpreter
                .raise(Exception::syntax_error(e.to_string()))
        })?;
        self.interpreter.execute(ast)
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
}

impl Interpreter for TreewalkContext {
    fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue> {
        self.eval_inner(text).map(Into::into).map_err(Into::into)
    }
}
