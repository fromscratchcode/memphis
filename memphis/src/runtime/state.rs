use std::path::PathBuf;

use crate::{
    core::Container,
    domain::{
        DebugCallStack, DebugStackFrame, LoadedModule, ModuleName, ModuleOrigin, ResolvedModule,
        ScriptPath, ToDebugStackFrame, resolve,
    },
};

use super::{ImportResolver, MemphisIo};

#[derive(Debug, Clone, PartialEq)]
pub struct ImportError {
    pub message: String,
}

impl ImportError {
    fn new(message: &str) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

pub struct MemphisState {
    import_resolver: ImportResolver,
    debug_call_stack: DebugCallStack,
    line_number: usize,
    pub io: MemphisIo,
}

impl MemphisState {
    pub fn new() -> Self {
        MemphisState {
            import_resolver: ImportResolver::new(),
            debug_call_stack: DebugCallStack::new(),
            line_number: 1,
            io: MemphisIo::new(),
        }
    }

    pub fn init(origin: &ModuleOrigin) -> Self {
        let mut state = MemphisState::new();
        if let ModuleOrigin::File(p) = origin {
            state.register_root(p);
        }
        state
    }

    fn register_root(&mut self, path: &ScriptPath) {
        self.import_resolver.register_root(path);
    }
}

impl Container<MemphisState> {
    pub fn save_line_number(&self) {
        let line_number = self.borrow().line_number;
        self.borrow_mut()
            .debug_call_stack
            .update_line_number(line_number);
    }

    pub fn set_line_number(&self, line_number: usize) {
        self.borrow_mut().line_number = line_number;
    }

    /// Return the `CallStack` at the current moment in time. This should be used at the time of an
    /// exception or immediately before any other use as it is a snapshot and will not keep updating.
    pub fn debug_call_stack(&self) -> DebugCallStack {
        self.borrow().debug_call_stack.clone()
    }

    pub fn push_stack_frame<T: ToDebugStackFrame>(&self, context: &T) {
        self.borrow_mut()
            .debug_call_stack
            .push_stack_frame(context.to_stack_frame());
    }

    pub fn pop_stack_frame(&self) -> Option<DebugStackFrame> {
        self.borrow_mut().debug_call_stack.pop_stack_frame()
    }

    pub fn load_source(&self, module_name: &ModuleName) -> Result<LoadedModule, ImportError> {
        let resolved = self.resolve_module(module_name)?;
        let loaded = resolved
            .load()
            .map_err(|_| ImportError::new(&format!("No module named {}", module_name)))?;
        Ok(loaded)
    }

    fn resolve_module(&self, module_name: &ModuleName) -> Result<ResolvedModule, ImportError> {
        let search_paths = self.search_paths();
        resolve(module_name, &search_paths)
            .ok_or_else(|| ImportError::new(&format!("No module named {}", module_name)))
    }

    fn search_paths(&self) -> Vec<PathBuf> {
        self.borrow().import_resolver.search_paths().to_vec()
    }
}
