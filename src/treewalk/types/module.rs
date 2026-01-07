#[cfg(feature = "c_stdlib")]
use std::collections::hash_map::Iter;
use std::{
    fmt::{Display, Error, Formatter},
    path::{Path, PathBuf},
};

use crate::{
    core::Container,
    domain::{DebugStackFrame, Dunder, ModuleName, ModuleOrigin, ToDebugStackFrame},
    treewalk::{
        protocols::MemberRead,
        types::{Dict, Str},
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    name: ModuleName,
    scope: Scope,
    origin: ModuleOrigin,
}

impl Module {
    pub fn new(name: ModuleName, origin: ModuleOrigin) -> Self {
        let mut scope = Scope::default();
        scope.insert(&Dunder::Name, TreewalkValue::Str(Str::new(&name.as_str())));
        Self {
            name,
            scope,
            origin,
        }
    }

    pub fn new_file_backed(name: ModuleName, path: &Path) -> Self {
        Self::new(name, ModuleOrigin::File(path.to_path_buf()))
    }

    pub fn new_builtin(name: ModuleName) -> Self {
        Self::new(name, ModuleOrigin::Builtin)
    }

    pub fn new_empty(name: ModuleName) -> Self {
        Self::new(name, ModuleOrigin::Synthetic)
    }

    pub fn path(&self) -> PathBuf {
        self.origin.path()
    }

    pub fn name(&self) -> &ModuleName {
        &self.name
    }

    pub fn get(&self, name: &str) -> Option<TreewalkValue> {
        self.scope.get(name)
    }

    pub fn insert(&mut self, name: &str, value: TreewalkValue) {
        self.scope.insert(name, value);
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.scope.delete(name)
    }

    // Should this return an actual dict? We chose not to do that right now because a
    // `Container<Dict>` requires a reference to the interpreter.
    #[cfg(feature = "c_stdlib")]
    pub fn dict(&self) -> Iter<'_, String, TreewalkValue> {
        self.scope.into_iter()
    }

    pub fn as_dict(&self, interpreter: &TreewalkInterpreter) -> Container<Dict> {
        self.scope.as_dict(interpreter)
    }
}

impl MemberRead for Module {
    fn get_member(
        &self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(self.scope.get(name))
    }

    fn dir(&self) -> Vec<String> {
        self.scope.symbols()
    }
}

impl Display for Container<Module> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<module '{}'>", self.borrow().name())
    }
}

impl ToDebugStackFrame for Module {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new("<module>", self.path(), 1)
    }
}
