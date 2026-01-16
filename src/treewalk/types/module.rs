#[cfg(feature = "c_stdlib")]
use std::collections::hash_map::Iter;
use std::path::{Path, PathBuf};

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
    package: ModuleName,
    origin: ModuleOrigin,
    scope: Scope,
}

impl Module {
    pub fn new(name: ModuleName, package: ModuleName, origin: ModuleOrigin) -> Self {
        let scope = init_scope(&name, &package);

        Self {
            name,
            package,
            origin,
            scope,
        }
    }

    pub fn new_file_backed(name: ModuleName, package: ModuleName, path: &Path) -> Self {
        Self::new(name, package, ModuleOrigin::File(path.to_path_buf()))
    }

    pub fn new_builtin(name: ModuleName) -> Self {
        Self::new(name, ModuleName::empty(), ModuleOrigin::Builtin)
    }

    pub fn new_empty(name: ModuleName) -> Self {
        Self::new(name, ModuleName::empty(), ModuleOrigin::Synthetic)
    }

    pub fn path(&self) -> PathBuf {
        self.origin.path()
    }

    pub fn name(&self) -> &ModuleName {
        &self.name
    }

    pub fn package(&self) -> &ModuleName {
        &self.package
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

fn init_scope(module: &ModuleName, package: &ModuleName) -> Scope {
    let mut scope = Scope::default();
    scope.insert(
        &Dunder::Name,
        TreewalkValue::Str(Str::new(&module.as_str())),
    );

    let package_value = if package.is_empty() {
        TreewalkValue::None
    } else {
        TreewalkValue::Str(Str::new(&package.as_str()))
    };
    scope.insert(&Dunder::Package, package_value);

    scope
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

impl ToDebugStackFrame for Module {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new("<module>", self.path(), 1)
    }
}
