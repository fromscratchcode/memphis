use std::{collections::HashMap, path::PathBuf};

use crate::{
    domain::{DebugStackFrame, Dunder, ModuleName, ModuleOrigin, ScriptPath, ToDebugStackFrame},
    treewalk::{
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue, protocols::MemberRead,
        types::Str,
    },
};

#[derive(Debug, Clone)]
pub struct Module {
    name: ModuleName,
    package: Option<ModuleName>,
    origin: ModuleOrigin,
    scope: Scope,
}

impl Module {
    pub fn new(name: ModuleName, package: Option<ModuleName>, origin: ModuleOrigin) -> Self {
        let scope = init_scope(&name, &package);

        Self {
            name,
            package,
            origin,
            scope,
        }
    }

    pub fn new_file_backed(
        name: ModuleName,
        package: Option<ModuleName>,
        path: ScriptPath,
    ) -> Self {
        Self::new(name, package, ModuleOrigin::File(path))
    }

    pub fn new_builtin(name: ModuleName) -> Self {
        Self::new(name, None, ModuleOrigin::Builtin)
    }

    pub fn new_empty(name: ModuleName) -> Self {
        Self::new(name, None, ModuleOrigin::Synthetic)
    }

    pub fn path(&self) -> PathBuf {
        self.origin.path()
    }

    pub fn name(&self) -> &ModuleName {
        &self.name
    }

    pub fn package(&self) -> &Option<ModuleName> {
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

    pub fn symbol_table(&self) -> &HashMap<String, TreewalkValue> {
        self.scope.symbol_table()
    }
}

fn init_scope(module: &ModuleName, package: &Option<ModuleName>) -> Scope {
    let mut scope = Scope::default();
    scope.insert(&Dunder::Name, TreewalkValue::Str(Str::new(module.as_str())));

    let package_value = if let Some(package) = package {
        TreewalkValue::Str(Str::new(package.as_str()))
    } else {
        TreewalkValue::None
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
