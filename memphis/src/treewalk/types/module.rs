use std::path::PathBuf;

use crate::{
    domain::{DebugStackFrame, Dunder, ModuleName, ModuleOrigin, ScriptPath, ToDebugStackFrame},
    treewalk::{
        SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue, protocols::MemberRead,
        types::Str,
    },
};

#[derive(Debug, Clone)]
pub struct Module {
    name: ModuleName,
    package: Option<ModuleName>,
    origin: ModuleOrigin,
    symbol_table: SymbolTable,
}

impl Module {
    pub fn new(name: ModuleName, package: Option<ModuleName>, origin: ModuleOrigin) -> Self {
        let symbol_table = init_symbol_table(&name, &package);

        Self {
            name,
            package,
            origin,
            symbol_table,
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
        self.symbol_table.get(name).cloned()
    }

    pub fn insert(&mut self, name: &str, value: TreewalkValue) {
        self.symbol_table.insert(name, value);
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.delete(name)
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }
}

fn init_symbol_table(module: &ModuleName, package: &Option<ModuleName>) -> SymbolTable {
    let mut symbol_table = SymbolTable::default();
    symbol_table.insert(&Dunder::Name, TreewalkValue::Str(Str::new(module.as_str())));

    let package_value = if let Some(package) = package {
        TreewalkValue::Str(Str::new(package.as_str()))
    } else {
        TreewalkValue::None
    };
    symbol_table.insert(&Dunder::Package, package_value);

    symbol_table
}

impl MemberRead for Module {
    fn get_member(
        &self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(self.symbol_table.get(name).cloned())
    }

    fn dir(&self) -> Vec<String> {
        self.symbol_table.symbols()
    }
}

impl ToDebugStackFrame for Module {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new("<module>", self.path(), 1)
    }
}
