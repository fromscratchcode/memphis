use std::collections::{HashMap, HashSet};

use crate::treewalk::TreewalkValue;

/// This is similar to our runtime `Dict` object, but where keys must be valid Python runtime
/// identifiers (basically, strings).
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    values: HashMap<String, TreewalkValue>,
}

impl SymbolTable {
    pub fn new(values: HashMap<String, TreewalkValue>) -> Self {
        Self { values }
    }

    pub fn get(&self, name: &str) -> Option<&TreewalkValue> {
        self.values.get(name)
    }

    pub fn symbols(&self) -> Vec<String> {
        self.values.keys().cloned().collect()
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.values.remove(name)
    }

    pub fn insert(&mut self, name: &str, value: TreewalkValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn has(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, TreewalkValue> {
        self.values.iter()
    }

    pub fn into_inner(self) -> HashMap<String, TreewalkValue> {
        self.values
    }
}

/// This represents a symbol table for a given scope.
#[derive(Debug, Clone, Default)]
pub struct Scope {
    symbol_table: SymbolTable,

    /// Used to hold directives such as `global x` which will expire with this scope.
    global_vars: HashSet<String>,

    /// Used to hold directives such as `nonlocal x` which will expire with this scope.
    nonlocal_vars: HashSet<String>,
}

impl Scope {
    /// Constructs a new `Scope` from an already-bound symbol table.
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            global_vars: HashSet::new(),
            nonlocal_vars: HashSet::new(),
        }
    }

    /// Given a variable `var`, indicate that `var` should refer to the variable in the
    /// global/module scope (which does not live in this struct) for the duration of _this_
    /// local scope.
    pub fn mark_global(&mut self, name: &str) {
        self.global_vars.insert(name.to_string());
    }

    /// Given a variable `var`, indicate that `var` should refer to the variable in the
    /// enclosing scope (which does not live in this struct) for the duration of _this_
    /// local scope.
    pub fn mark_nonlocal(&mut self, name: &str) {
        self.nonlocal_vars.insert(name.to_string());
    }

    pub fn has_global(&self, name: &str) -> bool {
        self.global_vars.contains(name)
    }

    pub fn has_nonlocal(&self, name: &str) -> bool {
        self.nonlocal_vars.contains(name)
    }

    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn get(&self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.get(name).cloned()
    }

    pub fn symbols(&self) -> Vec<String> {
        self.symbol_table.symbols()
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.delete(name)
    }

    pub fn insert(&mut self, name: &str, value: TreewalkValue) {
        self.symbol_table.insert(name, value);
    }
}
