use std::collections::{hash_map::Iter, HashMap, HashSet};

use crate::treewalk::{
    types::{Dict, Str},
    TreewalkValue,
};

/// This is similar to our runtime `Dict` object, but where keys must be valid Python runtime
/// identifiers (basically, strings).
// TODO make this a real type.
pub type SymbolTable = HashMap<String, TreewalkValue>;

pub fn symbol_table_to_runtime_dict(symbol_table: &SymbolTable) -> Dict {
    let items = symbol_table
        .iter()
        .map(|(key, value)| (TreewalkValue::Str(Str::new(key)), value.clone()))
        .collect();

    Dict::from_items(items).expect("All keys should be hashable strings here.")
}

/// This represents a symbol table for a given scope.
#[derive(Debug, PartialEq, Clone, Default)]
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

    pub fn get(&self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.get(name).cloned()
    }

    /// Return a list of all the symbols available in this `Scope`.
    pub fn symbols(&self) -> Vec<String> {
        self.symbol_table.keys().cloned().collect()
    }

    pub fn delete(&mut self, name: &str) -> Option<TreewalkValue> {
        self.symbol_table.remove(name)
    }

    /// Insert an `TreewalkValue` to this `Scope`. The `Scope` is returned to allow calls to be
    /// chained.
    pub fn insert(&mut self, name: &str, value: TreewalkValue) -> &mut Self {
        self.symbol_table.insert(name.to_string(), value);
        self
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

    pub fn to_runtime_dict(&self) -> Dict {
        symbol_table_to_runtime_dict(&self.symbol_table)
    }
}

/// Implement IntoIterator for &Scope to allow iteration by reference
impl<'a> IntoIterator for &'a Scope {
    type Item = (&'a String, &'a TreewalkValue);
    type IntoIter = Iter<'a, String, TreewalkValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.symbol_table.iter()
    }
}
