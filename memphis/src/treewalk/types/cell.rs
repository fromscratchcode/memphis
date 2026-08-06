use crate::treewalk::{
    SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue, protocols::MemberRead,
};

/// This corresponds to the Python internal `Cell` class, which is returned for values captured in
/// a closure.
#[derive(Clone)]
pub struct Cell {
    symbol_table: SymbolTable,
}

impl Cell {
    pub fn new(value: TreewalkValue) -> Self {
        let mut symbol_table = SymbolTable::default();
        symbol_table.insert("cell_contents", value);
        Self { symbol_table }
    }
}

impl MemberRead for Cell {
    fn get_member(
        &self,
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(self.symbol_table.get(name).cloned())
    }
}
