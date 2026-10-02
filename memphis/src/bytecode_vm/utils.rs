use crate::bytecode_vm::runtime::Reference;

#[derive(Debug, PartialEq, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum HashKey {
    Int(i64),
    Str(Reference),
    Tuple(Vec<HashKey>),
}
