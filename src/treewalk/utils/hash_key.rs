#[derive(PartialEq, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum HashKey {
    Int(i64),
    Str(String),
}
