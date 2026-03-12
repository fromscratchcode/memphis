#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum HashKey {
    Int(i64),
    /// Use the bit representation so we can safely derive Hash and Eq
    Float(u64),
    Str(String),
    Tuple(Vec<HashKey>),
}
