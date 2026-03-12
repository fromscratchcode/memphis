use crate::bytecode_vm::runtime::Reference;

pub fn find_index<T, Q>(vec: &[T], query: &Q) -> Option<usize>
where
    T: PartialEq<Q>,
    Q: ?Sized,
{
    vec.iter()
        .enumerate()
        .find_map(|(index, value)| if value == query { Some(index) } else { None })
}

#[derive(Debug, PartialEq, Clone, Hash, Eq, PartialOrd, Ord)]
pub enum HashKey {
    Int(i64),
    Str(Reference),
    Tuple(Vec<HashKey>),
}
