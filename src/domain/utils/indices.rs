pub fn wrap_negative(index: i64, len: i64) -> i64 {
    if index < 0 {
        len + index
    } else {
        index
    }
}

pub fn normalize_index(index: i64, len: i64) -> Option<usize> {
    let idx = wrap_negative(index, len);

    if idx < 0 || idx >= len {
        None
    } else {
        Some(idx as usize)
    }
}
