pub fn wrap_negative(index: i64, len: usize) -> i64 {
    if index < 0 {
        len as i64 + index
    } else {
        index
    }
}

pub fn normalize_index(index: i64, len: usize) -> Option<usize> {
    let idx = wrap_negative(index, len);

    if idx < 0 || idx >= len as i64 {
        None
    } else {
        Some(idx as usize)
    }
}
