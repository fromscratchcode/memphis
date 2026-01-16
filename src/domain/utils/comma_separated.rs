pub fn format_comma_separated<I>(iter: I) -> String
where
    I: IntoIterator,
    I::Item: ToString,
{
    format_comma_separated_with(iter, |i| i.to_string())
}

pub fn format_comma_separated_with<I, F>(iter: I, format_fn: F) -> String
where
    I: IntoIterator,
    F: Fn(I::Item) -> String,
{
    iter.into_iter()
        .map(format_fn)
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn format_bytes(bytes: &[u8]) -> String {
    let mut s = String::from("b'");
    for &byte in bytes {
        match byte {
            b'\n' => s.push_str("\\n"),
            b'\r' => s.push_str("\\r"),
            b'\t' => s.push_str("\\t"),
            b'\'' => s.push_str("\\'"),
            b'\\' => s.push_str("\\\\"),
            32..=126 => s.push(byte as char), // printable ASCII
            _ => s.push_str(&format!("\\x{:02x}", byte)), // hex escape
        }
    }
    s.push('\'');
    s
}
