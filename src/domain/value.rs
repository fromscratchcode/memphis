use std::fmt::{Display, Formatter, Result};

/// A common implementation to represent the return value of a Python expression for use in tests,
/// REPL, or other read-only contexts. This frees each engine up to implement their return values
/// as they like, provided the [`From`] trait is implemented.
#[derive(Clone, Debug, PartialEq)]
pub enum MemphisValue {
    None,
    Integer(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    List(Vec<MemphisValue>),
    Unimplemented(&'static str),
}

impl MemphisValue {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl Display for MemphisValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MemphisValue::None => write!(f, "None"),
            MemphisValue::Integer(i) => write!(f, "{i}"),
            MemphisValue::Float(i) => write!(f, "{i}"),
            MemphisValue::Str(s) => write!(f, "{s}"),
            MemphisValue::Boolean(b) => match b {
                true => write!(f, "True"),
                false => write!(f, "False"),
            },
            MemphisValue::List(i) => {
                let items = i
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{items}]")
            }
            MemphisValue::Unimplemented(msg) => write!(f, "Unimplemented: {msg}"),
        }
    }
}
