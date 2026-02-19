use std::fmt::{Display, Error, Formatter};

use crate::domain::{DebugCallStack, MemphisValue, Type};

#[derive(Debug, PartialEq, Clone)]
pub enum ExceptionKind {
    RuntimeError,
    ImportError,
    TypeError,
    LookupError,
    KeyError,
    IndexError,
    ValueError,
    NameError,
    AttributeError,
    DivisionByZero,
    StopIteration,
    AssertionError,
    SyntaxError,
}

impl ExceptionKind {
    pub fn get_type(&self) -> Type {
        match self {
            Self::TypeError => Type::TypeError,
            Self::StopIteration => Type::StopIteration,
            Self::DivisionByZero => Type::ZeroDivisionError,
            Self::RuntimeError => Type::RuntimeError,
            Self::ImportError => Type::ImportError,
            Self::LookupError => Type::LookupError,
            Self::KeyError => Type::KeyError,
            Self::IndexError => Type::IndexError,
            Self::ValueError => Type::ValueError,
            Self::NameError => Type::NameError,
            Self::AttributeError => Type::AttributeError,
            Self::AssertionError => Type::AssertionError,
            Self::SyntaxError => Type::SyntaxError,
        }
    }

    pub fn display_name(&self) -> String {
        self.get_type().to_string()
    }
}

pub type MemphisResult<T> = Result<T, RaisedMemphisError>;

#[derive(Debug, PartialEq, Clone)]
pub struct RaisedMemphisError {
    pub debug_call_stack: DebugCallStack,
    pub exception: MemphisException,
}

impl RaisedMemphisError {
    pub fn new(debug_call_stack: DebugCallStack, exception: MemphisException) -> Self {
        Self {
            debug_call_stack,
            exception,
        }
    }
}

impl Display for RaisedMemphisError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.debug_call_stack)?;
        write!(f, "{}", self.exception)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemphisException {
    pub kind: ExceptionKind,
    pub payload: Vec<MemphisValue>,
}

impl MemphisException {
    pub fn new(kind: ExceptionKind, payload: Vec<MemphisValue>) -> Self {
        Self { kind, payload }
    }
}

impl Display for MemphisException {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.kind.display_name())?;

        // If this exception does not contain a payload, we're done.
        if self.payload.is_empty() {
            return Ok(());
        } else {
            write!(f, ": ")?;
        }

        match self.kind {
            ExceptionKind::NameError => {
                if let Some(MemphisValue::Str(name)) = self.payload.first() {
                    write!(f, "name '{}' is not defined", name)
                } else {
                    write!(f, "name is not defined")
                }
            }
            ExceptionKind::KeyError => {
                if let Some(MemphisValue::Str(key)) = self.payload.first() {
                    write!(f, "'{}'", key)
                } else {
                    write!(f, "key is not defined")
                }
            }
            ExceptionKind::IndexError => {
                if let Some(MemphisValue::Str(msg)) = self.payload.first() {
                    write!(f, "{}", msg)
                } else {
                    write!(f, "index out of range")
                }
            }
            ExceptionKind::AttributeError => match (&self.payload.first(), &self.payload.get(1)) {
                (Some(MemphisValue::Str(obj)), Some(MemphisValue::Str(attr))) => {
                    write!(f, "'{}' object has no attribute '{}'", obj, attr)
                }
                _ => {
                    write!(f, "object has no attribute")
                }
            },
            ExceptionKind::SyntaxError | ExceptionKind::TypeError | ExceptionKind::ImportError => {
                if let Some(MemphisValue::Str(msg)) = self.payload.first() {
                    write!(f, "{}", msg)
                } else {
                    Ok(())
                }
            }
            _ => todo!(),
        }
    }
}
