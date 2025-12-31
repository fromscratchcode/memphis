use std::fmt::{Display, Error, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
    UnexpectedCharacter(char),
    InternalError(String),
    InvalidToken(String),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LexerError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {c}"),
            LexerError::InvalidToken(t) => write!(f, "Invalid token: {t}"),
            LexerError::InternalError(msg) => write!(f, "Internal Error: {msg}"),
        }
    }
}

pub type LexerResult<T> = Result<T, LexerError>;
