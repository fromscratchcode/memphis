use std::fmt::{Display, Error, Formatter};

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    ExpectedToken(Token, Token),
    UnexpectedToken(Token),
    SyntaxError(String),
}

impl ParserError {
    pub fn syntax_error(msg: impl Into<String>) -> Self {
        Self::SyntaxError(msg.into())
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ParserError::ExpectedToken(expected, found) => {
                write!(f, "Expected token {expected:?}, found {found:?}")
            }
            ParserError::UnexpectedToken(token) => {
                write!(f, "Unexpected token \"{token:?}\"")
            }
            ParserError::SyntaxError(msg) => {
                write!(f, "{msg}")
            }
        }
    }
}
