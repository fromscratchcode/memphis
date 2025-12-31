use std::fmt::{Display, Error, Formatter};

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    ExpectedToken(Token, Token),
    UnexpectedToken(Token),
    ExpectedException(String),
    SyntaxError,
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
            ParserError::ExpectedException(s) => {
                write!(f, "Expected exception: \"{s:?}\" is not defined")
            }
            ParserError::SyntaxError => {
                write!(f, "SyntaxError")
            }
        }
    }
}
