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

    #[cfg(any(feature = "repl", feature = "wasm"))]
    pub fn is_unexpected_eof(&self) -> bool {
        matches!(
            self,
            ParserError::UnexpectedToken(Token::Eof) | ParserError::ExpectedToken(_, Token::Eof)
        )
    }

    #[cfg(any(feature = "repl", feature = "wasm"))]
    pub fn expected_token_at_eof(&self) -> Option<Token> {
        match self {
            ParserError::ExpectedToken(expected, Token::Eof) => Some(expected.clone()),
            _ => None,
        }
    }

    #[allow(dead_code)]
    pub fn debug_message(&self) -> String {
        match self {
            ParserError::ExpectedToken(expected, found) => {
                format!("Expected token {expected:?}, found {found:?}")
            }
            ParserError::UnexpectedToken(token) => {
                format!("Unexpected token \"{token:?}\"")
            }
            ParserError::SyntaxError(msg) => msg.clone(),
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ParserError::ExpectedToken(_, _) => {
                write!(f, "invalid syntax")
            }
            ParserError::UnexpectedToken(_) => {
                write!(f, "invalid syntax")
            }
            ParserError::SyntaxError(msg) => {
                write!(f, "{msg}")
            }
        }
    }
}
