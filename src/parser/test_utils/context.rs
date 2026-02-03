use crate::{
    domain::Text,
    lexer::Lexer,
    parser::{types::Ast, Parser, ParserError},
};

pub struct ParseContext {
    lexer: Lexer,
}

impl ParseContext {
    pub fn new(text: &Text) -> Self {
        Self {
            lexer: Lexer::new(&text),
        }
    }

    /// This cannot be used for multiple parse calls.
    pub fn parse_oneshot(&mut self) -> Result<Ast, ParserError> {
        let mut parser = Parser::new(&mut self.lexer);
        parser.parse()
    }
}
