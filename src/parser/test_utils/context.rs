use crate::{
    domain::Text,
    lexer::Lexer,
    parser::{types::Ast, Parser, ParserError},
};

pub struct ParseContext {
    lexer: Lexer,
}

impl ParseContext {
    pub fn new() -> Self {
        Self {
            lexer: Lexer::script(),
        }
    }

    /// This cannot be used for multiple parse calls.
    pub fn parse_oneshot(&mut self, text: &Text) -> Result<Ast, ParserError> {
        self.lexer.add_text(text);
        let mut parser = Parser::new(&mut self.lexer);
        parser.parse()
    }
}
