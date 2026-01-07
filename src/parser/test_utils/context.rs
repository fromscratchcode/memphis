use crate::{
    domain::Text,
    lexer::Lexer,
    parser::{
        types::{ast, Ast, ParseNode},
        Parser, ParserError,
    },
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

    /// Parse a single [`ParseNode`]. This cannot be used for multiple parse calls.
    pub fn parse_oneshot<T>(&mut self) -> Result<T, ParserError>
    where
        T: ParseNode,
    {
        let parser = self.init_parser();
        T::parse_oneshot(parser)
    }

    pub fn parse_all(&mut self) -> Result<Ast, ParserError> {
        let mut parser = self.init_parser();
        let mut statements = ast![];

        while !parser.is_finished() {
            statements.push(parser.parse_statement()?);
        }

        Ok(statements)
    }

    pub fn init_parser(&mut self) -> Parser<'_> {
        Parser::new(&mut self.lexer)
    }
}
