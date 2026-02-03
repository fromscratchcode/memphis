use crate::{
    core::{log, LogLevel},
    domain::Identifier,
    lexer::{Lexer, Token},
    parser::{types::Ast, ParserError, TokenBuffer},
};

mod block;
mod expr;
mod import;
mod signature;
mod stmt;

/// A recursive-descent parser which attempts to encode the full Python grammar.
pub struct Parser<'a> {
    tokens: TokenBuffer<'a>,
    line_number: usize,
    delimiter_depth: usize,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Parser {
            tokens: TokenBuffer::new(lexer),
            line_number: 1,
            delimiter_depth: 0,
        }
    }

    /// Return the full AST. This will consume all the tokens.
    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        self.consume_newlines();

        let stmts = self.parse_statement_list_until(
            |tok| matches!(tok, Token::Eof),
            |tok| matches!(tok, Token::Newline | Token::Semicolon),
        )?;
        self.consume(&Token::Eof)?;
        // TODO is there a way to assert on our TokenBuffer that it's been exhausted? Doing this
        // right now would be a mutable operation which feels wrong.

        Ok(stmts)
    }

    pub fn consume_statement_separators(&mut self) {
        while self.is_statement_separator() {
            self.consume_current();
        }
    }

    pub fn consume_newlines(&mut self) {
        while self.current_token() == &Token::Newline {
            self.consume_current();
        }
    }

    pub fn is_finished(&mut self) -> bool {
        self.current_token() == &Token::Eof
    }

    // This must be mutable because we must refill the token buffer when empty.
    fn current_token(&mut self) -> &Token {
        self.tokens.peek(0)
    }

    fn end_of_statement(&mut self) -> bool {
        self.is_finished() || self.is_statement_separator()
    }

    fn is_statement_separator(&mut self) -> bool {
        matches!(self.current_token(), Token::Newline | Token::Semicolon)
    }

    fn inside_delimiter(&self) -> bool {
        self.delimiter_depth > 0
    }

    /// If we are inside a string literal, we must check for newline characters rather than
    /// tokens. These are produced by `Lexer::emit_newline`.
    fn advance_line_number_if_needed(&mut self) {
        if self.current_token() == &Token::Newline {
            self.line_number += 1;
        } else if let Token::StringLiteral(string) = &self.current_token() {
            self.line_number += string.matches('\n').count();
        } else if let Token::RawStringLiteral(string) = &self.current_token() {
            self.line_number += string.matches('\n').count();
        }
    }

    fn consume_current(&mut self) {
        let token = self.tokens.peek(0).clone();
        self.consume(&token)
            .expect("Consuming the current token should not fail.");
    }

    fn consume(&mut self, expected: &Token) -> Result<(), ParserError> {
        let current = self.tokens.peek(0);

        log(LogLevel::Trace, || format!("Token: {current:?}"));

        if current != expected {
            return Err(ParserError::ExpectedToken(
                expected.clone(),
                current.clone(),
            ));
        }

        if matches!(current, Token::LParen | Token::LBracket | Token::LBrace) {
            self.delimiter_depth += 1;
        } else if matches!(current, Token::RParen | Token::RBracket | Token::RBrace) {
            self.delimiter_depth -= 1;
        }

        self.advance_line_number_if_needed();

        self.tokens.consume();

        if self.inside_delimiter() {
            self.consume_newlines();
        }

        Ok(())
    }

    fn consume_optional(&mut self, expected: &Token) {
        if self.current_token() == expected {
            let _ = self.consume(expected);
        }
    }

    /// Parse a `Token::Identifier` without any semantic analysis.
    fn parse_identifier(&mut self) -> Result<Identifier, ParserError> {
        match self.current_token().clone() {
            Token::Identifier(ident) => {
                self.consume_current();
                Ok(ident)
            }
            _ => Err(ParserError::syntax_error("invalid identifier")),
        }
    }
}
