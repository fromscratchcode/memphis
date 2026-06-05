use std::collections::VecDeque;

use crate::lexer::{Lexer, Token};

pub struct TokenBuffer<'a> {
    lexer: &'a mut Lexer,
    buffer: VecDeque<Token>,
}

impl<'a> TokenBuffer<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Self {
            lexer,
            buffer: VecDeque::new(),
        }
    }

    /// Ensure the buffer has at least `n + 1` tokens
    fn fill_to(&mut self, n: usize) {
        while self.buffer.len() <= n {
            if let Some(tok) = self.lexer.next() {
                self.buffer.push_back(tok);
            } else {
                break;
            }
        }
    }

    pub fn peek(&mut self, ahead: usize) -> &Token {
        self.fill_to(ahead);
        self.buffer.get(ahead).unwrap_or(&Token::Eof)
    }

    pub fn consume(&mut self) -> Option<Token> {
        self.fill_to(0);
        self.buffer.pop_front()
    }

    pub fn peek_ahead_contains(&mut self, tokens: &[Token]) -> bool {
        for (i, tok) in tokens.iter().enumerate() {
            if self.peek(i) != tok {
                return false;
            }
        }
        true
    }
}
