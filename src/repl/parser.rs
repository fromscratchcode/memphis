use crate::{
    domain::Text,
    lexer::{Lexer, LexerMode, Token},
    parser::{Parser, ParserError},
};

pub enum ParseStep {
    Complete,
    Incomplete { indent: usize },
    Error,
}

pub fn analyze(text: &Text) -> ParseStep {
    let mut lexer = Lexer::new(LexerMode::Interactive);
    lexer.add_text(text);

    let parser = Parser::new(&mut lexer);
    let parse_status = check_complete(parser);
    let base_indent = lexer.indent_level();

    match parse_status {
        ParseStatus::Incomplete(IncompleteReason::NeedsIndent) => ParseStep::Incomplete {
            indent: base_indent + 1,
        },
        ParseStatus::Incomplete(_) => ParseStep::Incomplete {
            indent: base_indent,
        },
        ParseStatus::Complete => ParseStep::Complete,
        ParseStatus::Error => ParseStep::Error,
    }
}

#[derive(Debug, PartialEq)]
enum IncompleteReason {
    NeedsMoreTokens,
    NeedsIndent,
}

#[derive(Debug)]
enum ParseStatus {
    Complete,
    Incomplete(IncompleteReason),
    Error,
}

fn check_complete(mut parser: Parser) -> ParseStatus {
    match parser.parse() {
        Ok(_) => ParseStatus::Complete,
        Err(e) if is_unexpected_eof(&e) => match expected_token_at_eof(&e) {
            // If we detected incomplete input, see if we are starting a new block or not. The
            // other case is inside delimiters.
            Some(Token::Indent) => ParseStatus::Incomplete(IncompleteReason::NeedsIndent),
            _ => ParseStatus::Incomplete(IncompleteReason::NeedsMoreTokens),
        },
        Err(_) => ParseStatus::Error,
    }
}

fn is_unexpected_eof(err: &ParserError) -> bool {
    matches!(
        err,
        ParserError::UnexpectedToken(Token::Eof) | ParserError::ExpectedToken(_, Token::Eof)
    )
}

fn expected_token_at_eof(err: &ParserError) -> Option<Token> {
    match err {
        ParserError::ExpectedToken(expected, Token::Eof) => Some(expected.clone()),
        _ => None,
    }
}
