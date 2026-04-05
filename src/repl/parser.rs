use crate::{
    domain::Text,
    lexer::{Lexer, Token},
    parser::Parser,
};

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

pub struct ReplParser {
    /// The usize here represents the number of Indent tokens, not the number of spaces. The number of
    /// spaces is left as a REPL presentation decision.
    indent_level: usize,

    last_parse_status: Option<ParseStatus>,
}

impl ReplParser {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            last_parse_status: None,
        }
    }

    pub fn analyze_text(&mut self, text: &Text) {
        let mut lexer = Lexer::interactive();
        lexer.add_text(text);

        let parser = Parser::new(&mut lexer);
        let parse_status = check_complete(parser);
        let base_indent = lexer.num_indents();

        self.indent_level = match parse_status {
            ParseStatus::Incomplete(IncompleteReason::NeedsIndent) => base_indent + 1,
            ParseStatus::Incomplete(_) => base_indent,
            ParseStatus::Complete => 0,
            ParseStatus::Error => 0,
        };

        self.last_parse_status = Some(parse_status);
    }

    pub fn indent_level(&self) -> usize {
        self.indent_level
    }
    pub fn is_incomplete(&self) -> bool {
        matches!(self.last_parse_status, Some(ParseStatus::Incomplete(_)))
    }
}

fn check_complete(mut parser: Parser) -> ParseStatus {
    match parser.parse() {
        Ok(_) => ParseStatus::Complete,
        Err(e) if e.is_unexpected_eof() => match e.expected_token_at_eof() {
            // If we detected incomplete input, see if we are starting a new block or not. The
            // other case is inside delimiters.
            Some(Token::Indent) => ParseStatus::Incomplete(IncompleteReason::NeedsIndent),
            _ => ParseStatus::Incomplete(IncompleteReason::NeedsMoreTokens),
        },
        Err(_) => ParseStatus::Error,
    }
}
