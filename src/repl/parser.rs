use crate::{lexer::Token, parser::Parser};

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
        Err(e) if e.is_unexpected_eof() => match e.expected_token_at_eof() {
            // If we detected incomplete input, see if we are starting a new block or not. The
            // other case is inside delimiters.
            Some(Token::Indent) => ParseStatus::Incomplete(IncompleteReason::NeedsIndent),
            _ => ParseStatus::Incomplete(IncompleteReason::NeedsMoreTokens),
        },
        Err(_) => ParseStatus::Error,
    }
}

pub mod repl_parser {
    use crate::{domain::Text, lexer::Lexer, parser::Parser};

    use super::{check_complete, IncompleteReason, ParseStatus};

    pub enum ParseStep {
        Incomplete { indent: usize },
        Complete,
        Error,
    }

    pub fn analyze(text: &Text) -> ParseStep {
        let mut lexer = Lexer::interactive();
        lexer.add_text(text);

        let parser = Parser::new(&mut lexer);
        let parse_status = check_complete(parser);
        let base_indent = lexer.num_indents();

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
}
