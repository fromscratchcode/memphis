use crate::domain::Text;

use super::ParseContext;

pub fn init(text: &str) -> ParseContext {
    ParseContext::new(&Text::new(text))
}

macro_rules! expect_error {
    ($input:expr) => {
        match init($input).parse_oneshot() {
            Ok(_) => panic!("Expected a ParserError!"),
            Err(e) => e,
        }
    };
}

macro_rules! parse {
    ($input:expr) => {
        match init($input).parse_oneshot() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => ast,
        }
    };
}

pub(crate) use expect_error;
pub(crate) use parse;
