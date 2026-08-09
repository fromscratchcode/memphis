macro_rules! expect_error {
    ($input:expr) => {
        match $crate::parser::Parser::parse_text(&$crate::domain::Text::new($input)) {
            Ok(_) => panic!("Expected a ParserError!"),
            Err(e) => e,
        }
    };
}

macro_rules! parse {
    ($input:expr) => {
        match $crate::parser::Parser::parse_text(&$crate::domain::Text::new($input)) {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => ast,
        }
    };
}

pub(crate) use expect_error;
pub(crate) use parse;
