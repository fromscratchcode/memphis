mod error;
#[allow(clippy::module_inception)]
mod lexer;
mod multiline;
mod token;

pub use error::{LexerError, LexerResult};
pub use lexer::Lexer;
pub use multiline::MultilineString;
pub use token::Token;
