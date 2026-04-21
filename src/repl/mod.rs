#[cfg(feature = "interactive")]
mod core;
#[cfg(feature = "interactive")]
mod parser;
#[cfg(feature = "interactive")]
pub use core::{ReplCore, ReplOutput, ReplResult, ReplStep};

#[cfg(feature = "repl")]
mod terminal;
#[cfg(feature = "repl")]
pub use terminal::TerminalRepl;
