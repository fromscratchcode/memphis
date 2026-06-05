#[cfg(feature = "interactive")]
mod core;
#[cfg(feature = "interactive")]
mod parser;
#[cfg(feature = "interactive")]
mod session;
#[cfg(feature = "interactive")]
mod types;
#[cfg(feature = "interactive")]
pub use core::ReplCore;
#[cfg(feature = "interactive")]
pub use session::ReplSession;
#[cfg(feature = "interactive")]
pub use types::{ReplOutput, ReplResult, ReplStep};

#[cfg(feature = "repl")]
mod terminal;
#[cfg(feature = "repl")]
pub use terminal::TerminalRepl;
