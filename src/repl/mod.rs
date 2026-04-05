#[cfg(any(feature = "repl", feature = "wasm"))]
pub mod core;
#[cfg(any(feature = "repl", feature = "wasm"))]
mod parser;
#[cfg(feature = "repl")]
mod terminal;

#[cfg(feature = "repl")]
pub use terminal::TerminalRepl;
