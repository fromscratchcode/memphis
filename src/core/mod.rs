mod container;
mod log;
pub mod net;
mod utils;

pub use container::Container;
pub use log::{log, log_impure, LogLevel};
pub use utils::floats_equal;

use crate::domain::{MemphisResult, MemphisValue};

pub trait Interpreter {
    fn run(&mut self) -> MemphisResult<MemphisValue>;
    fn read(&mut self, name: &str) -> Option<MemphisValue>;
    fn add_line(&mut self, line: &str);
}

pub mod memphis_utils {
    use std::process;

    use crate::domain::RaisedMemphisError;

    /// The primary exit point from the interpreter. This should be used sparingly: either at the
    /// top level of the `Memphis` runtime or inline in select cases where a hard interface
    /// constraits the caller from propagating an error upwards.
    pub fn exit(err: RaisedMemphisError) -> ! {
        eprintln!("{err}");
        process::exit(1);
    }
}
