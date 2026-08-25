use memphis::{
    Engine, HostIo, HostIoError, Input, InputResult, MemphisContext, ModuleOrigin, Output, Source,
};
use std::{
    io::{self, Write},
    path::Path,
    process,
};

use crate::terminal::TerminalRepl;

pub struct SystemIo;

impl HostIo for SystemIo {}

impl Output for SystemIo {
    fn write(&mut self, text: &str) -> Result<(), HostIoError> {
        io::stdout()
            .write_all(text.as_bytes())
            .map_err(|e| HostIoError {
                message: e.to_string(),
            })
    }
}

impl Input for SystemIo {
    fn input(&mut self, prompt: &str) -> Result<InputResult, HostIoError> {
        self.write(prompt)?;

        // If we do not flush, the user is not guaranteed to see the prompt before it waits for
        // input
        io::stdout().flush().map_err(|e| HostIoError {
            message: e.to_string(),
        })?;

        let mut input = String::new();
        let num_bytes = io::stdin().read_line(&mut input).map_err(|e| HostIoError {
            message: e.to_string(),
        })?;
        if num_bytes == 0 {
            return Ok(InputResult::Eof);
        }
        while input.ends_with(['\n', '\r']) {
            input.pop();
        }
        Ok(InputResult::Line(input))
    }
}

pub fn script(filepath: impl AsRef<Path>, engine: Engine) {
    let source = Source::from_path(filepath)
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        })
        .unwrap();
    let origin = ModuleOrigin::File(source.path().clone());
    let _ = MemphisContext::new(engine, origin, SystemIo)
        .eval(source.text().clone())
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        });
}

pub fn repl(engine: Engine) {
    TerminalRepl::new(engine).start();
}
