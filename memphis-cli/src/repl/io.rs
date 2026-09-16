use crossterm::terminal;
use memphis::{HostIo, HostIoError, Input, InputResult, Output};

use crate::{repl::driver::normalize_for_terminal, system_io::SystemIo};

pub struct TerminalReplIo {
    system_io: SystemIo,
}

impl TerminalReplIo {
    pub fn new() -> Self {
        Self {
            system_io: SystemIo,
        }
    }
}

impl Input for TerminalReplIo {
    fn input(&mut self, prompt: &str) -> Result<InputResult, HostIoError> {
        let raw_mode_needs_disabling =
            terminal::is_raw_mode_enabled().map_err(|e| HostIoError {
                message: e.to_string(),
            })?;

        if raw_mode_needs_disabling {
            terminal::disable_raw_mode().map_err(|e| HostIoError {
                message: e.to_string(),
            })?;
        }

        // We must re-enable raw mode whether or not we hit an error result, so don't return any
        // errors immediately.
        let result = self.system_io.input(prompt);

        if raw_mode_needs_disabling {
            terminal::enable_raw_mode().map_err(|e| HostIoError {
                message: e.to_string(),
            })?;
        }

        result
    }
}

impl Output for TerminalReplIo {
    fn write(&mut self, text: &str) -> Result<(), HostIoError> {
        // We can write directly to the terminal, but we must add carriage returns because we are
        // likely in raw mode.
        self.system_io.write(&normalize_for_terminal(text))
    }
}

impl HostIo for TerminalReplIo {}
