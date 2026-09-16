use std::{
    fmt::Display,
    io::{self, Write},
};

use crossterm::{
    cursor::MoveToColumn,
    event::{self, Event},
    execute,
    terminal::{self, Clear, ClearType},
};

pub trait TerminalDriver {
    fn read_event(&mut self) -> io::Result<Event>;
    fn write<T: Display>(&mut self, output: T) -> io::Result<()>;
    fn redraw<T: Display>(&mut self, output: T, col: usize) -> io::Result<()>;
    fn writeln<T: Display>(&mut self, output: T) -> io::Result<()> {
        self.write(format!("{output}\n"))
    }
    fn enter(&mut self) -> io::Result<()> {
        self.writeln("")
    }
}

pub struct CrosstermDriver;

impl TerminalDriver for CrosstermDriver {
    /// Use `crossterm` to read events
    fn read_event(&mut self) -> io::Result<Event> {
        event::read()
    }

    /// Emit output to stdout, normalizing for any needed carriage returns
    fn write<T: Display>(&mut self, output: T) -> io::Result<()> {
        print!("{}", normalize_for_terminal(output));
        io::stdout().flush()
    }

    fn redraw<T: Display>(&mut self, output: T, col: usize) -> io::Result<()> {
        // Redraw
        execute!(io::stdout(), Clear(ClearType::CurrentLine))?;
        let _ = self.write(output);

        // Position
        let cursor_col = col as u16;
        execute!(io::stdout(), MoveToColumn(cursor_col))
    }
}

/// When the terminal is in raw mode, we must emit a carriage return in addition to a newline,
/// because that does not happen automatically.
pub fn normalize_for_terminal<T: Display>(err: T) -> String {
    let formatted = format!("{err}");
    if terminal::is_raw_mode_enabled().expect("Failed to query terminal raw mode") {
        formatted.replace("\n", "\n\r")
    } else {
        formatted.to_string()
    }
}
