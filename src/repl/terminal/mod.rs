use std::{panic, process};

use crossterm::{
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    terminal,
};

use crate::{
    repl::core::{ReplCore, ReplResult, ReplStep},
    Engine,
};

mod io;

use io::{CrosstermIO, TerminalIO};

type ExitCode = i32;

enum ReplControl {
    Continue,
    Exit(ExitCode),
}

/// Install a panic hook to ensure raw mode is disabled on panic.
fn install_custom_panic_hook() {
    panic::set_hook(Box::new(|info| {
        // This line is critical!! The rest of this function is just debug info, but without this
        // line, your shell will become unusable on an unexpected panic.
        let _ = terminal::disable_raw_mode();

        if let Some(s) = info.payload().downcast_ref::<&str>() {
            eprintln!("\nPanic: {s:?}");
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            eprintln!("\nPanic: {s:?}");
        } else {
            eprintln!("\nPanic occurred!");
        }

        if let Some(location) = info.location() {
            eprintln!(
                "  in file '{}' at line {}",
                location.file(),
                location.line()
            );
        } else {
            eprintln!("  in an unknown location.");
        }

        process::exit(1);
    }));
}

const INDENT_WIDTH: usize = 4;

/// The Memphis Read-Evaluate-Print-Loop (REPL).
pub struct TerminalRepl {
    engine: Engine,

    core: ReplCore,

    last_step: ReplStep,

    /// The current line being manipulated by the user.
    line: String,

    /// The current cursor position on the current line. This _excludes_ the `marker`.
    line_index: usize,

    /// A list of all the lines (_not_ statements) recording during this REPL session.
    history: Vec<String>,

    /// If Up/Down has been pressed, the index in `history` the user is currently selecting.
    history_index: Option<usize>,
}

impl TerminalRepl {
    pub fn new(engine: Engine) -> Self {
        Self {
            engine,
            core: ReplCore::new(engine),
            last_step: ReplStep::initial(),
            line: String::new(),
            line_index: 0,
            history: Vec::new(),
            history_index: None,
        }
    }

    /// The primary entrypoint to the REPL, which uses a real terminal in raw mode and will exit
    /// loudly when terminated. For virtual terminals, use `run_inner`.
    pub fn start(&mut self) {
        let terminal_io = &mut CrosstermIO;
        let _ = terminal_io.writeln(format!(
            "memphis {} REPL (engine: {}) (Type 'exit()' to quit)",
            env!("CARGO_PKG_VERSION"),
            self.engine
        ));

        // Enable raw mode to handle individual keypresses. This must be disabled during all
        // expected or unexpected exits!
        install_custom_panic_hook();
        let _ = terminal::enable_raw_mode();

        self.redraw(terminal_io);
        let exit_code = self.run_inner(terminal_io);

        let _ = terminal::disable_raw_mode();
        let _ = panic::take_hook();

        process::exit(exit_code);
    }

    fn run_inner<T: TerminalIO>(&mut self, terminal_io: &mut T) -> ExitCode {
        loop {
            match terminal_io.read_event() {
                Ok(Event::Key(event)) => match self.handle_key_event(terminal_io, event) {
                    ReplControl::Continue => {}
                    ReplControl::Exit(code) => break code,
                },
                Ok(_) => {}
                Err(_) => break 1,
            }
        }
    }

    /// Update the terminal and interpreter state based on the given `KeyEvent`.
    fn handle_key_event<T: TerminalIO>(
        &mut self,
        terminal_io: &mut T,
        event: KeyEvent,
    ) -> ReplControl {
        match (event.modifiers, event.code) {
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => {
                // CPython emits a `KeyboardInterrupt` here. We could do that and then
                // probably handle it one level up? That could help for the other
                // panics as well.
                let _ = terminal_io.enter();

                self.core.reset();
                self.last_step = ReplStep::initial();

                self.reset_input();
                self.redraw(terminal_io);
                return ReplControl::Continue;
            }
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
                let _ = terminal_io.enter();
                return ReplControl::Exit(0);
            }
            _ => {}
        }

        match event.code {
            KeyCode::Char(c) => {
                self.line.insert(self.line_index, c);
                self.line_index += 1;
            }
            KeyCode::Backspace => {
                if self.line_index > 0 {
                    self.line_index -= 1;
                    self.line.remove(self.line_index);
                }
            }
            KeyCode::Enter => {
                self.history.push(self.line.clone());
                self.history_index = None;

                let line = format!("{}\n", self.line);

                // We must virtually hit Enter before processing the line so any results will be
                // displayed on the next line.
                let _ = terminal_io.enter();
                let control = self.process_line(terminal_io, &line);
                if matches!(control, ReplControl::Exit(_)) {
                    return control;
                }

                self.reset_input();
            }
            KeyCode::Up => {
                if let Some(index) = self.history_index {
                    if index > 0 {
                        self.history_index = Some(index - 1);
                    }
                } else if !self.history.is_empty() {
                    self.history_index = Some(self.history.len() - 1);
                }

                if let Some(index) = self.history_index {
                    self.line = self.history[index].clone();
                    self.line_index = self.line.len();
                }
            }
            KeyCode::Down => {
                if let Some(index) = self.history_index {
                    if index < self.history.len() - 1 {
                        self.history_index = Some(index + 1);
                    } else {
                        self.history_index = None;
                        self.line.clear();
                    }

                    if let Some(index) = self.history_index {
                        self.line = self.history[index].clone();
                    } else {
                        self.line.clear();
                    }

                    self.line_index = self.line.len();
                }
            }
            KeyCode::Right => {
                if self.line_index < self.line.len() {
                    self.line_index += 1;
                }
            }
            KeyCode::Left => {
                if self.line_index > 0 {
                    self.line_index -= 1;
                }
            }
            _ => {}
        }

        self.redraw(terminal_io);
        ReplControl::Continue
    }

    /// Gives the indicator for the start of the given line, based on whether or not the most
    /// recent line provided by the user completed a statement or not.
    fn prompt(&self) -> &str {
        match self.last_step.is_complete() {
            true => ">>> ",
            false => "... ",
        }
    }

    /// Clear the REPL prompt to prepare for user input.
    fn reset_input(&mut self) {
        self.line = " ".repeat(self.last_step.indent_level() * INDENT_WIDTH);
        self.line_index = self.line.len();
    }

    /// Clear the current input, redraw it, and align the cursor to the proper column.
    fn redraw<T: TerminalIO>(&self, terminal_io: &mut T) {
        let output = format!("\r{}{}", self.prompt(), self.line);
        // The cursor position depends on where in the current input we are, not its length.
        let cursor_col = self.line_index + self.prompt().len();
        let _ = terminal_io.redraw(output, cursor_col);
    }

    /// Append the provided line to the constructed statement and evaluate it.
    fn process_line<T: TerminalIO>(&mut self, terminal_io: &mut T, line: &str) -> ReplControl {
        if line.trim_end() == "exit()" {
            return ReplControl::Exit(0);
        }

        self.last_step = self.core.input_line(line);

        match &self.last_step {
            ReplStep::Complete { result } => match result {
                ReplResult::Ok(val) => {
                    let _ = terminal_io.writeln(val);
                }
                ReplResult::Err(err) => {
                    let _ = terminal_io.writeln(err);
                }
                ReplResult::None => {}
            },
            ReplStep::Incomplete { .. } => {}
        };

        ReplControl::Continue
    }
}

#[cfg(test)]
mod tests {
    use std::{fmt::Display, io};

    use super::*;

    fn run_inner(engine: Engine, terminal: &mut MockTerminalIO) -> (ExitCode, String) {
        let exit_code = TerminalRepl::new(engine).run_inner(terminal);
        (exit_code, terminal.return_val())
    }

    /// Run the complete flow, from input code string to return value string. If you need any Ctrl
    /// modifiers, do not use this!
    fn run(input: &str) -> String {
        let mut terminal = MockTerminalIO::from_str(input);
        let (_, return_val) = run_inner(Engine::Treewalk, &mut terminal);
        return_val
    }

    fn run_vm(input: &str) -> String {
        let mut terminal = MockTerminalIO::from_str(input);
        let (_, return_val) = run_inner(Engine::BytecodeVm, &mut terminal);
        return_val
    }

    fn run_events(events: Vec<Event>) -> String {
        let mut terminal = MockTerminalIO::new(events);
        let (_, return_val) = run_inner(Engine::Treewalk, &mut terminal);
        return_val
    }

    fn run_and_exit_code(events: Vec<Event>) -> ExitCode {
        let mut terminal = MockTerminalIO::new(events);
        let (exit_code, _) = run_inner(Engine::Treewalk, &mut terminal);
        exit_code
    }

    fn string_to_events(input: &str) -> Vec<Event> {
        input
            .chars()
            .map(|c| {
                let key_code = match c {
                    '\n' => KeyCode::Enter,
                    _ => KeyCode::Char(c),
                };
                Event::Key(KeyEvent::new(key_code, KeyModifiers::NONE))
            })
            .collect()
    }

    /// A mock for testing that doesn't use `crossterm`.
    struct MockTerminalIO {
        /// Predefined events for testing
        events: Vec<Event>,

        /// Captured output for assertions
        output: Vec<String>,
    }

    impl MockTerminalIO {
        fn new(events: Vec<Event>) -> Self {
            Self {
                events,
                output: vec![],
            }
        }

        fn from_str(input: &str) -> Self {
            Self {
                events: string_to_events(input),
                output: vec![],
            }
        }

        /// For a populated `MockTerminalIO`, fetch the last return value.
        fn return_val(&self) -> String {
            // End of the output will be similar to this, which is why we look for the 3rd to last
            // element.
            //
            // "Traceback....NameError...",
            // "\n",
            // ">>> ",
            let third_from_last = self
                .output
                .len()
                .checked_sub(3)
                .and_then(|index| self.output.get(index))
                .expect("Not enough elements in output");

            third_from_last.to_string()
        }
    }

    impl TerminalIO for MockTerminalIO {
        fn read_event(&mut self) -> Result<Event, io::Error> {
            if self.events.is_empty() {
                Err(io::Error::new(io::ErrorKind::Other, "No more events"))
            } else {
                // remove from the front (semantically similar to VecDequeue::pop_front).
                Ok(self.events.remove(0))
            }
        }

        fn write<T: Display>(&mut self, output: T) -> io::Result<()> {
            self.output.push(format!("{}", output));
            Ok(())
        }

        fn writeln<T: Display>(&mut self, output: T) -> io::Result<()> {
            self.write(output)?;
            self.write("\n")
        }

        fn redraw<T: Display>(&mut self, output: T, _col: usize) -> io::Result<()> {
            self.write(output)
        }
    }

    #[test]
    fn test_repl_name_error() {
        let return_val = run("e\n");
        assert!(return_val.contains("NameError: name 'e' is not defined"));
    }

    #[test]
    fn test_repl_name_error_vm() {
        let return_val = run_vm("e\n");
        assert!(return_val.contains("NameError: name 'e' is not defined"));
    }

    #[test]
    fn test_repl_expr() {
        let return_val = run("12345\n");
        assert_eq!(return_val, "12345");
    }

    #[test]
    fn test_repl_statement() {
        let return_val = run("a = 5.5\n");

        // empty string because a statement does not have a return value
        assert_eq!(return_val, "");
    }

    #[test]
    fn test_repl_function() {
        let code = r#"
def foo():
    a = 10
    return 2 * a

foo()
"#;
        let return_val = run(code);
        assert_eq!(return_val, "20");
    }

    #[test]
    fn test_multiline_grouping() {
        let code = r#"
x = (1 +
2)
x
"#;
        let return_val = run(code);
        assert_eq!(return_val, "3");
    }

    #[test]
    fn test_repl_function_vm() {
        let code = r#"
def foo():
    a = 10
    return 2 * a

foo()
"#;
        // TODO should we test all of these through both engines? Not sure yet, that may be too
        // deep of a test for this file.
        let return_val = run_vm(code);
        assert_eq!(return_val, "20");
    }

    #[test]
    fn test_repl_ctrl_c() {
        let mut events = string_to_events("123456789\n");
        let ctrl_c = Event::Key(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
        events.insert(4, ctrl_c);

        let return_val = run_events(events);
        assert_eq!(return_val, "56789");
    }

    #[test]
    fn test_repl_ctrl_d() {
        let mut events = string_to_events("123");
        let ctrl_d = Event::Key(KeyEvent::new(KeyCode::Char('d'), KeyModifiers::CONTROL));
        events.push(ctrl_d);
        let exit_code = run_and_exit_code(events);
        assert_eq!(exit_code, 0);
    }

    #[test]
    fn test_repl_exit_function() {
        let events = string_to_events("exit()\n");
        let exit_code = run_and_exit_code(events);
        assert_eq!(exit_code, 0);
    }

    #[test]
    fn test_repl_exit_function_ignores_prior_errors() {
        let events = string_to_events("undefined_var\nexit()\n");
        let exit_code = run_and_exit_code(events);
        assert_eq!(exit_code, 0);
    }

    #[test]
    fn test_function_call_with_long_body() {
        let code = r#"
def foo():
    a = 10
    b = 11
    c = 12
    d = 13
    return a + b + c + d

foo()
"#;
        let return_val = run(code);
        assert_eq!(return_val, "46");
    }

    #[test]
    fn test_treewalk_last_returned_val() {
        let code = r#"
a = 10
b = 12
b
a
"#;
        let return_val = run(code);
        assert_eq!(return_val, "10");
    }

    #[test]
    fn test_vm_last_returned_val() {
        // We had a bug here where this would previously return 12.
        let code = r#"
a = 10
b = 12
b
a
"#;
        let return_val = run_vm(code);
        assert_eq!(return_val, "10");
    }
}
