use std::{panic, process};

use crossterm::{
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    terminal,
};

use crate::{
    repl::{ReplResult, ReplSession, ReplStep},
    Engine,
};

mod io;

use io::{CrosstermIO, TerminalIO};

enum ReplControl {
    Continue,
    Exit,
}

/// These are ergonomic helpers specific to the terminal REPL.
impl ReplStep {
    pub fn output(&self) -> Option<&ReplResult> {
        match self {
            ReplStep::Complete(output) => Some(&output.result),
            ReplStep::Incomplete { .. } => None,
        }
    }

    pub fn stdout(&self) -> Option<&str> {
        match self {
            ReplStep::Complete(output) => Some(&output.stdout),
            ReplStep::Incomplete { .. } => None,
        }
    }
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

/// The Memphis Read-Evaluate-Print-Loop (REPL).
pub struct TerminalRepl {
    session: ReplSession,
}

impl TerminalRepl {
    pub fn new(engine: Engine) -> Self {
        Self {
            session: ReplSession::new(engine),
        }
    }

    /// The primary entrypoint to the REPL, which uses a real terminal in raw mode and will exit
    /// loudly when terminated. For virtual terminals, use `run_inner`.
    pub fn start(&mut self) {
        let terminal_io = &mut CrosstermIO;
        let _ = terminal_io.writeln(format!(
            "memphis {} REPL (engine: {})",
            self.session.version(),
            self.session.engine()
        ));

        // Enable raw mode to handle individual keypresses. This must be disabled during all
        // expected or unexpected exits!
        install_custom_panic_hook();
        let _ = terminal::enable_raw_mode();

        self.redraw(terminal_io);
        self.run_inner(terminal_io);

        let _ = terminal::disable_raw_mode();
        let _ = panic::take_hook();
    }

    fn run_inner<T: TerminalIO>(&mut self, terminal_io: &mut T) {
        loop {
            match terminal_io.read_event() {
                Ok(Event::Key(event)) => match self.handle_key_event(terminal_io, event) {
                    ReplControl::Continue => {}
                    ReplControl::Exit => break,
                },
                Ok(_) => {}
                Err(_) => break,
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
                self.session.interrupt();

                // CPython emits a `KeyboardInterrupt` here. We could do that and then
                // probably handle it one level up? That could help for the other
                // panics as well.
                let _ = terminal_io.enter();

                self.redraw(terminal_io);
                return ReplControl::Continue;
            }
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
                let _ = terminal_io.enter();
                return ReplControl::Exit;
            }
            _ => {}
        }

        match event.code {
            KeyCode::Char(c) => {
                self.session.insert(c);
            }
            KeyCode::Backspace => {
                self.session.backspace();
            }
            KeyCode::Enter => {
                let step = self.session.submit();
                // We must virtually hit Enter before processing the line so any results will be
                // displayed on the next line.
                let _ = terminal_io.enter();
                Self::handle_step(terminal_io, step);
            }
            KeyCode::Up => {
                self.session.history_up();
            }
            KeyCode::Down => {
                self.session.history_down();
            }
            KeyCode::Right => {
                self.session.move_right();
            }
            KeyCode::Left => {
                self.session.move_left();
            }
            _ => {}
        }

        self.redraw(terminal_io);
        ReplControl::Continue
    }

    /// Clear the current input, redraw it, and align the cursor to the proper column.
    fn redraw<T: TerminalIO>(&self, terminal_io: &mut T) {
        let rendered_line = format!("\r{}{}", self.session.prompt(), self.session.current_line());
        // The cursor position depends on where in the current input we are, not its length.
        let cursor_col = self.session.prompt().len() + self.session.cursor_index();
        let _ = terminal_io.redraw(rendered_line, cursor_col);
    }

    /// Append the provided line to the constructed statement and evaluate it.
    fn handle_step<T: TerminalIO>(terminal_io: &mut T, step: &ReplStep) {
        if let Some(stdout) = step.stdout() {
            let _ = terminal_io.write(stdout);
        }

        if let Some(result) = step.output() {
            match result {
                ReplResult::Ok(val) | ReplResult::Err(val) => {
                    let _ = terminal_io.writeln(val);
                }
                ReplResult::None => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fmt::Display, io};

    use super::*;

    fn run_inner(engine: Engine, terminal: &mut MockTerminalIO) -> String {
        TerminalRepl::new(engine).run_inner(terminal);
        terminal.return_val()
    }

    /// Run the complete flow, from input code string to return value string. If you need any Ctrl
    /// modifiers, do not use this!
    fn run(input: &str) -> String {
        let mut terminal = MockTerminalIO::from_str(input);
        run_inner(Engine::Treewalk, &mut terminal)
    }

    fn run_vm(input: &str) -> String {
        let mut terminal = MockTerminalIO::from_str(input);
        run_inner(Engine::BytecodeVm, &mut terminal)
    }

    fn run_events(events: Vec<Event>) -> String {
        let mut terminal = MockTerminalIO::new(events);
        run_inner(Engine::Treewalk, &mut terminal)
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
        ///
        /// End of the output will be similar to this, which is why we look for the 2nd to last
        /// element.
        ///
        /// "Traceback....NameError...\n",
        /// ">>> ",
        fn return_val(&self) -> String {
            self.output
                .len()
                .checked_sub(2)
                .and_then(|index| self.output.get(index))
                .expect("Not enough elements in output")
                .to_string()
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
            self.write(format!("{}\n", output))
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
        assert_eq!(return_val, "12345\n");
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
        assert_eq!(return_val, "20\n");
    }

    #[test]
    fn test_multiline_grouping() {
        let code = r#"
x = (1 +
2)
x
"#;
        let return_val = run(code);
        assert_eq!(return_val, "3\n");
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
        assert_eq!(return_val, "20\n");
    }

    #[test]
    fn test_repl_ctrl_c() {
        let mut events = string_to_events("123456789\n");
        let ctrl_c = Event::Key(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
        events.insert(4, ctrl_c);

        let return_val = run_events(events);
        assert_eq!(return_val, "56789\n");
    }

    #[test]
    fn test_repl_ctrl_d_does_not_panic() {
        let mut events = string_to_events("123");
        let ctrl_d = Event::Key(KeyEvent::new(KeyCode::Char('d'), KeyModifiers::CONTROL));
        events.push(ctrl_d);
        let return_val = run_events(events);
        // this isn't technically a return value, we never got one because we didn't hit enter \n
        assert_eq!(return_val, "\r>>> 123");
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
        assert_eq!(return_val, "46\n");
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
        assert_eq!(return_val, "10\n");
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
        assert_eq!(return_val, "10\n");
    }
}
