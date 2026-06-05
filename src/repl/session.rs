use crate::{
    repl::{ReplCore, ReplStep},
    Engine,
};

const INDENT_WIDTH: usize = 4;

pub struct ReplSession {
    core: ReplCore,

    last_step: ReplStep,

    /// The current line being manipulated by the user.
    current_line: String,

    /// The current cursor position on the current line. This _excludes_ the prompt.
    cursor_index: usize,

    /// A list of all the lines (_not_ statements) recording during this REPL session.
    history: Vec<String>,

    /// If Up/Down has been pressed, the index in `history` the user is currently selecting.
    history_index: Option<usize>,
}

impl ReplSession {
    pub fn new(engine: Engine) -> Self {
        Self {
            core: ReplCore::new(engine),
            last_step: ReplStep::initial(),
            current_line: String::new(),
            cursor_index: 0,
            history: Vec::new(),
            history_index: None,
        }
    }

    pub fn version(&self) -> &'static str {
        env!("CARGO_PKG_VERSION")
    }

    pub fn engine(&self) -> &Engine {
        self.core.engine()
    }

    pub fn insert(&mut self, c: char) {
        self.current_line.insert(self.cursor_index, c);
        self.cursor_index += 1;
    }

    pub fn backspace(&mut self) {
        if self.cursor_index > 0 {
            self.cursor_index -= 1;
            self.current_line.remove(self.cursor_index);
        }
    }

    pub fn move_left(&mut self) {
        if self.cursor_index > 0 {
            self.cursor_index -= 1;
        }
    }

    pub fn move_right(&mut self) {
        if self.cursor_index < self.current_line.len() {
            self.cursor_index += 1;
        }
    }

    pub fn history_up(&mut self) {
        if let Some(index) = self.history_index {
            if index > 0 {
                self.history_index = Some(index - 1);
            }
        } else if !self.history.is_empty() {
            self.history_index = Some(self.history.len() - 1);
        }

        if let Some(index) = self.history_index {
            self.current_line = self.history[index].clone();
            self.cursor_index = self.current_line.len();
        }
    }

    pub fn history_down(&mut self) {
        if let Some(index) = self.history_index {
            if index < self.history.len() - 1 {
                self.history_index = Some(index + 1);
            } else {
                self.history_index = None;
                self.current_line.clear();
            }

            if let Some(index) = self.history_index {
                self.current_line = self.history[index].clone();
            } else {
                self.current_line.clear();
            }

            self.cursor_index = self.current_line.len();
        }
    }

    pub fn submit(&mut self) -> &ReplStep {
        self.history.push(self.current_line.clone());
        self.history_index = None;

        let line = format!("{}\n", self.current_line);
        self.last_step = self.core.input_line(&line);

        self.reset_input();
        &self.last_step
    }

    pub fn interrupt(&mut self) {
        self.core.reset();
        self.last_step = ReplStep::initial();
        self.reset_input();
    }

    /// Gives the indicator for the start of the given line, based on whether or not the most
    /// recent line provided by the user completed a statement or not.
    pub fn prompt(&self) -> &str {
        match self.last_step.is_complete() {
            true => ">>> ",
            false => "... ",
        }
    }

    pub fn current_line(&self) -> &str {
        &self.current_line
    }

    pub fn cursor_index(&self) -> usize {
        self.cursor_index
    }

    /// Clear the REPL prompt to prepare for user input.
    fn reset_input(&mut self) {
        self.current_line = " ".repeat(self.last_step.indent_level() * INDENT_WIDTH);
        self.cursor_index = self.current_line.len();
    }
}
