use std::collections::VecDeque;

use crate::{Capture, Container, HostIo, HostIoError, Input, InputResult, Output};

pub struct TestIo {
    input_lines: VecDeque<String>,
    output: Container<Capture>,
}

impl TestIo {
    pub fn new() -> (Self, Container<Capture>) {
        Self::with_input::<[_; 0], String>([])
    }

    pub fn with_input<I, S>(lines: I) -> (Self, Container<Capture>)
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let capture = Container::new(Capture::new());
        let io = Self {
            input_lines: lines.into_iter().map(Into::into).collect(),
            output: capture.clone(),
        };
        (io, capture)
    }
}

impl Input for TestIo {
    fn input(&mut self, prompt: &str) -> Result<InputResult, HostIoError> {
        self.output.borrow_mut().append(prompt);
        let mut input = match self.input_lines.pop_front() {
            Some(line) => line,
            None => return Ok(InputResult::Eof),
        };
        while input.ends_with(['\n', '\r']) {
            input.pop();
        }
        Ok(InputResult::Line(input))
    }
}

impl Output for TestIo {
    fn write(&mut self, text: &str) -> Result<(), HostIoError> {
        self.output.borrow_mut().append(text);
        Ok(())
    }
}

impl HostIo for TestIo {}
