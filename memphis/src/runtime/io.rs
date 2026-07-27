use std::{
    collections::VecDeque,
    io::{self, Write},
};

pub enum IoError {
    Eof,
}

pub struct MemphisIo {
    stdin_lines: Option<VecDeque<String>>,
    stdout_capture: Option<Vec<u8>>,
}

impl MemphisIo {
    pub fn new() -> Self {
        Self {
            stdin_lines: None,
            stdout_capture: None,
        }
    }

    pub fn enable_capture(&mut self) {
        self.stdout_capture = Some(Vec::new());
    }

    pub fn take_output(&mut self) -> Option<String> {
        self.stdout_capture
            .take()
            .map(|b| String::from_utf8(b).unwrap())
    }

    pub fn println(&mut self, s: &str) {
        if let Some(buf) = &mut self.stdout_capture {
            writeln!(buf, "{}", s).unwrap();
        } else {
            println!("{}", s);
            io::stdout().flush().unwrap();
        }
    }

    pub fn set_input<I, S>(&mut self, lines: I)
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.stdin_lines = Some(lines.into_iter().map(Into::into).collect());
    }

    pub fn print(&mut self, s: &str) {
        if let Some(buf) = &mut self.stdout_capture {
            write!(buf, "{}", s).unwrap();
        } else {
            print!("{}", s);
            io::stdout().flush().unwrap();
        }
    }

    pub fn input(&mut self) -> Result<String, IoError> {
        let mut input = if let Some(lines) = &mut self.stdin_lines {
            match lines.pop_front() {
                Some(line) => line,
                None => return Err(IoError::Eof),
            }
        } else {
            let mut input = String::new();
            let num_bytes = io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");
            if num_bytes == 0 {
                return Err(IoError::Eof);
            }
            input
        };

        while input.ends_with(['\n', '\r']) {
            input.pop();
        }

        Ok(input)
    }
}
