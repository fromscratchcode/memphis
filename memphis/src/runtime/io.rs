use std::io::Write;

pub struct MemphisIo {
    stdout_capture: Option<Vec<u8>>,
}

impl MemphisIo {
    pub fn new() -> Self {
        Self {
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

    pub fn print_line(&mut self, s: &str) {
        if let Some(buf) = &mut self.stdout_capture {
            writeln!(buf, "{}", s).unwrap();
        } else {
            println!("{}", s);
        }
    }
}
