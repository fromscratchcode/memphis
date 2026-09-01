pub struct Capture {
    output: String,
}

impl Capture {
    pub fn new() -> Self {
        Self {
            output: String::new(),
        }
    }

    pub fn append(&mut self, text: &str) {
        self.output.push_str(text);
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }
}

impl Default for Capture {
    fn default() -> Self {
        Self::new()
    }
}
