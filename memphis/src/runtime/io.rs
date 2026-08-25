#[derive(Debug)]
pub enum InputResult {
    Line(String),
    Eof,
}

#[derive(Debug)]
pub struct HostIoError {
    pub message: String,
}

pub trait Input {
    fn input(&mut self, prompt: &str) -> Result<InputResult, HostIoError>;
}

pub trait Output {
    fn write(&mut self, text: &str) -> Result<(), HostIoError>;
    fn writeln(&mut self, text: &str) -> Result<(), HostIoError> {
        self.write(text)?;
        self.write("\n")
    }
}

pub trait HostIo: Input + Output {}
