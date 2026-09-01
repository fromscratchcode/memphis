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
        self.write(&format!("{text}\n"))
    }
}

pub trait HostIo: Input + Output {}

/// A host adapter for compiler-only contexts, which must not interact with external I/O.
#[cfg(any(test, feature = "wasm"))]
pub(crate) struct CompileIo;

#[cfg(any(test, feature = "wasm"))]
impl Input for CompileIo {
    fn input(&mut self, _prompt: &str) -> Result<InputResult, HostIoError> {
        Err(HostIoError {
            message: "input unavailable during compiler-only mode".to_string(),
        })
    }
}

#[cfg(any(test, feature = "wasm"))]
impl Output for CompileIo {
    fn write(&mut self, _text: &str) -> Result<(), HostIoError> {
        Err(HostIoError {
            message: "output unavailable during compiler-only mode".to_string(),
        })
    }
}

#[cfg(any(test, feature = "wasm"))]
impl HostIo for CompileIo {}
