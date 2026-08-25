use crate::{Capture, Container, HostIo, HostIoError, Input, InputResult, Output};

pub struct WasmIo {
    output: Container<Capture>,
}

impl WasmIo {
    pub fn new() -> (Self, Container<Capture>) {
        let capture = Container::new(Capture::new());
        let io = Self {
            output: capture.clone(),
        };
        (io, capture)
    }
}

impl Input for WasmIo {
    fn input(&mut self, _prompt: &str) -> Result<InputResult, HostIoError> {
        unimplemented!()
    }
}

impl Output for WasmIo {
    fn write(&mut self, text: &str) -> Result<(), HostIoError> {
        self.output.borrow_mut().append(text);
        Ok(())
    }
}

impl HostIo for WasmIo {}
