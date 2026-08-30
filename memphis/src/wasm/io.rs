use wasm_bindgen::JsValue;

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

pub struct WasmStreamingIo {
    on_stdout: js_sys::Function,
}

impl WasmStreamingIo {
    pub fn new(on_stdout: js_sys::Function) -> Self {
        Self { on_stdout }
    }
}

impl Input for WasmStreamingIo {
    fn input(&mut self, _prompt: &str) -> Result<InputResult, HostIoError> {
        unimplemented!()
    }
}

impl Output for WasmStreamingIo {
    fn write(&mut self, text: &str) -> Result<(), HostIoError> {
        self.on_stdout
            .call1(&JsValue::UNDEFINED, &JsValue::from_str(text))
            .map_err(|e| HostIoError {
                message: format!("output callback failed: {e:?}"),
            })?;
        Ok(())
    }
}

impl HostIo for WasmStreamingIo {}
