use js_sys::Function;
use wasm_bindgen::JsValue;

use crate::{HostIo, HostIoError, Input, InputResult, Output};

pub struct WasmStreamingIo {
    on_stdout: Function,
    on_input: Function,
}

impl WasmStreamingIo {
    pub fn new(on_stdout: Function, on_input: Function) -> Self {
        Self {
            on_stdout,
            on_input,
        }
    }
}

impl Input for WasmStreamingIo {
    fn input(&mut self, prompt: &str) -> Result<InputResult, HostIoError> {
        let result = self
            .on_input
            .call1(&JsValue::UNDEFINED, &JsValue::from_str(prompt))
            .map_err(|e| HostIoError {
                message: format!("input callback failed: {e:?}"),
            })?;

        match result {
            value if value.is_null() => Ok(InputResult::Eof),
            value => match value.as_string() {
                Some(line) => Ok(InputResult::Line(line)),
                None => panic!("input must return a string or null"),
            },
        }
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
