use std::str::FromStr;

use js_sys::Function;
use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::{
    Engine,
    repl::{ReplResult, ReplSession, ReplStep},
    wasm::io::WasmStreamingIo,
};

#[derive(Serialize)]
#[serde(tag = "type", content = "value", rename_all = "lowercase")]
pub enum WasmReplResult {
    None,
    Ok(String),
    Err(String),
}

impl From<ReplResult> for WasmReplResult {
    fn from(result: ReplResult) -> Self {
        match result {
            ReplResult::None => WasmReplResult::None,
            ReplResult::Ok(val) => WasmReplResult::Ok(val),
            ReplResult::Err(err) => WasmReplResult::Err(err),
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "type", content = "data", rename_all = "lowercase")]
pub enum WasmReplStep {
    Complete(WasmReplResult),
    Incomplete(usize),
}

impl From<&ReplStep> for WasmReplStep {
    fn from(value: &ReplStep) -> Self {
        match value {
            ReplStep::Complete(result) => {
                let wasm_result = WasmReplResult::from(result.clone());
                WasmReplStep::Complete(wasm_result)
            }
            ReplStep::Incomplete { indent } => WasmReplStep::Incomplete(*indent),
        }
    }
}

#[wasm_bindgen]
pub struct WasmRepl {
    session: ReplSession,
}

#[wasm_bindgen]
impl WasmRepl {
    #[wasm_bindgen(constructor)]
    pub fn new(engine_str: &str, on_stdout: &Function, on_input: &Function) -> WasmRepl {
        // We guard this using TypeScript
        let engine = Engine::from_str(engine_str).expect("Invalid engine.");

        let io = WasmStreamingIo::new(on_stdout.clone(), on_input.clone());
        WasmRepl {
            session: ReplSession::new(engine, io),
        }
    }

    #[wasm_bindgen]
    pub fn version(&self) -> String {
        self.session.version().to_string()
    }

    #[wasm_bindgen]
    pub fn engine(&self) -> String {
        self.session.engine().to_string()
    }

    #[wasm_bindgen]
    pub fn insert_text(&mut self, text: &str) {
        for c in text.chars() {
            self.session.insert(c);
        }
    }

    #[wasm_bindgen]
    pub fn backspace(&mut self) {
        self.session.backspace();
    }

    #[wasm_bindgen]
    pub fn move_left(&mut self) {
        self.session.move_left();
    }

    #[wasm_bindgen]
    pub fn move_right(&mut self) {
        self.session.move_right();
    }

    #[wasm_bindgen]
    pub fn history_up(&mut self) {
        self.session.history_up();
    }

    #[wasm_bindgen]
    pub fn history_down(&mut self) {
        self.session.history_down();
    }

    #[wasm_bindgen]
    pub fn submit(&mut self) -> JsValue {
        let step = self.session.submit();
        let output = WasmReplStep::from(step);
        serde_wasm_bindgen::to_value(&output).expect("Bad WasmReplOutput")
    }

    #[wasm_bindgen]
    pub fn interrupt(&mut self) {
        self.session.interrupt();
    }

    #[wasm_bindgen]
    pub fn prompt(&self) -> String {
        self.session.prompt().to_string()
    }

    #[wasm_bindgen]
    pub fn current_line(&self) -> String {
        self.session.current_line().to_string()
    }

    #[wasm_bindgen]
    pub fn cursor_index(&self) -> usize {
        self.session.cursor_index()
    }
}
