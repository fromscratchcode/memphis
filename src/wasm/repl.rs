use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::{
    repl::{ReplOutput, ReplResult, ReplSession, ReplStep},
    Engine,
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
pub struct WasmReplOutput {
    pub stdout: String,
    pub result: WasmReplResult,
}

impl From<&ReplOutput> for WasmReplOutput {
    fn from(result: &ReplOutput) -> Self {
        WasmReplOutput {
            stdout: result.stdout.clone(),
            result: WasmReplResult::from(result.result.clone()),
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "type", content = "data", rename_all = "lowercase")]
pub enum WasmReplStep {
    Complete(WasmReplOutput),
    Incomplete(usize),
}

impl From<&ReplStep> for WasmReplStep {
    fn from(value: &ReplStep) -> Self {
        match value {
            ReplStep::Complete(output) => {
                let wasm_output = WasmReplOutput::from(output);
                WasmReplStep::Complete(wasm_output)
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
    pub fn new(engine_str: &str) -> WasmRepl {
        // We guard this using TypeScript
        let engine = Engine::try_from(engine_str).expect("Invalid engine.");

        WasmRepl {
            session: ReplSession::new(engine),
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
