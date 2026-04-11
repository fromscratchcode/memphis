use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::{
    repl::core::{ReplCore, ReplResult, ReplStep},
    Engine,
};

#[derive(Serialize)]
#[serde(tag = "type", content = "data", rename_all = "lowercase")]
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

impl From<ReplStep> for WasmReplStep {
    fn from(value: ReplStep) -> Self {
        match value {
            ReplStep::Complete { result } => WasmReplStep::Complete(result.into()),
            ReplStep::Incomplete { indent } => WasmReplStep::Incomplete(indent),
        }
    }
}

#[wasm_bindgen]
pub struct WasmRepl {
    core: ReplCore,
}

#[wasm_bindgen]
impl WasmRepl {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmRepl {
        WasmRepl {
            core: ReplCore::new(Engine::Treewalk),
        }
    }

    #[wasm_bindgen]
    pub fn input_line(&mut self, line: &str) -> JsValue {
        let result = self.core.input_line(line);
        let output: WasmReplStep = result.into();
        serde_wasm_bindgen::to_value(&output).expect("Bad WasmReplOutput")
    }
}
