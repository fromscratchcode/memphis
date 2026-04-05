use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::{
    repl::core::{ReplCore, ReplResult},
    Engine,
};

#[derive(Serialize)]
#[serde(tag = "type", content = "value")]
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
    is_complete: bool,
    indent_level: usize,
    result: WasmReplResult,
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
        let output = WasmReplOutput {
            result: result.result.into(),
            is_complete: result.is_complete,
            indent_level: result.indent_level,
        };
        serde_wasm_bindgen::to_value(&output).expect("Bad WasmReplOutput")
    }
}
