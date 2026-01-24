use console_error_panic_hook::set_once;
use wasm_bindgen::prelude::*;

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Constant},
        CompilerError, VmContext,
    },
    domain::Text,
    Engine, MemphisContext,
};

// Export a function to JavaScript
#[wasm_bindgen]
pub fn greet() -> String {
    "Hello from WebAssembly!".to_string()
}

#[wasm_bindgen]
pub fn evaluate(code: String) -> String {
    // Set the panic hook for better error messages in the browser console
    set_once();

    let mut context = MemphisContext::from_text(Engine::Treewalk, Text::new(&code));
    let result = context.run().expect("Failed to evaluate.");
    format!("{result}")
}

use serde::Serialize;

#[derive(Serialize)]
#[serde(tag = "type", content = "value")] // For discriminating code objects
pub enum WasmConstant {
    None,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Code(WasmCodeObject),
}

impl From<Constant> for WasmConstant {
    fn from(c: Constant) -> Self {
        match c {
            Constant::None => WasmConstant::None,
            Constant::Boolean(b) => WasmConstant::Bool(b),
            Constant::Int(i) => WasmConstant::Int(i),
            Constant::Float(f) => WasmConstant::Float(f),
            Constant::String(s) => WasmConstant::String(s),
            Constant::Code(code) => {
                let wasm_code = WasmCodeObject::from(code);
                WasmConstant::Code(wasm_code)
            }
        }
    }
}

#[derive(Serialize)]
#[wasm_bindgen]
pub struct WasmCodeObject {
    name: String,
    bytecode: Vec<String>,
    varnames: Vec<String>,
    freevars: Vec<String>,
    names: Vec<String>,
    constants: Vec<WasmConstant>,
}

#[wasm_bindgen]
impl WasmCodeObject {
    fn from(code: CodeObject) -> Self {
        WasmCodeObject {
            name: code.name().to_string(),
            bytecode: code
                .bytecode
                .iter()
                .map(|op| op.display_annotated(&code))
                .collect(),
            varnames: code.varnames,
            freevars: code.freevars,
            names: code.names,
            constants: code.constants.iter().map(|c| c.to_owned().into()).collect(),
        }
    }
}

use std::fmt::{Display, Error, Formatter};

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Unsupported(msg) => write!(f, "Unsupported feature: {msg}"),
            Self::SyntaxError(msg) => write!(f, "Syntax error: {msg}"),
            Self::Internal(msg) => write!(f, "Internal error: {msg}"),
            Self::ImportError(msg) => write!(f, "ImportError: {msg}"),
        }
    }
}

fn actually_compile(code: &str) -> Result<CodeObject, CompilerError> {
    VmContext::from_text(Text::new(code)).compile()
}

#[wasm_bindgen]
pub fn compile(text: String) -> Result<JsValue, JsValue> {
    set_once();

    let code = actually_compile(&text).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let wasm_code = WasmCodeObject::from(code);
    serde_wasm_bindgen::to_value(&wasm_code).map_err(|e| e.into())
}
