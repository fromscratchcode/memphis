use console_error_panic_hook::set_once;
use wasm_bindgen::prelude::*;

use crate::{
    bytecode_vm::{compiler::CodeObject, CompilerResult, VmContext},
    domain::Text,
    lexer::Lexer,
    parser::Parser,
    wasm::repr::WasmCodeObject,
};

#[wasm_bindgen]
pub fn compile(text: String) -> Result<JsValue, JsValue> {
    set_once();
    let code = actually_compile(&text).map_err(|e| JsValue::from_str(&e.wasm_repr()))?;
    let wasm_code = WasmCodeObject::from_code(code);
    Ok(serde_wasm_bindgen::to_value(&wasm_code).expect("Corrupted WasmCodeObject"))
}

#[wasm_bindgen]
pub fn lex(text: String) -> JsValue {
    set_once();
    let tokens = Lexer::lex_text(&Text::new(&text));
    serde_wasm_bindgen::to_value(&tokens).expect("Corrupted token stream")
}

#[wasm_bindgen]
pub fn parse(text: String) -> Result<JsValue, JsValue> {
    set_once();
    let ast =
        Parser::parse_text(&Text::new(&text)).map_err(|e| JsValue::from_str(&e.debug_message()))?;
    Ok(serde_wasm_bindgen::to_value(&ast).expect("Corrupted token stream"))
}

fn actually_compile(text: &str) -> CompilerResult<CodeObject> {
    let ctx = VmContext::stdin();
    ctx.compile(&Text::new(text))
}
