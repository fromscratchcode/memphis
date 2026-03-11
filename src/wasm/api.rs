use console_error_panic_hook::set_once;
use wasm_bindgen::prelude::*;

use crate::{
    bytecode_vm::{compiler::CodeObject, CompilerResult, VmContext},
    domain::Text,
    lexer::{Lexer, Token},
    wasm::repr::WasmCodeObject,
    Engine, MemphisContext,
};

#[wasm_bindgen]
pub fn greet() -> String {
    "Hello from WebAssembly!".to_string()
}

#[wasm_bindgen]
pub fn evaluate(code: String) -> String {
    // Set the panic hook for better error messages in the browser console
    set_once();

    let mut context = MemphisContext::stdin(Engine::Treewalk);
    let result = context.eval(Text::new(&code)).expect("Failed to evaluate.");
    result.to_string()
}

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
    let tokens = actually_lex(&text);
    serde_wasm_bindgen::to_value(&tokens).expect("Corrupted token stream")
}

fn actually_compile(text: &str) -> CompilerResult<CodeObject> {
    let mut ctx = VmContext::stdin();
    ctx.add_text(Text::new(text));
    ctx.compile()
}

fn actually_lex(text: &str) -> Vec<Token> {
    let mut l = Lexer::new();
    l.add_text(&Text::new(text));
    l.collect()
}
