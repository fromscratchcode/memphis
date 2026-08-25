use console_error_panic_hook::set_once;
use wasm_bindgen::prelude::*;

use crate::{
    Engine, MemphisContext, ModuleOrigin,
    bytecode_vm::{CompilerResult, VmContext, compiler::CodeObject},
    domain::Text,
    lexer::Lexer,
    parser::Parser,
    wasm::{io::WasmIo, repr::WasmCodeObject},
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

#[wasm_bindgen]
pub fn run(text: &str) -> String {
    set_once();
    let (io, capture) = WasmIo::new();
    let mut ctx = MemphisContext::new(Engine::Treewalk, ModuleOrigin::Stdin, io);
    let result = ctx.eval(Text::new(text));
    let mut output = capture.borrow_mut().take_output();
    // in Exec mode, we don't really need the Ok result
    // should we model this better?
    if let Err(e) = result {
        output.push_str(&e.to_string());
    }
    output
}

fn actually_compile(text: &str) -> CompilerResult<CodeObject> {
    let (io, _) = WasmIo::new();
    let ctx = VmContext::init(ModuleOrigin::Stdin, io);
    ctx.compile(&Text::new(text))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_works_and_captures_output() {
        let input = r#"
print("Hello World")
"#;
        assert_eq!(run(input), "Hello World\n");
    }
}
