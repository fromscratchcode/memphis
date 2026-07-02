use serde::{Serialize, Serializer};

use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Constant},
        CompilerError,
    },
    lexer::Token,
};

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
            Constant::Code(code) => WasmConstant::Code(WasmCodeObject::from_code(code)),
        }
    }
}

#[derive(Serialize)]
pub struct WasmCodeObject {
    name: String,
    bytecode: Vec<String>,
    varnames: Vec<String>,
    freevars: Vec<String>,
    names: Vec<String>,
    constants: Vec<WasmConstant>,
}

impl WasmCodeObject {
    pub fn from_code(code: CodeObject) -> Self {
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

impl CompilerError {
    pub fn wasm_repr(&self) -> String {
        match self {
            Self::Unsupported(msg) => format!("Unsupported feature: {msg}"),
            Self::SyntaxError(msg) => format!("SyntaxError: {msg}"),
            Self::ImportError(msg) => format!("ImportError: {msg}"),
        }
    }
}

impl Token {
    pub fn wasm_repr(&self) -> String {
        match self {
            Self::Identifier(i) => format!("Identifier({})", i),
            _ => format!("{:?}", self),
        }
    }
}

impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.wasm_repr())
    }
}
