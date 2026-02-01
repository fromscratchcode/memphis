use crate::{
    bytecode_vm::{
        compiler::{Bytecode, CodeObject, Compiler, Constant, Opcode},
        indices::Index,
        CompilerError, VmContext,
    },
    domain::{FunctionType, ModuleName, Text},
    parser::{
        test_utils::*,
        types::{ast, Statement},
    },
};

fn init() -> Compiler {
    Compiler::new(&ModuleName::main(), &None, "compiler_unit_test")
}

fn init_ctx(text: &str) -> VmContext {
    VmContext::from_text(Text::new(text))
}

pub fn compile_stmt(stmt: Statement) -> Bytecode {
    let mut compiler = init();
    let ast = ast![stmt];
    let code = compiler
        .compile(&ast)
        .expect("Failed to compile test Statement!");
    code.bytecode
}

pub fn expect_err(stmt: Statement) -> CompilerError {
    let mut compiler = init();
    let ast = ast![stmt];
    compiler
        .compile(&ast)
        .expect_err("Expected an error while compiling Statement!")
}

pub fn compile(text: &str) -> CodeObject {
    init_ctx(text)
        .compile()
        .expect("Failed to compile test program!")
}

pub fn compile_at_pkg(text: &str, module: ModuleName, pkg: ModuleName) -> CodeObject {
    let mut ctx = init_ctx(text);
    ctx.set_module(module);
    ctx.set_pkg(pkg);
    ctx.compile().expect("Failed to compile test program!")
}

pub fn compile_err(text: &str) -> CompilerError {
    match init_ctx(text).compile() {
        Ok(_) => panic!("Expected an CompilerError!"),
        Err(e) => e,
    }
}

pub fn compile_err_at_pkg(text: &str, module: ModuleName, pkg: ModuleName) -> CompilerError {
    let mut ctx = init_ctx(text);
    ctx.set_module(module);
    ctx.set_pkg(pkg);
    match ctx.compile() {
        Ok(_) => panic!("Expected an CompilerError!"),
        Err(e) => e,
    }
}

pub fn wrap_top_level_function(func: CodeObject) -> CodeObject {
    CodeObject {
        module_name: ModuleName::main(),
        name: "<module>".into(),
        filename: "<stdin>".into(),
        bytecode: vec![
            Opcode::LoadConst(Index::new(0)),
            Opcode::MakeFunction,
            Opcode::StoreGlobal(Index::new(0)),
        ],
        arg_count: 0,
        varnames: vec![],
        freevars: vec![],
        names: vec![func.name().into()],
        constants: vec![Constant::Code(func)],
        line_map: vec![],
        function_type: FunctionType::Regular,
        exception_table: vec![],
    }
}

pub fn wrap_top_level_class(name: &str, cls: CodeObject) -> CodeObject {
    CodeObject {
        module_name: ModuleName::main(),
        name: "<module>".into(),
        filename: "<stdin>".into(),
        bytecode: vec![
            Opcode::LoadBuildClass,
            Opcode::LoadConst(Index::new(0)),
            Opcode::Call(1),
            Opcode::StoreGlobal(Index::new(0)),
        ],
        arg_count: 0,
        varnames: vec![],
        freevars: vec![],
        names: vec![name.into()],
        constants: vec![Constant::Code(cls)],
        line_map: vec![],
        function_type: FunctionType::Regular,
        exception_table: vec![],
    }
}

macro_rules! assert_code_eq {
    ($actual:expr, $expected:expr) => {
        _assert_code_eq(&$actual, &$expected)
    };
}

/// This is designed to confirm everything in a CodeObject matches besides the Source and
/// the line number mappings.
pub fn _assert_code_eq(actual: &CodeObject, expected: &CodeObject) {
    assert_eq!(actual.name, expected.name, "Code object names do not match");
    assert_eq!(
        actual.filename, expected.filename,
        "Code object filenames do not match"
    );
    assert_eq!(
        actual.module_name, expected.module_name,
        "Code object module name do not match"
    );
    assert_eq!(
        actual.bytecode, expected.bytecode,
        "Code object bytecode does not match"
    );
    assert_eq!(
        actual.arg_count, expected.arg_count,
        "Code object arg_count does not match"
    );
    assert_eq!(
        actual.varnames, expected.varnames,
        "Code object varnames do not match"
    );
    assert_eq!(
        actual.freevars, expected.freevars,
        "Code object freevars do not match"
    );
    assert_eq!(
        actual.names, expected.names,
        "Code object names do not match"
    );
    assert_eq!(
        actual.function_type, expected.function_type,
        "Code object function types do not match"
    );

    assert_eq!(
        actual.constants.len(),
        expected.constants.len(),
        "Unequal number of code object constants"
    );

    for (i, (a_const, e_const)) in actual
        .constants
        .iter()
        .zip(expected.constants.iter())
        .enumerate()
    {
        match (a_const, e_const) {
            (Constant::Code(a_code), Constant::Code(e_code)) => {
                assert_code_eq!(a_code, e_code);
            }
            _ => {
                assert_eq!(
                    a_const, e_const,
                    "Code object constant at index {} does not match",
                    i
                );
            }
        }
    }
}

pub(crate) use assert_code_eq;
