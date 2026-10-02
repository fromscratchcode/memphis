use crate::{
    ModuleOrigin,
    bytecode_vm::{
        CompilerError, VmContext,
        compiler::{Bytecode, CodeObject, CodeSpec, Compiler, Constant, ExceptionRange, Opcode},
        indices::Index,
    },
    domain::{FunctionType, ModuleName, Text},
    parser::{
        test_utils::*,
        types::{Statement, ast},
    },
    runtime::CompileIo,
};

fn init() -> Compiler {
    Compiler::new(&ModuleName::main(), &None, "compiler_unit_test")
}

fn init_ctx() -> VmContext {
    VmContext::init(ModuleOrigin::Stdin, CompileIo)
}

pub fn compile_stmt(stmt: Statement) -> Bytecode {
    let mut compiler = init();
    let ast = ast![stmt];
    let code = compiler
        .compile(&ast)
        .expect("Failed to compile test Statement!");
    code.bytecode().to_vec()
}

pub fn expect_err(stmt: Statement) -> CompilerError {
    let mut compiler = init();
    let ast = ast![stmt];
    compiler
        .compile(&ast)
        .expect_err("Expected an error while compiling Statement!")
}

pub fn compile(text: &str) -> CodeObject {
    init_ctx()
        .compile(&Text::new(text))
        .expect("Failed to compile test program!")
}

pub fn compile_at_pkg(text: &str, module: ModuleName, pkg: ModuleName) -> CodeObject {
    let mut ctx = init_ctx();
    ctx.set_module(module);
    ctx.set_pkg(pkg);
    ctx.compile(&Text::new(text))
        .expect("Failed to compile test program!")
}

pub fn compile_err(text: &str) -> CompilerError {
    match init_ctx().compile(&Text::new(text)) {
        Ok(_) => panic!("Expected an CompilerError!"),
        Err(e) => e,
    }
}

pub fn compile_err_at_pkg(text: &str, module: ModuleName, pkg: ModuleName) -> CompilerError {
    let mut ctx = init_ctx();
    ctx.set_module(module);
    ctx.set_pkg(pkg);
    match ctx.compile(&Text::new(text)) {
        Ok(_) => panic!("Expected an CompilerError!"),
        Err(e) => e,
    }
}

pub fn test_code(name: &str, params: &[&str]) -> TestCodeBuilder {
    let spec = CodeSpec::new(
        name,
        ModuleName::main(),
        "<stdin>",
        params,
        FunctionType::Regular,
    );
    TestCodeBuilder::new(spec)
}

pub fn test_module() -> TestCodeBuilder {
    test_module_for(ModuleName::main())
}

pub fn test_module_for(module_name: ModuleName) -> TestCodeBuilder {
    let spec = CodeSpec::new_root(module_name, "<stdin>");
    TestCodeBuilder::new(spec)
}

pub struct TestCodeBuilder {
    spec: CodeSpec,
    bytecode: Bytecode,
    local_names: Vec<String>,
    free_names: Vec<String>,
    nonlocal_names: Vec<String>,
    constants: Vec<Constant>,
    line_map: Vec<(usize, usize)>,
    exception_table: Vec<ExceptionRange>,
}

impl TestCodeBuilder {
    fn new(spec: CodeSpec) -> Self {
        let local_names = spec.parameters().to_vec();
        Self {
            spec,
            bytecode: Vec::new(),
            free_names: Vec::new(),
            local_names,
            nonlocal_names: Vec::new(),
            constants: Vec::new(),
            line_map: Vec::new(),
            exception_table: Vec::new(),
        }
    }

    pub fn with_bytecode(mut self, bytecode: impl IntoIterator<Item = Opcode>) -> Self {
        self.bytecode = bytecode.into_iter().collect();
        self
    }

    pub fn with_constants(mut self, constants: impl IntoIterator<Item = Constant>) -> Self {
        self.constants = constants.into_iter().collect();
        self
    }

    pub fn with_local_names(mut self, names: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.local_names = names.into_iter().map(Into::into).collect();
        self
    }

    pub fn with_free_names(mut self, names: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.free_names = names.into_iter().map(Into::into).collect();
        self
    }

    pub fn with_nonlocal_names(
        mut self,
        names: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        self.nonlocal_names = names.into_iter().map(Into::into).collect();
        self
    }

    pub fn with_function_type(mut self, function_type: FunctionType) -> Self {
        self.spec = self.spec.with_function_type(function_type);
        self
    }

    pub fn build(self) -> CodeObject {
        CodeObject::from_compiler(
            self.spec,
            self.bytecode,
            self.local_names,
            self.free_names,
            self.nonlocal_names,
            self.constants,
            self.line_map,
            self.exception_table,
        )
    }
}

pub fn wrap_function(func: CodeObject) -> CodeObject {
    test_module()
        .with_bytecode([
            Opcode::LoadConst(Index::new(0)),
            Opcode::MakeFunction,
            Opcode::StoreGlobal(Index::new(0)),
        ])
        .with_nonlocal_names([func.name()])
        .with_constants([Constant::Code(func)])
        .build()
}

pub fn wrap_class(cls: CodeObject) -> CodeObject {
    test_module()
        .with_bytecode([
            Opcode::LoadBuildClass,
            Opcode::LoadConst(Index::new(0)),
            Opcode::Call(1),
            Opcode::StoreGlobal(Index::new(0)),
        ])
        .with_nonlocal_names([cls.name()])
        .with_constants([Constant::Code(cls)])
        .build()
}

macro_rules! assert_code_eq {
    ($actual:expr, $expected:expr) => {
        _assert_code_eq(&$actual, &$expected)
    };
}

/// This is designed to confirm everything in a CodeObject matches besides the Source and
/// the line number mappings.
pub fn _assert_code_eq(actual: &CodeObject, expected: &CodeObject) {
    assert_eq!(
        actual.name(),
        expected.name(),
        "Code object names do not match"
    );
    assert_eq!(
        actual.path(),
        expected.path(),
        "Code object filenames do not match"
    );
    assert_eq!(
        actual.module_name(),
        expected.module_name(),
        "Code object module name do not match"
    );
    assert_eq!(
        actual.bytecode(),
        expected.bytecode(),
        "Code object bytecode does not match"
    );
    assert_eq!(
        actual.arg_count(),
        expected.arg_count(),
        "Code object arg_count does not match"
    );
    assert_eq!(
        actual.local_names(),
        expected.local_names(),
        "Code object local names do not match"
    );
    assert_eq!(
        actual.free_names(),
        expected.free_names(),
        "Code object free names do not match"
    );
    assert_eq!(
        actual.nonlocal_names(),
        expected.nonlocal_names(),
        "Code object nonlocal names do not match"
    );
    assert_eq!(
        actual.function_type(),
        expected.function_type(),
        "Code object function types do not match"
    );

    assert_eq!(
        actual.constants().len(),
        expected.constants().len(),
        "Unequal number of code object constants"
    );

    for (i, (a_const, e_const)) in actual
        .constants()
        .iter()
        .zip(expected.constants().iter())
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
