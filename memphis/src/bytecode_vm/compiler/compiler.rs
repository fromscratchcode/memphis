use crate::{
    bytecode_vm::{
        CompilerResult,
        compiler::{CodeGenFrame, CodeObject, Constant, Opcode, code::CodeSpec},
    },
    core::{LogLevel, log},
    domain::{Context, ModuleName},
    parser::types::{Ast, LoopIndex},
};

use super::opcode::UnsignedOffset;

mod expr;
mod stmt;

/// A Python bytecode compiler.
pub struct Compiler {
    /// Track the filename so we can associate it with later `CodeObjects`.
    filename: String,

    /// Track the module name so we can associate it with later `CodeObjects`.
    module_name: ModuleName,

    /// We must know the package name for relative imports to work.
    package: Option<ModuleName>,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (variable names, constants, etc.).
    code_stack: Vec<CodeGenFrame>,

    /// The most recent line number seen from the Ast.
    line_number: UnsignedOffset,
}

impl Compiler {
    pub fn new(module_name: &ModuleName, package: &Option<ModuleName>, filename: &str) -> Self {
        Self {
            filename: filename.to_string(),
            module_name: module_name.clone(),
            package: package.clone(),
            code_stack: vec![],
            line_number: 0,
        }
    }

    /// Compile the provided `Ast` and return a `CodeObject` which can be executed.
    pub fn compile(&mut self, ast: &Ast) -> CompilerResult<CodeObject> {
        assert!(self.code_stack.is_empty());
        let spec = CodeSpec::new_root(self.module_name.clone(), &self.filename);
        let code = self.compile_ast_with_spec(ast, spec)?;
        assert!(self.code_stack.is_empty());
        Ok(code)
    }

    fn compile_ast(&mut self, ast: &Ast) -> CompilerResult<()> {
        ast.iter().try_fold((), |_, stmt| self.compile_stmt(stmt))
    }

    fn compile_ast_with_spec(&mut self, ast: &Ast, spec: CodeSpec) -> CompilerResult<CodeObject> {
        self.code_stack.push(CodeGenFrame::new(spec));
        self.compile_ast(ast)?;
        let code_gen_frame = self.code_stack.pop().expect("Code stack underflow!");
        log(LogLevel::Debug, || {
            code_gen_frame.debug_disasm_with_labels()
        });
        Ok(code_gen_frame.finalize())
    }

    fn emit(&mut self, opcode: Opcode) {
        let line_number = self.line_number;
        self.frame_mut().emit(opcode, line_number);
    }

    fn generate_load(&mut self, name: &str) -> Opcode {
        match self.context() {
            Context::Global => Opcode::LoadGlobal(self.frame_mut().get_or_set_nonlocal_index(name)),
            Context::Local => {
                // Check locals first (top of the stack)
                if let Some(index) = self.frame().local_index(name) {
                    return Opcode::LoadFast(index);
                }

                // Now check if this is a free variable, meaning a variable captured from a
                // non-global outer function.
                // We skip the first (top) entry because it's the current code object and the last
                // (bottom) entry because it's the global scope.
                let enclosing_scopes = &self.code_stack[1..self.code_stack.len() - 1];
                for frame in enclosing_scopes.iter().rev() {
                    if frame.local_index(name).is_some() {
                        // This would be a local in an enclosing scope, but we need an index
                        // relative to our own code object.
                        return Opcode::LoadFree(self.frame_mut().get_or_set_free_var(name));
                    }
                }

                // If it's not local or free, it's global. Put that quote on the wall.
                Opcode::LoadGlobal(self.frame_mut().get_or_set_nonlocal_index(name))
            }
        }
    }

    fn generate_store(&mut self, name: &str) -> Opcode {
        match self.context() {
            Context::Global => {
                Opcode::StoreGlobal(self.frame_mut().get_or_set_nonlocal_index(name))
            }
            Context::Local => Opcode::StoreFast(self.frame_mut().get_or_set_local_index(name)),
        }
    }

    /// Load a CodeObject and turn it into a function or closure.
    fn compile_function(&mut self, code: CodeObject) -> CompilerResult<()> {
        let free_vars = code.free_names().to_vec();
        self.compile_code(code);

        if free_vars.is_empty() {
            self.emit(Opcode::MakeFunction);
        } else {
            // We push the free vars onto the stack in reverse order so that we will pop
            // them off in order.
            for free_var in free_vars.iter().rev() {
                self.compile_load(free_var);
            }
            self.emit(Opcode::MakeClosure(free_vars.len()));
        }
        Ok(())
    }

    fn compile_loop_index(&mut self, index: &LoopIndex) {
        match index {
            LoopIndex::Variable(var) => self.compile_store(var.as_str()),
            LoopIndex::Tuple(t) => {
                self.emit(Opcode::UnpackSequence(t.len()));
                for var in t.iter().rev() {
                    self.compile_store(var.as_str());
                }
            }
        };
    }

    fn compile_code(&mut self, code: CodeObject) {
        self.compile_constant(Constant::Code(code));
    }

    fn compile_constant(&mut self, constant: Constant) {
        let index = self.frame_mut().get_or_set_constant_index(constant);
        self.emit(Opcode::LoadConst(index));
    }

    fn compile_load(&mut self, name: &str) {
        let load = self.generate_load(name);
        self.emit(load);
    }

    fn compile_store(&mut self, name: &str) {
        let store = self.generate_store(name);
        self.emit(store);
    }

    /// Since an instance of this `Compiler` operates on a single module, we can assume
    /// that the outer code object is the global scope and any others are local scopes.
    fn context(&self) -> Context {
        match self.code_stack.len() {
            1 => Context::Global,
            _ => Context::Local,
        }
    }

    fn frame_mut(&mut self) -> &mut CodeGenFrame {
        self.code_stack
            .last_mut()
            .expect("Compiler invariant violated: no current CodeGenFrame")
    }

    fn frame(&self) -> &CodeGenFrame {
        self.code_stack
            .last()
            .expect("Compiler invariant violated: no current CodeGenFrame")
    }
}

#[cfg(test)]
mod tests_compiler {
    use crate::{
        bytecode_vm::{CompilerError, compiler::test_utils::*, indices::Index},
        domain::FunctionType,
    };

    use super::*;

    #[test]
    fn function_definition_early_return() {
        let text = r#"
def foo():
    return
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &[])
            .with_bytecode([Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue])
            .with_constants([Constant::None])
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_parameters() {
        let text = r#"
def foo(a, b):
    pass
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &["a", "b"]).build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_decorator() {
        let text = r#"
@decorate
def foo():
    pass
"#;
        let code = compile(text);
        let fn_foo = test_code("foo", &[]).build();

        let expected = test_module()
            .with_bytecode([
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(1)),
            ])
            .with_nonlocal_names(["decorate", "foo"])
            .with_constants([Constant::Code(fn_foo)])
            .build();
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_multiple_decorators() {
        let text = r#"
@outer
@inner
def foo():
    pass
"#;
        let code = compile(text);
        let fn_foo = test_code("foo", &[]).build();
        let expected = test_module()
            .with_bytecode([
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(2)),
            ])
            .with_nonlocal_names(["inner", "outer", "foo"])
            .with_constants([Constant::Code(fn_foo)])
            .build();
        assert_code_eq!(code, expected);
    }

    #[test]
    fn generator_definition() {
        let text = r#"
def foo():
    yield 1
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &[])
            .with_bytecode([Opcode::LoadConst(Index::new(0)), Opcode::YieldValue])
            .with_constants([Constant::Int(1)])
            .with_function_type(FunctionType::Generator)
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn generator_definition_yield_from() {
        let text = r#"
def foo():
    yield from [1, 2]
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &[])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
                Opcode::YieldFrom,
            ])
            .with_constants([Constant::Int(1), Constant::Int(2)])
            .with_function_type(FunctionType::Generator)
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn async_function_definition() {
        let text = r#"
async def foo():
    pass
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &[])
            .with_function_type(FunctionType::Async)
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner():
        return 10
    return a + b
"#;
        let code = compile(text);

        let fn_inner = test_code("inner", &[])
            .with_bytecode([Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue])
            .with_constants([Constant::Int(10)])
            .build();

        let fn_foo = test_code("foo", &["a", "b"])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(2)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::Add,
                Opcode::ReturnValue,
            ])
            .with_local_names(["a", "b", "inner"])
            .with_constants([Constant::Code(fn_inner)])
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_local_var() {
        let text = r#"
def foo():
    c = 10
    d = 11.1
    e = 11.1
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &[])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(1)),
                // this should still be index 1 because we should reuse the 11.1
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(2)),
            ])
            .with_local_names(["c", "d", "e"])
            .with_constants([Constant::Int(10), Constant::Float(11.1)])
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_local_var_and_return() {
        let text = r#"
def foo():
    c = 10
    return c
"#;
        let code = compile(text);

        let fn_foo = test_code("foo", &[])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::ReturnValue,
            ])
            .with_local_names(["c"])
            .with_constants([Constant::Int(10)])
            .build();

        let expected = wrap_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_two_calls_and_no_return() {
        let text = r#"
def hello():
    print("Hello")

def world():
    print("World")

hello()
world()
"#;
        let code = compile(text);

        let fn_hello = test_code("hello", &[])
            .with_bytecode([
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ])
            .with_nonlocal_names(["print"])
            .with_constants([Constant::String("Hello".into())])
            .build();

        let fn_world = test_code("world", &[])
            .with_bytecode([
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ])
            .with_nonlocal_names(["print"])
            .with_constants([Constant::String("World".into())])
            .build();

        let expected = test_module()
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::MakeFunction,
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::PopTop,
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::Call(0),
                Opcode::ReturnValue,
            ])
            .with_nonlocal_names(["hello", "world"])
            .with_constants([Constant::Code(fn_hello), Constant::Code(fn_world)])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn closure_definition() {
        let text = r#"
def make_adder(x):
    def inner_adder(y):
        return x + y
    return inner_adder
"#;
        let code = compile(text);

        let fn_inner_adder = test_code("inner_adder", &["y"])
            .with_bytecode([
                Opcode::LoadFree(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::Add,
                Opcode::ReturnValue,
            ])
            .with_free_names(["x"])
            .build();

        let fn_make_adder = test_code("make_adder", &["x"])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::MakeClosure(1),
                Opcode::StoreFast(Index::new(1)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::ReturnValue,
            ])
            .with_local_names(["x", "inner_adder"])
            .with_constants([Constant::Code(fn_inner_adder)])
            .build();

        let expected = wrap_function(fn_make_adder);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar(self):
        return 99
"#;
        let code = compile(text);

        let fn_bar = test_code("bar", &["self"])
            .with_bytecode([Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue])
            .with_constants([Constant::Int(99)])
            .build();

        let cls_foo = test_code("Foo", &[])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ])
            .with_local_names(["bar"])
            .with_constants([Constant::Code(fn_bar)])
            .build();

        let expected = wrap_class(cls_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_definition_member_access() {
        let text = r#"
class Foo:
    def bar(self):
        return self.val
"#;
        let code = compile(text);

        let fn_bar = test_code("bar", &["self"])
            .with_bytecode([
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadAttr(Index::new(0)),
                Opcode::ReturnValue,
            ])
            .with_nonlocal_names(["val"])
            .build();

        let cls_foo = test_code("Foo", &[])
            .with_bytecode([
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ])
            .with_local_names(["bar"])
            .with_constants([Constant::Code(fn_bar)])
            .build();

        let expected = wrap_class(cls_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
f = Foo()
"#;
        let code = compile(text);

        let expected = test_module()
            .with_bytecode([
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
            ])
            .with_nonlocal_names(["Foo", "f"])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn method_call() {
        let text = r#"
b = f.bar()
"#;
        let code = compile(text);

        let expected = test_module()
            .with_bytecode([
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(2)),
            ])
            .with_nonlocal_names(["f", "bar", "b"])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn regular_import_two_layers() {
        let text = r#"
import a.b.c
"#;
        let code = compile(text);

        let expected = test_module()
            .with_bytecode([
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ])
            .with_nonlocal_names(["a.b.c", "a"])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn regular_import_two_layers_with_alias() {
        let text = r#"
import a.b.c as foo
"#;
        let code = compile(text);

        let expected = test_module()
            .with_bytecode([
                Opcode::ImportFrom(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ])
            .with_nonlocal_names(["a.b.c", "foo"])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn selective_import_relative_one_layer_from_main() {
        let text = r#"
from .outer import foo
"#;
        let err = compile_err(text);

        match err {
            CompilerError::ImportError(msg) => assert_eq!(
                msg,
                "attempted relative import with no known parent package".to_string()
            ),
            _ => panic!("Expected an ImportError"),
        }
    }

    #[test]
    fn selective_import_relative_one_layer_from_pkg() {
        let text = r#"
from .outer import foo
"#;
        let module_name = ModuleName::from_segments(&["pkg", "mod"]);
        let pkg = ModuleName::from_segments(&["pkg"]);
        let code = compile_at_pkg(text, module_name.clone(), pkg);

        let expected = test_module_for(module_name)
            .with_bytecode([
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ])
            .with_nonlocal_names(["pkg.outer", "foo"])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn selective_import_relative_two_layers_from_pkg() {
        let text = r#"
from .outer.inner import foo
"#;
        let module_name = ModuleName::from_segments(&["pkg", "mod"]);
        let pkg = ModuleName::from_segments(&["pkg"]);
        let code = compile_at_pkg(text, module_name.clone(), pkg);

        let expected = test_module_for(module_name)
            .with_bytecode([
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ])
            .with_nonlocal_names(["pkg.outer.inner", "foo"])
            .build();

        assert_code_eq!(code, expected);
    }

    #[test]
    fn selective_import_relative_one_layer_from_pkg_too_many_levels() {
        let text = r#"
from ..outer import foo
"#;
        let module_name = ModuleName::from_segments(&["pkg", "mod"]);
        let pkg = ModuleName::from_segments(&["pkg"]);
        let err = compile_err_at_pkg(text, module_name, pkg);

        match err {
            CompilerError::ImportError(msg) => assert_eq!(
                msg,
                "attempted relative import beyond top-level package".to_string()
            ),
            _ => panic!("Expected an ImportError"),
        }
    }
}
