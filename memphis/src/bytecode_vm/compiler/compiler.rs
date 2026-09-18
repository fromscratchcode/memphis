use crate::{
    bytecode_vm::{
        CompilerResult,
        compiler::{CodeGenFrame, CodeObject, Constant, Opcode},
        find_index,
        indices::{ConstantIndex, FreeIndex, Index, LocalIndex, NonlocalIndex},
    },
    core::{LogLevel, log},
    domain::{Context, Identifier, ModuleName},
    parser::types::Ast,
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

    /// Compile the provided `Ast` and return a `CodeObject` which can be executed. This is not
    /// destructive, meaning multiple calls will build upon the same `CodeObject`.
    pub fn compile(&mut self, ast: &Ast) -> CompilerResult<CodeObject> {
        assert!(self.code_stack.is_empty());
        let code = CodeObject::new_root(self.module_name.clone(), &self.filename);
        let code = self.compile_ast_with_code(ast, code)?;
        assert!(self.code_stack.is_empty());
        Ok(code)
    }

    fn compile_ast(&mut self, ast: &Ast) -> CompilerResult<()> {
        ast.iter().try_fold((), |_, stmt| self.compile_stmt(stmt))
    }

    fn compile_ast_with_code(&mut self, ast: &Ast, code: CodeObject) -> CompilerResult<CodeObject> {
        self.code_stack.push(CodeGenFrame::new(code));
        self.compile_ast(ast)?;
        let code_gen_frame = self.code_stack.pop().expect("Code stack underflow!");
        log(LogLevel::Debug, || {
            code_gen_frame.debug_disasm_with_labels()
        });
        Ok(code_gen_frame.finalize())
    }

    fn emit(&mut self, opcode: Opcode) {
        let line_number = self.line_number;

        let code = self.frame_mut().code_mut();
        let offset = code.bytecode.len();

        code.bytecode.push(opcode);
        code.line_map.push((offset, line_number));
    }

    fn generate_load(&mut self, name: &Identifier) -> Opcode {
        match self.context() {
            Context::Global => Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name.as_str())),
            Context::Local => {
                // Check locals first (top of the stack)
                if let Some(index) = self.get_local_index(name) {
                    return Opcode::LoadFast(index);
                }

                // Now check if this is a free variable, meaning a variable captured from a
                // non-global outer function.
                // We skip the first (top) entry because it's the current code object and the last
                // (bottom) entry because it's the global scope.
                let enclosing_scopes = &self.code_stack[1..self.code_stack.len() - 1];
                for code_gen_frame in enclosing_scopes.iter().rev() {
                    if self
                        .resolve_local_index_for_code(name, code_gen_frame.code())
                        .is_some()
                    {
                        // This would be a local in an enclosing scope, but we need an index
                        // relative to our own code object.
                        return Opcode::LoadFree(self.get_or_set_free_var(name));
                    }
                }

                // If it's not local or free, it's global. Put that quote on the wall.
                Opcode::LoadGlobal(self.get_or_set_nonlocal_index(name.as_str()))
            }
        }
    }

    fn generate_store(&mut self, name: &Identifier) -> Opcode {
        match self.context() {
            Context::Global => Opcode::StoreGlobal(self.get_or_set_nonlocal_index(name.as_str())),
            Context::Local => Opcode::StoreFast(self.get_or_set_local_index(name)),
        }
    }

    fn compile_constant(&mut self, constant: Constant) {
        let index = self.get_or_set_constant_index(constant);
        self.emit(Opcode::LoadConst(index));
    }

    fn compile_load(&mut self, name: &Identifier) {
        let load = self.generate_load(name);
        self.emit(load);
    }

    fn compile_store(&mut self, name: &Identifier) {
        let store = self.generate_store(name);
        self.emit(store);
    }

    fn get_or_set_local_index(&mut self, name: &Identifier) -> LocalIndex {
        log(LogLevel::Trace, || {
            format!("Looking for '{name}' in locals")
        });
        if let Some(index) = self.get_local_index(name) {
            index
        } else {
            let code = self.frame_mut().code_mut();
            let new_index = code.local_names.len();
            code.local_names.push(name.to_string());
            Index::new(new_index)
        }
    }

    fn get_local_index(&self, name: &Identifier) -> Option<LocalIndex> {
        let code = self.frame().code();
        self.resolve_local_index_for_code(name, code)
    }

    fn get_or_set_free_var(&mut self, name: &Identifier) -> FreeIndex {
        let code = self.frame_mut().code_mut();
        let index = if let Some(index) = find_index(&code.free_names, name.as_str()) {
            index
        } else {
            let new_index = code.free_names.len();
            code.free_names.push(name.to_string());
            new_index
        };
        Index::new(index)
    }

    fn resolve_local_index_for_code(
        &self,
        name: &Identifier,
        code: &CodeObject,
    ) -> Option<LocalIndex> {
        find_index(&code.local_names, name.as_str()).map(Index::new)
    }

    // We didn't convert this one to use Identifier yet because of how it interacts with
    // ModuleName.
    fn get_or_set_nonlocal_index(&mut self, name: &str) -> NonlocalIndex {
        log(LogLevel::Trace, || {
            format!("Looking for '{name}' in globals")
        });
        let code = self.frame_mut().code_mut();
        let index = if let Some(index) = find_index(&code.nonlocal_names, name) {
            index
        } else {
            let new_index = code.nonlocal_names.len();
            code.nonlocal_names.push(name.to_string());
            new_index
        };
        Index::new(index)
    }

    fn get_or_set_constant_index(&mut self, value: Constant) -> ConstantIndex {
        log(LogLevel::Trace, || {
            format!("Looking for '{value}' in constants")
        });
        let code = self.frame_mut().code_mut();
        let index = if let Some(index) = find_index(&code.constants, &value) {
            index
        } else {
            let next_index = code.constants.len();
            code.constants.push(value);
            next_index
        };
        Index::new(index)
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
        bytecode_vm::{CompilerError, compiler::test_utils::*},
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

        let fn_foo = CodeObject {
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            constants: vec![Constant::None],
            ..test_code("foo", &[])
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn function_definition_with_parameters() {
        let text = r#"
def foo(a, b):
    pass
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            bytecode: vec![],
            ..test_code("foo", &["a", "b"])
        };

        let expected = wrap_top_level_function(fn_foo);
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
        let fn_foo = test_code("foo", &[]);

        let expected = CodeObject {
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            nonlocal_names: vec!["decorate".into(), "foo".into()],
            constants: vec![Constant::Code(fn_foo)],
            ..test_code("<module>", &[])
        };
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
        let fn_foo = test_code("foo", &[]);
        let expected = CodeObject {
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(2)),
            ],
            nonlocal_names: vec!["inner".into(), "outer".into(), "foo".into()],
            constants: vec![Constant::Code(fn_foo)],
            ..test_code("<module>", &[])
        };
        assert_code_eq!(code, expected);
    }

    #[test]
    fn generator_definition() {
        let text = r#"
def foo():
    yield 1
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::YieldValue],
            constants: vec![Constant::Int(1)],
            function_type: FunctionType::Generator,
            ..test_code("foo", &[])
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn generator_definition_yield_from() {
        let text = r#"
def foo():
    yield from [1, 2]
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
                Opcode::YieldFrom,
            ],
            constants: vec![Constant::Int(1), Constant::Int(2)],
            function_type: FunctionType::Generator,
            ..test_code("foo", &[])
        };

        let expected = wrap_top_level_function(fn_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn async_function_definition() {
        let text = r#"
async def foo():
    pass
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            function_type: FunctionType::Async,
            ..test_code("foo", &[])
        };

        let expected = wrap_top_level_function(fn_foo);
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

        let fn_inner = CodeObject {
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            constants: vec![Constant::Int(10)],
            ..test_code("inner", &[])
        };

        let fn_foo = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(2)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::Add,
                Opcode::ReturnValue,
            ],
            local_names: vec!["a".into(), "b".into(), "inner".into()],
            constants: vec![Constant::Code(fn_inner)],
            ..test_code("foo", &["a", "b"])
        };

        let expected = wrap_top_level_function(fn_foo);
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

        let fn_foo = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(1)),
                // this should still be index 1 because we should reuse the 11.1
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(2)),
            ],
            local_names: vec!["c".into(), "d".into(), "e".into()],
            constants: vec![Constant::Int(10), Constant::Float(11.1)],
            ..test_code("foo", &[])
        };

        let expected = wrap_top_level_function(fn_foo);
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

        let fn_foo = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::ReturnValue,
            ],
            local_names: vec!["c".into()],
            constants: vec![Constant::Int(10)],
            ..test_code("foo", &[])
        };

        let expected = wrap_top_level_function(fn_foo);
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

        let fn_hello = CodeObject {
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ],
            nonlocal_names: vec!["print".into()],
            constants: vec![Constant::String("Hello".into())],
            ..test_code("hello", &[])
        };

        let fn_world = CodeObject {
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ],
            nonlocal_names: vec!["print".into()],
            constants: vec![Constant::String("World".into())],
            ..test_code("world", &[])
        };

        let expected = CodeObject {
            bytecode: vec![
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
            ],
            nonlocal_names: vec!["hello".into(), "world".into()],
            constants: vec![Constant::Code(fn_hello), Constant::Code(fn_world)],
            ..test_code("<module>", &[])
        };

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

        let fn_inner_adder = CodeObject {
            bytecode: vec![
                Opcode::LoadFree(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::Add,
                Opcode::ReturnValue,
            ],
            free_names: vec!["x".into()],
            ..test_code("inner_adder", &["y"])
        };

        let fn_make_adder = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::MakeClosure(1),
                Opcode::StoreFast(Index::new(1)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::ReturnValue,
            ],
            local_names: vec!["x".into(), "inner_adder".into()],
            constants: vec![Constant::Code(fn_inner_adder)],
            ..test_code("make_adder", &["x"])
        };

        let expected = wrap_top_level_function(fn_make_adder);
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

        let fn_bar = CodeObject {
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            constants: vec![Constant::Int(99)],
            ..test_code("bar", &["self"])
        };

        let cls_foo = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ],
            local_names: vec!["bar".into()],
            constants: vec![Constant::Code(fn_bar)],
            ..test_code("Foo", &[])
        };

        let expected = wrap_top_level_class(cls_foo);
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

        let fn_bar = CodeObject {
            bytecode: vec![
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadAttr(Index::new(0)),
                Opcode::ReturnValue,
            ],
            nonlocal_names: vec!["val".into()],
            ..test_code("bar", &["self"])
        };

        let cls_foo = CodeObject {
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ],
            local_names: vec!["bar".into()],
            constants: vec![Constant::Code(fn_bar)],
            ..test_code("Foo", &[])
        };

        let expected = wrap_top_level_class(cls_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
f = Foo()
"#;
        let code = compile(text);

        let expected = CodeObject {
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            nonlocal_names: vec!["Foo".into(), "f".into()],
            ..test_code("<module>", &[])
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn method_call() {
        let text = r#"
b = f.bar()
"#;
        let code = compile(text);

        let expected = CodeObject {
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(2)),
            ],
            nonlocal_names: vec!["f".into(), "bar".into(), "b".into()],
            ..test_code("<module>", &[])
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn regular_import_two_layers() {
        let text = r#"
import a.b.c
"#;
        let code = compile(text);

        let expected = CodeObject {
            bytecode: vec![
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            nonlocal_names: vec!["a.b.c".into(), "a".into()],
            ..test_code("<module>", &[])
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn regular_import_two_layers_with_alias() {
        let text = r#"
import a.b.c as foo
"#;
        let code = compile(text);

        let expected = CodeObject {
            bytecode: vec![
                Opcode::ImportFrom(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            nonlocal_names: vec!["a.b.c".into(), "foo".into()],
            ..test_code("<module>", &[])
        };

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

        let expected = CodeObject {
            module_name,
            bytecode: vec![
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            nonlocal_names: vec!["pkg.outer".into(), "foo".into()],
            ..test_code("<module>", &[])
        };

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

        let expected = CodeObject {
            module_name,
            bytecode: vec![
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            nonlocal_names: vec!["pkg.outer.inner".into(), "foo".into()],
            ..test_code("<module>", &[])
        };

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
