use crate::{
    bytecode_vm::{
        compiler::{CodeObject, CompilerError, Constant, Opcode},
        find_index,
        indices::{ConstantIndex, FreeIndex, Index, LocalIndex, NonlocalIndex},
        CompilerResult,
    },
    core::{log, LogLevel},
    domain::{Context, Identifier, ModuleName},
    parser::types::Ast,
};

use super::opcode::{SignedOffset, UnsignedOffset};

mod expr;
mod stmt;

/// A Python bytecode compiler.
pub struct Compiler {
    filename: String,
    module_name: ModuleName,

    /// Keep a reference to the code object being constructed so we can associate things with it,
    /// (variable names, constants, etc.).
    code_stack: Vec<CodeObject>,

    /// The most recent line number seen from the Ast.
    line_number: UnsignedOffset,
}

impl Compiler {
    pub fn new(module_name: &ModuleName, filename: &str) -> Self {
        Self {
            filename: filename.to_string(),
            module_name: module_name.clone(),
            code_stack: vec![],
            line_number: 0,
        }
    }

    /// Compile the provided `Ast` and return a `CodeObject` which can be executed. This is not
    /// destructive, meaning multiple calls will build upon the same `CodeObject`.
    pub fn compile(&mut self, ast: &Ast) -> CompilerResult<CodeObject> {
        assert!(self.code_stack.is_empty());
        let code = CodeObject::new(self.module_name.clone(), &self.filename);
        let code = self.compile_ast_with_code(ast, code)?;
        assert!(self.code_stack.is_empty());
        Ok(code)
    }

    fn compile_ast(&mut self, ast: &Ast) -> CompilerResult<()> {
        ast.iter().try_fold((), |_, stmt| self.compile_stmt(stmt))
    }

    fn compile_ast_with_code(&mut self, ast: &Ast, code: CodeObject) -> CompilerResult<CodeObject> {
        self.code_stack.push(code);
        self.compile_ast(ast)?;
        Ok(self.code_stack.pop().expect("Code stack underflow!"))
    }

    fn current_offset(&self) -> CompilerResult<UnsignedOffset> {
        let code = self.ensure_code_object()?;
        Ok(code.bytecode.len())
    }

    fn forward_offset_to(&self, to: UnsignedOffset) -> CompilerResult<SignedOffset> {
        Ok(self.current_offset()? as SignedOffset - to as SignedOffset - 1)
    }

    // We must mark these as signed because we are doing subtraction which could product a negative
    // value.
    fn backward_offset_from(&self, from: UnsignedOffset) -> CompilerResult<SignedOffset> {
        Ok(from as SignedOffset - self.current_offset()? as SignedOffset - 1)
    }

    fn emit(&mut self, opcode: Opcode) -> CompilerResult<()> {
        let line_number = self.line_number;
        let offset = self.current_offset()?;

        let code = self.ensure_code_object_mut()?;
        code.bytecode.push(opcode);
        code.line_map.push((offset, line_number));
        Ok(())
    }

    fn emit_at(&mut self, offset: UnsignedOffset, opcode: Opcode) -> CompilerResult<()> {
        let code = self.ensure_code_object_mut()?;
        code.bytecode[offset] = opcode;
        Ok(())
    }

    /// Emit a `Placeholder` op and return its `UnsignedOffset`. This will later need to be updated
    /// using `emit_at` once the final jump target is known.
    fn emit_placeholder(&mut self) -> CompilerResult<UnsignedOffset> {
        let placeholder = self.current_offset()?;
        self.emit(Opcode::Placeholder)?;
        Ok(placeholder)
    }

    fn generate_load(&mut self, name: &Identifier) -> CompilerResult<Opcode> {
        match self.context() {
            Context::Global => Ok(Opcode::LoadGlobal(
                self.get_or_set_nonlocal_index(name.as_str())?,
            )),
            Context::Local => {
                // Check locals first (top of the stack)
                if let Some(index) = self.get_local_index(name)? {
                    return Ok(Opcode::LoadFast(index));
                }

                // Now check if this is a free variable, meaning a variable captured from a
                // non-global outer function.
                // We skip the first (top) entry because it's the current code object and the last
                // (bottom) entry because it's the global scope.
                let enclosing_scopes = &self.code_stack[1..self.code_stack.len() - 1];
                for code in enclosing_scopes.iter().rev() {
                    if self.resolve_local_index_for_code(name, code).is_some() {
                        // This would be a local in an enclosing scope, but we need an index
                        // relative to our own code object.
                        return Ok(Opcode::LoadFree(self.get_or_set_free_var(name)?));
                    }
                }

                // If it's not local or free, it's global. Put that quote on the wall.
                Ok(Opcode::LoadGlobal(
                    self.get_or_set_nonlocal_index(name.as_str())?,
                ))
            }
        }
    }

    fn generate_store(&mut self, name: &Identifier) -> CompilerResult<Opcode> {
        let opcode = match self.context() {
            Context::Global => Opcode::StoreGlobal(self.get_or_set_nonlocal_index(name.as_str())?),
            Context::Local => Opcode::StoreFast(self.get_or_set_local_index(name)?),
        };
        Ok(opcode)
    }

    fn compile_constant(&mut self, constant: Constant) -> CompilerResult<()> {
        let index = self.get_or_set_constant_index(constant)?;
        self.emit(Opcode::LoadConst(index))?;
        Ok(())
    }

    fn compile_load(&mut self, name: &Identifier) -> CompilerResult<()> {
        let load = self.generate_load(name)?;
        self.emit(load)
    }

    fn compile_store(&mut self, name: &Identifier) -> CompilerResult<()> {
        let store = self.generate_store(name)?;
        self.emit(store)
    }

    fn get_or_set_local_index(&mut self, name: &Identifier) -> CompilerResult<LocalIndex> {
        log(LogLevel::Trace, || {
            format!("Looking for '{name}' in locals")
        });
        if let Some(index) = self.get_local_index(name)? {
            Ok(index)
        } else {
            let code = self.ensure_code_object_mut()?;
            let new_index = code.varnames.len();
            code.varnames.push(name.to_string());
            Ok(Index::new(new_index))
        }
    }

    fn get_local_index(&self, name: &Identifier) -> CompilerResult<Option<LocalIndex>> {
        let code = self.ensure_code_object()?;
        Ok(self.resolve_local_index_for_code(name, code))
    }

    fn get_or_set_free_var(&mut self, name: &Identifier) -> CompilerResult<FreeIndex> {
        let code = self.ensure_code_object_mut()?;
        let index = if let Some(index) = find_index(&code.freevars, name.as_str()) {
            index
        } else {
            let new_index = code.freevars.len();
            code.freevars.push(name.to_string());
            new_index
        };
        Ok(Index::new(index))
    }

    fn resolve_local_index_for_code(
        &self,
        name: &Identifier,
        code: &CodeObject,
    ) -> Option<LocalIndex> {
        find_index(&code.varnames, name.as_str()).map(Index::new)
    }

    // We didn't convert this one to use Identifier yet because of how it interacts with
    // ModuleName.
    fn get_or_set_nonlocal_index(&mut self, name: &str) -> CompilerResult<NonlocalIndex> {
        log(LogLevel::Trace, || {
            format!("Looking for '{name}' in globals")
        });
        let code = self.ensure_code_object_mut()?;
        let index = if let Some(index) = find_index(&code.names, name) {
            index
        } else {
            let new_index = code.names.len();
            code.names.push(name.to_string());
            new_index
        };
        Ok(Index::new(index))
    }

    fn get_or_set_constant_index(&mut self, value: Constant) -> CompilerResult<ConstantIndex> {
        log(LogLevel::Trace, || {
            format!("Looking for '{value}' in constants")
        });
        let code = self.ensure_code_object_mut()?;
        let index = if let Some(index) = find_index(&code.constants, &value) {
            index
        } else {
            let next_index = code.constants.len();
            code.constants.push(value);
            next_index
        };
        Ok(Index::new(index))
    }

    /// Since an instance of this `Compiler` operates on a single module, we can assume
    /// that the outer code object is the global scope and any others are local scopes.
    fn context(&self) -> Context {
        match self.code_stack.len() {
            1 => Context::Global,
            _ => Context::Local,
        }
    }

    fn ensure_code_object_mut(&mut self) -> CompilerResult<&mut CodeObject> {
        self.code_stack
            .last_mut()
            .ok_or_else(|| internal_error("Failed to find current code object."))
    }

    fn ensure_code_object(&self) -> CompilerResult<&CodeObject> {
        self.code_stack
            .last()
            .ok_or_else(|| internal_error("Failed to find current code object."))
    }
}

fn internal_error(msg: &str) -> CompilerError {
    CompilerError::Internal(msg.to_string())
}

#[cfg(test)]
mod tests_compiler {
    use crate::{bytecode_vm::compiler::test_utils::*, domain::FunctionType};

    use super::*;

    #[test]
    fn function_definition_early_return() {
        let text = r#"
def foo():
    return
"#;
        let code = compile(text);

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 2,
            varnames: vec!["a".into(), "b".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["decorate".into(), fn_foo.name().into()],
            constants: vec![Constant::Code(fn_foo)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::Call(1),
                Opcode::Call(1),
                Opcode::StoreGlobal(Index::new(2)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["inner".into(), "outer".into(), fn_foo.name().into()],
            constants: vec![Constant::Code(fn_foo)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::YieldValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(1)],
            line_map: vec![],
            function_type: FunctionType::Generator,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
                Opcode::YieldFrom,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(1), Constant::Int(2)],
            line_map: vec![],
            function_type: FunctionType::Generator,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Async,
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
            module_name: ModuleName::main(),
            name: "inner".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let fn_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(2)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::Add,
                Opcode::ReturnValue,
            ],
            arg_count: 2,
            varnames: vec!["a".into(), "b".into(), "inner".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_inner)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(1)),
                // this should still be index 1 because we should reuse the 11.1
                Opcode::LoadConst(Index::new(1)),
                Opcode::StoreFast(Index::new(2)),
            ],
            arg_count: 0,
            varnames: vec!["c".into(), "d".into(), "e".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10), Constant::Float(11.1)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreFast(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::ReturnValue,
            ],
            arg_count: 0,
            varnames: vec!["c".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(10)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "hello".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["print".into()],
            constants: vec![Constant::String("Hello".into())],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let fn_world = CodeObject {
            module_name: ModuleName::main(),
            name: "world".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::Call(1),
                Opcode::PopTop,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["print".into()],
            constants: vec![Constant::String("World".into())],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
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
                Opcode::PopTop,
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["hello".into(), "world".into()],
            constants: vec![Constant::Code(fn_hello), Constant::Code(fn_world)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "inner_adder".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadFree(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::Add,
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["y".into()],
            freevars: vec!["x".into()],
            names: vec![],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let fn_make_adder = CodeObject {
            module_name: ModuleName::main(),
            name: "make_adder".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadFast(Index::new(0)),
                Opcode::MakeClosure(1),
                Opcode::StoreFast(Index::new(1)),
                Opcode::LoadFast(Index::new(1)),
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["x".into(), "inner_adder".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_inner_adder)],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "bar".into(),
            filename: "<stdin>".into(),
            bytecode: vec![Opcode::LoadConst(Index::new(0)), Opcode::ReturnValue],
            arg_count: 1,
            varnames: vec!["self".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Int(99)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let cls_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "Foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ],
            arg_count: 0,
            varnames: vec!["bar".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_bar)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_class("Foo", cls_foo);
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
            module_name: ModuleName::main(),
            name: "bar".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadFast(Index::new(0)),
                Opcode::LoadAttr(Index::new(0)),
                Opcode::ReturnValue,
            ],
            arg_count: 1,
            varnames: vec!["self".into()],
            freevars: vec![],
            names: vec!["val".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let cls_foo = CodeObject {
            module_name: ModuleName::main(),
            name: "Foo".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadConst(Index::new(0)),
                Opcode::MakeFunction,
                Opcode::StoreFast(Index::new(0)),
            ],
            arg_count: 0,
            varnames: vec!["bar".into()],
            freevars: vec![],
            names: vec![],
            constants: vec![Constant::Code(fn_bar)],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        let expected = wrap_top_level_class("Foo", cls_foo);
        assert_code_eq!(code, expected);
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
f = Foo()
"#;
        let code = compile(text);

        let expected = CodeObject {
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["Foo".into(), "f".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::Call(0),
                Opcode::StoreGlobal(Index::new(2)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["f".into(), "bar".into(), "b".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["a.b.c".into(), "a".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
            module_name: ModuleName::main(),
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::ImportFrom(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["a.b.c".into(), "foo".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
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
        let code = compile_at_module(text, module_name.clone());

        let expected = CodeObject {
            module_name,
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["pkg.outer".into(), "foo".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn selective_import_relative_two_layers_from_pkg() {
        let text = r#"
from .outer.inner import foo
"#;
        let module_name = ModuleName::from_segments(&["pkg", "mod"]);
        let code = compile_at_module(text, module_name.clone());

        let expected = CodeObject {
            module_name,
            name: "<module>".into(),
            filename: "<stdin>".into(),
            bytecode: vec![
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ],
            arg_count: 0,
            varnames: vec![],
            freevars: vec![],
            names: vec!["pkg.outer.inner".into(), "foo".into()],
            constants: vec![],
            line_map: vec![],
            function_type: FunctionType::Regular,
        };

        assert_code_eq!(code, expected);
    }

    #[test]
    fn selective_import_relative_one_layer_from_pkg_too_many_levels() {
        let text = r#"
from ..outer import foo
"#;
        let module_name = ModuleName::from_segments(&["pkg", "mod"]);
        let err = compile_err_at_module(text, module_name);

        match err {
            CompilerError::ImportError(msg) => assert_eq!(
                msg,
                "attempted relative import beyond top-level package".to_string()
            ),
            _ => panic!("Expected an ImportError"),
        }
    }
}
