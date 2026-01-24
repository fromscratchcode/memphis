use crate::{
    bytecode_vm::{
        compiler::{opcode::UnsignedOffset, CodeObject, Constant, Opcode},
        Compiler, CompilerError, CompilerResult,
    },
    domain::{resolve_import_path, FromImportPath, FunctionType, Identifier},
    parser::types::{
        Ast, ConditionalAst, Expr, FromImportMode, LoopIndex, Params, RegularImport, Statement,
        StatementKind,
    },
};

impl Compiler {
    pub fn compile_stmt(&mut self, stmt: &Statement) -> CompilerResult<()> {
        self.line_number = stmt.start_line;

        match &stmt.kind {
            StatementKind::Pass => {}
            StatementKind::Expression(expr) => {
                self.compile_expr(expr)?;

                match expr {
                    // Yield and YieldFrom are special cases where they don't leave a value on the
                    // stack. This still feels a bit odd to me.
                    Expr::Yield(_) | Expr::YieldFrom(_) | Expr::Await(_) => {}
                    _ => {
                        // If an expression is used as a statement, we must tell the VM to discard
                        // the result from the stack.
                        self.emit(Opcode::PopTop)?;
                    }
                }
            }
            StatementKind::Return(expr) => self.compile_return(expr)?,
            StatementKind::Assignment { left, right } => self.compile_assignment(left, right)?,
            StatementKind::WhileLoop(cond_ast) => self.compile_while_loop(cond_ast)?,
            StatementKind::ForInLoop {
                index,
                iterable,
                body,
                else_block,
            } => self.compile_for_in_loop(index, iterable, body, else_block)?,
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => self.compile_if_else(if_part, elif_parts, else_part)?,
            StatementKind::FunctionDef {
                name,
                args,
                body,
                decorators,
                is_async,
            } => self.compile_function_definition(name, args, body, decorators, is_async)?,
            StatementKind::ClassDef {
                name,
                parents,
                metaclass,
                body,
            } => self.compile_class_definition(name, parents, metaclass, body)?,
            StatementKind::RegularImport(items) => self.compile_regular_import(items)?,
            StatementKind::SelectiveImport { import_path, mode } => {
                self.compile_selective_import(import_path, mode)?
            }
            _ => {
                return Err(CompilerError::Unsupported(format!(
                    "Statement type: {stmt:?}"
                )))
            }
        };

        Ok(())
    }

    fn compile_return(&mut self, expr: &[Expr]) -> CompilerResult<()> {
        if expr.len() > 1 {
            return Err(CompilerError::Unsupported(
                "Multiple return values not yet supported in the bytecode VM.".to_string(),
            ));
        } else if expr.len() == 1 {
            self.compile_expr(&expr[0])?;
        }

        self.emit(Opcode::ReturnValue)?;
        Ok(())
    }

    fn compile_assignment(&mut self, left: &Expr, right: &Expr) -> CompilerResult<()> {
        match left {
            Expr::Variable(name) => {
                self.compile_expr(right)?;
                self.compile_store(name)?;
            }
            Expr::MemberAccess { object, field } => {
                // Push the object onto the stack
                self.compile_expr(object)?;
                // Push the value to be assigned onto the stack
                self.compile_expr(right)?;
                let attr_index = self.get_or_set_nonlocal_index(field.as_str())?;
                self.emit(Opcode::SetAttr(attr_index))?;
            }
            Expr::IndexAccess { .. } => {
                return Err(CompilerError::Unsupported(
                    "Index access assignment not yet supported in bytecode VM.".to_string(),
                ))
            }
            _ => {
                return Err(CompilerError::SyntaxError(
                    "cannot assign to that expression type here.".into(),
                ))
            }
        };

        Ok(())
    }

    fn compile_while_loop(&mut self, cond_ast: &ConditionalAst) -> CompilerResult<()> {
        let condition_start = self.current_offset()?;
        let post_condition_ph = self.compile_conditional_branch(cond_ast)?;

        // Unconditional jump back to the start of the condition
        let offset = self.backward_offset_from(condition_start)?;
        self.emit(Opcode::Jump(offset))?;

        // Update the JUMP_IF_FALSE offset now that we know the length of the loop body
        let offset = self.forward_offset_to(post_condition_ph)?;
        self.emit_at(post_condition_ph, Opcode::JumpIfFalse(offset))?;

        Ok(())
    }

    fn compile_for_in_loop(
        &mut self,
        index: &LoopIndex,
        iterable: &Expr,
        body: &Ast,
        else_block: &Option<Ast>,
    ) -> CompilerResult<()> {
        if else_block.is_some() {
            return Err(CompilerError::Unsupported(
                "'else' not yet supported for a for loop in bytecode VM.".to_string(),
            ));
        }
        let LoopIndex::Variable(index) = index else {
            return Err(CompilerError::Unsupported(
                "Tuple indicies not yet supported in bytecode VM.".to_string(),
            ));
        };
        self.compile_expr(iterable)?;
        self.emit(Opcode::GetIter)?;

        // where FOR_ITER will live
        let loop_header = self.current_offset()?;

        // Emit placeholder FOR_ITER with dummy offset
        let for_iter_placeholder = self.emit_placeholder()?;

        self.compile_store(index)?;
        self.compile_ast(body)?;

        // jump back to loop_header
        let jump_back_offset = self.backward_offset_from(loop_header)?;
        self.emit(Opcode::Jump(jump_back_offset))?;

        // patch FOR_ITER to jump to end_of_loop
        let for_iter_offset = self.forward_offset_to(for_iter_placeholder)?;
        self.emit_at(for_iter_placeholder, Opcode::ForIter(for_iter_offset))?;
        Ok(())
    }

    fn compile_if_else(
        &mut self,
        if_part: &ConditionalAst,
        elif_parts: &[ConditionalAst],
        else_part: &Option<Ast>,
    ) -> CompilerResult<()> {
        // This will collect placeholders for unconditional jumps at the end of each true branch
        let mut end_jump_placeholders = vec![];

        let mut post_condition_ph = self.compile_conditional_branch(if_part)?;

        if !elif_parts.is_empty() {
            // Jump over any elifs/else if the condition was true
            end_jump_placeholders.push(self.emit_placeholder()?);
        }

        // Compile each `elif`
        for (i, elif) in elif_parts.iter().enumerate() {
            // Patch the previous jump-if-false
            let offset = self.forward_offset_to(post_condition_ph)?;
            self.emit_at(post_condition_ph, Opcode::JumpIfFalse(offset))?;

            // Compile this elif condition and block
            let post_elif_condition_ph = self.compile_conditional_branch(elif)?;

            // Only emit a jump if this is not the last elif or else
            if i != elif_parts.len() - 1 || else_part.is_some() {
                end_jump_placeholders.push(self.emit_placeholder()?);
            }

            // Update for next loop iteration
            post_condition_ph = post_elif_condition_ph;
        }

        let offset = self.forward_offset_to(post_condition_ph)?;
        self.emit_at(post_condition_ph, Opcode::JumpIfFalse(offset))?;

        // Handle optional `else`
        if let Some(else_part) = else_part {
            self.compile_ast(else_part)?;
        }

        // Patch all end-of-true-branch jumps
        for placeholder in end_jump_placeholders {
            let offset = self.forward_offset_to(placeholder)?;
            self.emit_at(placeholder, Opcode::Jump(offset))?;
        }

        Ok(())
    }

    fn compile_function_definition(
        &mut self,
        name: &Identifier,
        args: &Params,
        body: &Ast,
        decorators: &[Expr],
        is_async: &bool,
    ) -> CompilerResult<()> {
        let function_type = if body.has_yield() {
            FunctionType::Generator
        } else if *is_async {
            FunctionType::Async
        } else {
            FunctionType::Regular
        };

        let varnames = args
            .args
            .iter()
            .map(|p| p.arg.to_string())
            .collect::<Vec<String>>();
        let code_object = CodeObject::new_function(
            name.as_str(),
            self.module_name.clone(),
            &self.filename,
            &varnames,
            function_type,
        );

        let code = self.compile_ast_with_code(body, code_object)?;

        // Compile decorators in reverse
        for decorator in decorators.iter().rev() {
            self.compile_expr(decorator)?;
        }

        // Make the function/closure itself out of the compiled code
        self.compile_function(code)?;

        // Apply the decorators - innermost outward
        for _ in decorators {
            // The 1 is for the function we are wrapping
            self.emit(Opcode::Call(1))?;
        }

        // Bind the final decorated function
        self.compile_store(name)?;
        Ok(())
    }

    fn compile_class_definition(
        &mut self,
        name: &Identifier,
        parents: &[Expr],
        metaclass: &Option<Identifier>,
        body: &Ast,
    ) -> CompilerResult<()> {
        if !parents.is_empty() {
            return Err(CompilerError::Unsupported(
                "Inheritance not yet supported in the bytecode VM.".to_string(),
            ));
        }
        if metaclass.is_some() {
            return Err(CompilerError::Unsupported(
                "Metaclasses are not yet supported in the bytecode VM.".to_string(),
            ));
        }

        let code_object = CodeObject::new_function(
            name.as_str(),
            self.module_name.clone(),
            &self.filename,
            &[],
            FunctionType::Regular,
        );
        let code = self.compile_ast_with_code(body, code_object)?;

        self.emit(Opcode::LoadBuildClass)?;
        self.compile_code(code)?;

        // subtract one to ignore Opcode::LoadBuildClass
        let num_args = 1;
        self.emit(Opcode::Call(num_args))?;

        self.compile_store(name)?;
        Ok(())
    }

    fn compile_regular_import(&mut self, items: &[RegularImport]) -> CompilerResult<()> {
        for item in items {
            let index = self.get_or_set_nonlocal_index(&item.module_path.as_str())?;

            if item.alias.is_some() {
                self.emit(Opcode::ImportFrom(index))?;
            } else {
                self.emit(Opcode::ImportName(index))?;
            }

            let symbol_index = item
                .alias
                .as_ref()
                .map(|alias| self.get_or_set_nonlocal_index(alias.as_str()))
                .unwrap_or_else(|| {
                    let head = item.module_path.head().expect("No head!");
                    self.get_or_set_nonlocal_index(head)
                })?;
            self.emit(Opcode::StoreGlobal(symbol_index))?;
        }
        Ok(())
    }

    fn compile_selective_import(
        &mut self,
        import_path: &FromImportPath,
        mode: &FromImportMode,
    ) -> CompilerResult<()> {
        let module_name = resolve_import_path(import_path, &self.package)
            .map_err(|e| CompilerError::import_error(e.message()))?;

        let index = self.get_or_set_nonlocal_index(&module_name.as_str())?;
        self.emit(Opcode::ImportFrom(index))?;

        match mode {
            FromImportMode::All => self.emit(Opcode::ImportAll)?,
            FromImportMode::List(items) => {
                for item in items {
                    let attr_index = self.get_or_set_nonlocal_index(item.original().as_str())?;
                    self.emit(Opcode::LoadAttr(attr_index))?;

                    let alias_index = self.get_or_set_nonlocal_index(item.imported().as_str())?;
                    self.emit(Opcode::StoreGlobal(alias_index))?;
                }
            }
        }

        Ok(())
    }

    /// Compiles a condition and block, returning the offset of the placeholder
    /// that should later be patched with a `JumpIfFalse`.
    fn compile_conditional_branch(
        &mut self,
        ast: &ConditionalAst,
    ) -> CompilerResult<UnsignedOffset> {
        self.compile_expr(&ast.condition)?;
        let placeholder = self.emit_placeholder()?;
        self.compile_ast(&ast.ast)?;
        Ok(placeholder)
    }

    /// Load a CodeObject and turn it into a function or closure.
    fn compile_function(&mut self, code: CodeObject) -> CompilerResult<()> {
        let free_vars = code.freevars.clone();
        self.compile_code(code)?;

        if free_vars.is_empty() {
            self.emit(Opcode::MakeFunction)?;
        } else {
            // We push the free vars onto the stack in reverse order so that we will pop
            // them off in order.
            for free_var in free_vars.iter().rev() {
                // TODO this is a hack, we should either treat these as identifiers or not!
                self.compile_load(&Identifier::new(free_var).unwrap())?;
            }
            self.emit(Opcode::MakeClosure(free_vars.len()))?;
        }
        Ok(())
    }

    fn compile_code(&mut self, code: CodeObject) -> CompilerResult<()> {
        self.compile_constant(Constant::Code(code))
    }
}

#[cfg(test)]
mod tests_bytecode_stmt {
    use super::*;

    use crate::{
        bytecode_vm::{compiler::test_utils::*, indices::Index},
        parser::{test_utils::*, types::ast},
    };

    fn ident(input: &str) -> Identifier {
        Identifier::new(input).expect("Invalid identifier")
    }

    #[test]
    fn assignment() {
        let s = stmt_assign!(var!("var"), bin_op!(int!(5), Sub, int!(2)));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Sub,
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );

        let s = stmt_assign!(var!("var"), str!("Hello World"));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );

        let s = stmt_assign!(var!("var"), Expr::None);
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn assign_member_access() {
        let s = stmt_assign!(member_access!(var!("foo"), "x"), int!(4));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::SetAttr(Index::new(1))
            ]
        );
    }
    #[test]
    fn expression_as_statement() {
        let s = stmt_expr!(func_call!("print"));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Call(0),
                Opcode::PopTop,
            ]
        );
    }

    #[test]
    fn while_loop() {
        let s = stmt!(StatementKind::WhileLoop(ConditionalAst {
            condition: cmp_op!(int!(4), LessThan, int!(5)),
            ast: ast![],
        }));
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
                Opcode::JumpIfFalse(1),
                Opcode::Jump(-5),
            ]
        );
    }

    #[test]
    fn for_in_loop() {
        let s = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Variable(ident("i")),
            iterable: list![int!(1), int!(2)],
            body: ast![stmt_assign!(var!("a"), int!(-1))],
            else_block: None
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
                Opcode::GetIter,
                Opcode::ForIter(4),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::Jump(-5),
            ]
        );
    }

    #[test]
    fn if_else_only_if() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), LessThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_else() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), LessThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-3))],
            },
            elif_parts: vec![],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
                Opcode::JumpIfFalse(2),
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::LoadConst(Index::new(3)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_elif() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(4)),
                ast: ast![stmt_assign!(var!("a"), int!(-2))],
            }],
            else_part: None,
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                // if: 0-2
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump to elif condition
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(6), // skip rest
                // elif: 7-9
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(2), // jump past elif block if false
                Opcode::LoadConst(Index::new(3)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn if_else_with_elif_and_else() {
        let s = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(5)),
                ast: ast![stmt_assign!(var!("a"), int!(-1))],
            },
            elif_parts: vec![ConditionalAst {
                condition: cmp_op!(int!(4), GreaterThan, int!(4)),
                ast: ast![stmt_assign!(var!("a"), int!(-2))],
            }],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        let bytecode = compile_stmt(s);
        assert_eq!(
            bytecode,
            &[
                // if: 0-2
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump to elif condition
                Opcode::LoadConst(Index::new(2)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(9), // skip rest
                // elif: 7-9
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::GreaterThan,
                Opcode::JumpIfFalse(3), // jump past elif block if false
                Opcode::LoadConst(Index::new(3)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::Jump(2), // skip else
                Opcode::LoadConst(Index::new(4)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn regular_import() {
        let expr = stmt_reg_import![import!("other")];
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
            ]
        );
    }

    #[test]
    fn regular_import_multiple_layers() {
        let expr = stmt_reg_import![import!("other.module")];
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn regular_import_alias() {
        let expr = stmt_reg_import![import!("other", "foo")];
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportFrom(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn regular_import_multiple() {
        let expr = stmt_reg_import![import!("first"), import!("second"),];
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportName(Index::new(0)),
                Opcode::StoreGlobal(Index::new(0)),
                Opcode::ImportName(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn regular_import_multiple_alias() {
        let expr = stmt_reg_import![import!("a", "b"), import!("c", "d"),];
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportFrom(Index::new(0)),
                Opcode::StoreGlobal(Index::new(1)),
                Opcode::ImportFrom(Index::new(2)),
                Opcode::StoreGlobal(Index::new(3)),
            ]
        );
    }

    #[test]
    fn selective_import_all() {
        let expr = stmt_from_import!("other", from_import_all!());
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[Opcode::ImportFrom(Index::new(0)), Opcode::ImportAll,]
        );
    }

    #[test]
    fn selective_import_single() {
        let expr = stmt_from_import!("other", from_import_list![from_import_item!("foo")]);
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(1)),
            ]
        );
    }

    #[test]
    fn selective_import_single_alias() {
        let expr = stmt_from_import!("other", from_import_list![from_import_item!("foo", "bar")]);
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(2)),
            ]
        );
    }

    #[test]
    fn selective_import_multiple_alias() {
        let expr = stmt_from_import!(
            "other",
            from_import_list![from_import_item!("a", "b"), from_import_item!("c", "d")]
        );
        let bytecode = compile_stmt(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::ImportFrom(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::StoreGlobal(Index::new(2)),
                Opcode::LoadAttr(Index::new(3)),
                Opcode::StoreGlobal(Index::new(4)),
            ]
        );
    }
}
