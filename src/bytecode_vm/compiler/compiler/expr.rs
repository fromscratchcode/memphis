use crate::{
    bytecode_vm::{
        compiler::{Constant, JumpKind, Opcode},
        Compiler, CompilerError, CompilerResult,
    },
    domain::Identifier,
    parser::types::{BinOp, CallArgs, Callee, CompareOp, DictOperation, Expr, LogicalOp, UnaryOp},
};

impl Compiler {
    pub fn compile_expr(&mut self, expr: &Expr) -> CompilerResult<()> {
        match expr {
            Expr::None => {
                self.compile_none();
                Ok(())
            }
            Expr::Boolean(value) => {
                self.compile_bool(*value);
                Ok(())
            }
            Expr::Integer(value) => {
                self.compile_int(*value);
                Ok(())
            }
            Expr::Float(value) => {
                self.compile_float(*value);
                Ok(())
            }
            Expr::StringLiteral(value) => {
                self.compile_string_literal(value);
                Ok(())
            }
            Expr::Variable(name) => {
                self.compile_load(name);
                Ok(())
            }
            Expr::List(items) => self.compile_list(items),
            Expr::Tuple(items) => self.compile_tuple(items),
            Expr::Dict(dict_op) => self.compile_dict(dict_op),
            Expr::UnaryOperation { op, right } => self.compile_unary_op(op, right),
            Expr::BinaryOperation { left, op, right } => self.compile_binary_op(left, op, right),
            Expr::ComparisonChain { left, ops } => self.compile_comparison_chain(left, ops),
            Expr::LogicalOperation { left, op, right } => self.compile_logical_op(left, op, right),
            Expr::MemberAccess { object, field } => self.compile_member_access(object, field),
            Expr::FunctionCall { callee, args } => self.compile_function_call(callee, args),
            Expr::Yield(value) => self.compile_yield(value),
            Expr::YieldFrom(value) => self.compile_yield_from(value),
            Expr::Await(expr) => self.compile_await(expr),
            _ => Err(CompilerError::Unsupported(format!(
                "Expression type: {expr:?}"
            ))),
        }
    }

    fn compile_none(&mut self) {
        self.compile_constant(Constant::None);
    }

    fn compile_bool(&mut self, bool: bool) {
        self.compile_constant(Constant::Boolean(bool));
    }

    fn compile_int(&mut self, int: i64) {
        self.compile_constant(Constant::Int(int));
    }

    fn compile_float(&mut self, float: f64) {
        self.compile_constant(Constant::Float(float));
    }

    fn compile_string_literal(&mut self, value: &str) {
        self.compile_constant(Constant::String(value.to_string()));
    }

    fn compile_list(&mut self, items: &[Expr]) -> CompilerResult<()> {
        self.compile_expr_slice(items)?;
        self.emit(Opcode::BuildList(items.len()));
        Ok(())
    }

    fn compile_tuple(&mut self, items: &[Expr]) -> CompilerResult<()> {
        self.compile_expr_slice(items)?;
        self.emit(Opcode::BuildTuple(items.len()));
        Ok(())
    }

    fn compile_dict(&mut self, dict_ops: &[DictOperation]) -> CompilerResult<()> {
        for dict_op in dict_ops.iter() {
            let (key, value) = match dict_op {
                DictOperation::Pair(key, value) => (key, value),
                DictOperation::Unpack(_) => unimplemented!("Unpacking not yet supported."),
            };
            self.compile_expr(key)?;
            self.compile_expr(value)?;
        }
        self.emit(Opcode::BuildMap(dict_ops.len()));
        Ok(())
    }

    fn compile_unary_op(&mut self, op: &UnaryOp, right: &Expr) -> CompilerResult<()> {
        self.compile_expr(right)?;

        let opcode = match op {
            UnaryOp::Minus => Some(Opcode::UnaryNegative),
            // this acts as a no-op. can be overridden with __pos__ for custom classes
            UnaryOp::Plus => None,
            UnaryOp::Not => Some(Opcode::UnaryNot),
            UnaryOp::BitwiseNot => Some(Opcode::UnaryInvert),
            _ => return Err(CompilerError::Unsupported(format!("unary op: {op:?}"))),
        };
        if let Some(opcode) = opcode {
            self.emit(opcode);
        }
        Ok(())
    }

    fn compile_binary_op(
        &mut self,
        left: &Expr,
        bin_op: &BinOp,
        right: &Expr,
    ) -> CompilerResult<()> {
        self.compile_expr(left)?;
        self.compile_expr(right)?;

        let opcode = Opcode::try_from_bin_op(bin_op)
            .ok_or_else(|| CompilerError::Unsupported(format!("binary op: {bin_op:?}")))?;

        self.emit(opcode);
        Ok(())
    }

    /// Pseudocode for an operator chain:
    /// evaluate left
    /// for each (op, right):
    ///     evaluate right
    ///     compare op
    ///     if false: goto end
    ///     if not last iteration: pop true
    ///     else: leave it
    /// end:
    fn compile_comparison_chain(
        &mut self,
        left: &Expr,
        ops: &[(CompareOp, Expr)],
    ) -> CompilerResult<()> {
        if ops.is_empty() {
            panic!("Comparison chain must have >= 1 op.");
        }

        let chain_end = {
            let frame = self.frame_mut();
            frame.new_label()
        };

        self.compile_expr(left)?;

        for (i, (op, right)) in ops.iter().enumerate() {
            let last_op = i == ops.len() - 1;

            self.compile_expr(right)?;

            // Preserve the right-hand side for the next comparison
            if !last_op {
                self.emit(Opcode::DupTop);
                self.emit(Opcode::RotThree);
            }

            self.emit(Opcode::from(op));

            // If any comparison evaluates to False, jump to end.
            // Otherwise, pop the True and continue the chain.
            // Unless it's the last operation, and we should leave the result on the stack.
            if !last_op {
                self.frame_mut()
                    .emit_jump_to(chain_end, JumpKind::JumpIfFalse);

                self.emit(Opcode::PopTop);
            }
        }

        self.frame_mut().bind_label(chain_end);

        Ok(())
    }

    fn compile_logical_op(
        &mut self,
        left: &Expr,
        op: &LogicalOp,
        right: &Expr,
    ) -> CompilerResult<()> {
        let jump_kind = match op {
            LogicalOp::And => JumpKind::JumpIfFalse,
            LogicalOp::Or => JumpKind::JumpIfTrue,
        };

        // Compile the first operand.
        self.compile_expr(left)?;

        let condition_end = {
            let frame = self.frame_mut();
            frame.new_label()
        };

        self.frame_mut().emit_jump_to(condition_end, jump_kind);

        // Discard the first operand if we got this far.
        self.emit(Opcode::PopTop);

        // Compile the second operand.
        self.compile_expr(right)?;

        self.frame_mut().bind_label(condition_end);

        Ok(())
    }

    fn compile_member_access(&mut self, object: &Expr, field: &Identifier) -> CompilerResult<()> {
        self.compile_expr(object)?;
        let attr_index = self.get_or_set_nonlocal_index(field.as_str());
        self.emit(Opcode::LoadAttr(attr_index));
        Ok(())
    }

    fn compile_function_call(&mut self, callee: &Callee, args: &CallArgs) -> CompilerResult<()> {
        match callee {
            Callee::Expr(callee) => self.compile_expr(callee)?,
            Callee::Symbol(name) => self.compile_load(name),
        };

        // We push the args onto the stack in reverse call order so that we will pop
        // them off in call order.
        for arg in args.args.iter().rev() {
            self.compile_expr(arg)?;
        }

        self.emit(Opcode::Call(args.args.len()));
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Option<Box<Expr>>) -> CompilerResult<()> {
        if let Some(expr) = expr {
            self.compile_expr(expr)?;
        }

        self.emit(Opcode::YieldValue);
        Ok(())
    }

    fn compile_yield_from(&mut self, expr: &Expr) -> CompilerResult<()> {
        self.compile_expr(expr)?;
        self.emit(Opcode::YieldFrom);
        Ok(())
    }

    fn compile_await(&mut self, expr: &Expr) -> CompilerResult<()> {
        self.compile_expr(expr)?;
        self.emit(Opcode::Await);
        Ok(())
    }

    fn compile_expr_slice(&mut self, items: &[Expr]) -> CompilerResult<()> {
        for item in items {
            self.compile_expr(item)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests_bytecode_expr {
    use super::*;

    use crate::{
        bytecode_vm::{
            compiler::{test_utils::*, Bytecode},
            indices::Index,
        },
        parser::test_utils::*,
    };

    pub fn compile_expr(expr: Expr) -> Bytecode {
        let mut bytecode = compile_stmt(stmt_expr!(expr));

        // Some expressions end with a PopTop since we are not saving their value here, we are safe
        // to strip that away for tests.
        if let Some(&Opcode::PopTop) = bytecode.last() {
            bytecode.pop();
        }

        bytecode
    }

    #[test]
    fn expression() {
        let expr = bin_op!(int!(4), Mul, bin_op!(int!(2), Add, int!(3)));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LoadConst(Index::new(2)),
                Opcode::Add,
                Opcode::Mul,
            ]
        );
    }

    #[test]
    fn binary_expressions_mathematical_op() {
        let expr = bin_op!(int!(4), Add, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Add,
            ]
        );

        let expr = bin_op!(int!(4), Sub, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Sub,
            ]
        );

        let expr = bin_op!(int!(4), Mul, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Mul,
            ]
        );

        let expr = bin_op!(int!(4), Div, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Div,
            ]
        );

        let expr = bin_op!(int!(2), Add, var!("a"));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::Add,
            ]
        );
    }

    #[test]
    fn binary_expressions_compare_op() {
        let expr = cmp_op!(int!(4), Equals, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Eq,
            ]
        );

        let expr = cmp_op!(int!(4), NotEquals, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Ne,
            ]
        );

        let expr = cmp_op!(int!(4), Is, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Is,
            ]
        );

        let expr = cmp_op!(int!(4), IsNot, float!(5.1));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::IsNot,
            ]
        );

        let expr = cmp_op!(int!(4), LessThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::LessThan,
            ]
        );

        let expr = cmp_op!(int!(4), GreaterThan, int!(5));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::GreaterThan,
            ]
        );

        let expr = cmp_op!(int!(4), In, list![int!(5)]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(1),
                Opcode::In,
            ]
        );

        let expr = cmp_op!(int!(4), NotIn, list![int!(5)]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(1),
                Opcode::NotIn,
            ]
        );
    }

    #[test]
    fn operator_chaining() {
        let expr = cmp_chain!(int!(4), [(Equals, float!(5.1))]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Eq,
            ]
        );

        let expr = cmp_chain!(int!(4), [(Equals, float!(5.1)), (Equals, int!(4))]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::DupTop,
                Opcode::RotThree,
                Opcode::Eq,
                Opcode::JumpIfFalse(3),
                Opcode::PopTop,
                Opcode::LoadConst(Index::new(0)),
                Opcode::Eq,
            ]
        );
    }

    #[test]
    fn binary_expressions_logical_op() {
        let expr = logic_op!(bool!(true), And, bool!(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::JumpIfFalse(2),
                Opcode::PopTop,
                Opcode::LoadConst(Index::new(1)),
            ]
        );

        let expr = logic_op!(bool!(true), Or, bool!(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::JumpIfTrue(2),
                Opcode::PopTop,
                Opcode::LoadConst(Index::new(1)),
            ]
        );
    }

    #[test]
    fn unary_operations() {
        let expr = unary_op!(Minus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryNegative]
        );

        let expr = unary_op!(Plus, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(bytecode, &[Opcode::LoadConst(Index::new(0))]);

        let expr = unary_op!(Not, Expr::Boolean(false));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryNot]
        );

        let expr = unary_op!(BitwiseNot, int!(4));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadConst(Index::new(0)), Opcode::UnaryInvert]
        );
    }

    #[test]
    fn lists() {
        let expr = list![int!(2), int!(3)];
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildList(2),
            ]
        );
    }

    #[test]
    fn tuples() {
        let expr = tuple![int!(2), int!(3)];
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildTuple(2),
            ]
        );
    }

    #[test]
    fn dictionaries() {
        let expr = dict![dict_pair!(str!("a"), int!(1))];
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::BuildMap(1),
            ]
        );
    }

    #[test]
    fn await_expr() {
        let expr = await_expr!(var!("x"));
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadGlobal(Index::new(0)), Opcode::Await,]
        );
    }

    #[test]
    fn member_access() {
        let expr = member_access!(var!("foo"), "x");
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1))
            ]
        );
    }

    #[test]
    fn function_call_user_defined() {
        let expr = func_call!("foo", call_args![var!("a"), var!("b")]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::LoadGlobal(Index::new(2)),
                Opcode::Call(2),
            ]
        );
    }

    #[test]
    fn function_call_builtin() {
        let expr = func_call!("list", call_args![]);
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[Opcode::LoadGlobal(Index::new(0)), Opcode::Call(0),]
        );
    }

    #[test]
    fn method_call() {
        let expr = func_call_callee!(
            member_access!(var!("foo"), "bar"),
            call_args![int!(88), int!(99)]
        );
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadAttr(Index::new(1)),
                Opcode::LoadConst(Index::new(0)),
                Opcode::LoadConst(Index::new(1)),
                Opcode::Call(2),
            ]
        );
    }

    #[test]
    fn function_call_with_callee() {
        let expr = func_call_callee!(
            func_call!("test_decorator", call_args![var!("get_val_undecorated")]),
            call_args![]
        );
        let bytecode = compile_expr(expr);
        assert_eq!(
            bytecode,
            &[
                Opcode::LoadGlobal(Index::new(0)),
                Opcode::LoadGlobal(Index::new(1)),
                Opcode::Call(1),
                Opcode::Call(0),
            ]
        );
    }
}
