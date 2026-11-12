use crate::parser::types::{
    Ast, AstInvokeArgs, AstParams, Callee, ConditionalAst, DictOperation, ExceptHandler, Expr,
    ExprFormat, FStringPart, ForClause, HandlerKind, KwargsOperation, Param, RaiseKind,
    SliceParams, Statement, StatementKind,
};

/// Core visitor trait used to traverse the AST.
pub trait Visitor {
    fn visit_ast(&mut self, _program: &Ast) {}
    fn visit_statement(&mut self, _statement: &Statement) {}
    fn visit_expr(&mut self, _expr: &Expr) {}
    fn should_enter_function_body(&self) -> bool {
        true
    }

    fn enter_function_body(&mut self, _statement: &Statement) {}
    fn exit_function_body(&mut self, _statement: &Statement) {}

    fn enter_class_body(&mut self, _statement: &Statement) {}
    fn exit_class_body(&mut self, _statement: &Statement) {}
}

/// Trait for anything that can be visited by a `Visitor`.
pub trait AcceptsVisitor {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

impl AcceptsVisitor for Ast {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_ast(self);
        for statement in self.iter() {
            statement.accept(visitor);
        }
    }
}

impl AcceptsVisitor for ConditionalAst {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let ConditionalAst { condition, ast } = self;
        condition.accept(visitor);
        ast.accept(visitor);
    }
}

impl AcceptsVisitor for ExceptHandler {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let ExceptHandler { kind, block } = self;
        match kind {
            HandlerKind::Typed { expr, alias: _ } => {
                expr.accept(visitor);
            }
            HandlerKind::Default => {}
        }

        block.accept(visitor);
    }
}

impl AcceptsVisitor for Param {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let Param { arg: _, default } = self;
        if let Some(default) = default {
            default.accept(visitor);
        }
    }
}

impl AcceptsVisitor for AstParams {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let AstParams {
            positional_only,
            positional_or_keyword,
            args_var: _,
            keyword_only,
            kwargs_var: _,
        } = self;
        for param in positional_only {
            param.accept(visitor);
        }
        for param in positional_or_keyword {
            param.accept(visitor);
        }
        for param in keyword_only {
            param.accept(visitor);
        }
    }
}

impl AcceptsVisitor for DictOperation {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            DictOperation::Unpack(expr) => {
                expr.accept(visitor);
            }
            DictOperation::Pair(left, right) => {
                left.accept(visitor);
                right.accept(visitor);
            }
        }
    }
}

impl AcceptsVisitor for FStringPart {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            FStringPart::String(_) => {}
            FStringPart::Expr(expr) => {
                let ExprFormat { format: _, expr } = expr;
                expr.accept(visitor);
            }
        }
    }
}

impl AcceptsVisitor for SliceParams {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let SliceParams { start, stop, step } = self;
        if let Some(start) = start {
            start.accept(visitor);
        }
        if let Some(stop) = stop {
            stop.accept(visitor);
        }
        if let Some(step) = step {
            step.accept(visitor);
        }
    }
}

impl AcceptsVisitor for Callee {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            Callee::Symbol(_) => {}
            Callee::Expr(expr) => {
                expr.accept(visitor);
            }
        }
    }
}

impl AcceptsVisitor for AstInvokeArgs {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let AstInvokeArgs {
            positional,
            kwargs,
            args_var,
        } = self;
        for expr in positional {
            expr.accept(visitor);
        }
        for op in kwargs {
            op.accept(visitor);
        }
        if let Some(args_var) = args_var {
            args_var.accept(visitor);
        }
    }
}

impl AcceptsVisitor for KwargsOperation {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            KwargsOperation::Pair(_, expr) | KwargsOperation::Unpacking(expr) => {
                expr.accept(visitor);
            }
        }
    }
}

impl AcceptsVisitor for ForClause {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        let ForClause {
            index: _,
            iterable,
            condition,
        } = self;
        iterable.accept(visitor);
        if let Some(condition) = condition {
            condition.accept(visitor);
        }
    }
}

impl AcceptsVisitor for Statement {
    /// Visit this statement, then walk the AST to any nested blocks.
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_statement(self);

        let Statement {
            start_line: _,
            kind,
        } = self;

        match &kind {
            StatementKind::Pass | StatementKind::Break | StatementKind::Continue => {}
            StatementKind::Expression(expr) | StatementKind::Assert(expr) => {
                expr.accept(visitor);
            }
            StatementKind::Return(exprs) | StatementKind::Delete(exprs) => {
                for expr in exprs {
                    expr.accept(visitor);
                }
            }
            StatementKind::Assignment { left, right }
            | StatementKind::CompoundAssignment {
                target: left,
                op: _,
                value: right,
            } => {
                left.accept(visitor);
                right.accept(visitor);
            }
            StatementKind::MultipleAssignment { left, right }
            | StatementKind::UnpackingAssignment { left, right } => {
                for expr in left {
                    expr.accept(visitor);
                }
                right.accept(visitor);
            }
            StatementKind::FunctionDef {
                body,
                name: _,
                decorators,
                args,
                is_async: _,
            } => {
                for decorator in decorators {
                    decorator.accept(visitor);
                }
                args.accept(visitor);
                if visitor.should_enter_function_body() {
                    visitor.enter_function_body(self);
                    body.accept(visitor);
                    visitor.exit_function_body(self);
                }
            }
            StatementKind::WhileLoop(cond_ast) => {
                cond_ast.accept(visitor);
            }
            StatementKind::ForInLoop {
                body,
                index: _,
                iterable,
                else_block,
            } => {
                iterable.accept(visitor);
                body.accept(visitor);
                if let Some(else_block) = else_block {
                    else_block.accept(visitor);
                }
            }
            StatementKind::ContextManager {
                block,
                expr,
                variable: _,
            } => {
                expr.accept(visitor);
                block.accept(visitor);
            }
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                if_part.accept(visitor);
                for part in elif_parts {
                    part.accept(visitor);
                }

                if let Some(else_part) = else_part {
                    else_part.accept(visitor);
                }
            }
            StatementKind::ClassDef {
                name: _,
                parents,
                metaclass: _,
                body,
            } => {
                for parent in parents {
                    parent.accept(visitor);
                }
                visitor.enter_class_body(self);
                body.accept(visitor);
                visitor.exit_class_body(self);
            }
            StatementKind::Global(_) | StatementKind::Nonlocal(_) => {}
            StatementKind::RegularImport(_)
            | StatementKind::SelectiveImport {
                import_path: _,
                mode: _,
            } => {}
            StatementKind::Raise(kind) => match kind {
                RaiseKind::Reraise => {}
                RaiseKind::Raise(expr) => {
                    expr.accept(visitor);
                }
                RaiseKind::RaiseFrom { exception, cause } => {
                    exception.accept(visitor);
                    cause.accept(visitor);
                }
            },
            StatementKind::TryExcept {
                try_block,
                handlers,
                else_block,
                finally_block,
            } => {
                try_block.accept(visitor);
                for handler in handlers {
                    handler.accept(visitor);
                }
                if let Some(else_block) = else_block {
                    else_block.accept(visitor);
                }
                if let Some(finally_block) = finally_block {
                    finally_block.accept(visitor);
                }
            }
        }
    }
}

impl AcceptsVisitor for Expr {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_expr(self);

        match self {
            Expr::None | Expr::Ellipsis | Expr::NotImplemented => {}
            Expr::Integer(_)
            | Expr::Float(_)
            | Expr::Boolean(_)
            | Expr::Variable(_)
            | Expr::StringLiteral(_)
            | Expr::BytesLiteral(_) => {}
            Expr::List(exprs) | Expr::Set(exprs) | Expr::Tuple(exprs) => {
                for expr in exprs {
                    expr.accept(visitor);
                }
            }
            Expr::Dict(ops) => {
                for op in ops {
                    op.accept(visitor);
                }
            }
            Expr::FString(parts) => {
                for part in parts {
                    part.accept(visitor);
                }
            }
            Expr::Yield(expr) => {
                if let Some(expr) = expr {
                    expr.accept(visitor);
                }
            }
            Expr::YieldFrom(expr)
            | Expr::Await(expr)
            | Expr::MemberAccess {
                object: expr,
                field: _,
            }
            | Expr::UnaryOperation { op: _, right: expr } => {
                expr.accept(visitor);
            }
            Expr::BinaryOperation { left, op: _, right }
            | Expr::LogicalOperation { left, op: _, right } => {
                left.accept(visitor);
                right.accept(visitor);
            }
            Expr::IndexAccess { object, index } => {
                object.accept(visitor);
                index.accept(visitor);
            }
            Expr::ComparisonChain { left, ops } => {
                left.accept(visitor);
                for (_, expr) in ops {
                    expr.accept(visitor);
                }
            }
            Expr::TernaryOp {
                condition,
                if_value,
                else_value,
            } => {
                condition.accept(visitor);
                if_value.accept(visitor);
                else_value.accept(visitor);
            }
            Expr::SliceOperation { object, params } => {
                object.accept(visitor);
                params.accept(visitor);
            }
            Expr::FunctionCall { callee, args } => {
                callee.accept(visitor);
                args.accept(visitor);
            }
            Expr::Lambda { args, expr } => {
                args.accept(visitor);
                expr.accept(visitor);
            }
            Expr::ListComprehension { clauses, body }
            | Expr::SetComprehension { clauses, body }
            | Expr::GeneratorComprehension { clauses, body } => {
                for clause in clauses {
                    clause.accept(visitor);
                }
                body.accept(visitor);
            }
            Expr::DictComprehension {
                clauses,
                key_body,
                value_body,
            } => {
                for clause in clauses {
                    clause.accept(visitor);
                }
                key_body.accept(visitor);
                value_body.accept(visitor);
            }
            Expr::TypeNode(_) => {}
        }
    }
}
