use std::collections::HashSet;

use crate::{
    domain::Identifier,
    parser::types::{Ast, AstParams, Statement, StatementKind},
};

pub fn collect_function_bindings(args: &AstParams, body: &Ast) -> HashSet<Identifier> {
    let mut bindings = collect_bindings(body);
    bindings.extend(args.names().cloned());
    bindings
}

// This works on a lexical scope, meaning it does not enter functions or classes
fn collect_bindings(body: &Ast) -> HashSet<Identifier> {
    let mut bindings = HashSet::new();
    for stmt in body.iter() {
        collect_bindings_from_stmt(stmt, &mut bindings);
    }
    bindings
}

fn collect_bindings_from_stmt(stmt: &Statement, bindings: &mut HashSet<Identifier>) {
    match &stmt.kind {
        StatementKind::Assignment { left, right: _ }
        | StatementKind::CompoundAssignment {
            target: left,
            op: _,
            value: _,
        } => {
            if let Some(name) = left.as_variable() {
                bindings.insert(name.clone());
            }
        }
        StatementKind::UnpackingAssignment { left, right: _ }
        | StatementKind::MultipleAssignment { left, right: _ } => {
            for name in left {
                if let Some(name) = name.as_variable() {
                    bindings.insert(name.clone());
                }
            }
        }
        StatementKind::IfElse {
            if_part,
            elif_parts,
            else_part,
        } => {
            for stmt in if_part.ast.iter() {
                collect_bindings_from_stmt(stmt, bindings);
            }
            for elif_part in elif_parts {
                for stmt in elif_part.ast.iter() {
                    collect_bindings_from_stmt(stmt, bindings);
                }
            }
            if let Some(else_part) = else_part {
                for stmt in else_part.iter() {
                    collect_bindings_from_stmt(stmt, bindings);
                }
            }
        }
        StatementKind::FunctionDef {
            name,
            args: _,
            body: _,
            decorators: _,
            is_async: _,
        }
        | StatementKind::ClassDef {
            name,
            parents: _,
            metaclass: _,
            body: _,
        } => {
            bindings.insert(name.clone());
        }
        StatementKind::WhileLoop(ast) => {
            for stmt in ast.ast.iter() {
                collect_bindings_from_stmt(stmt, bindings);
            }
        }
        StatementKind::ForInLoop {
            index,
            iterable,
            body,
            else_block,
        } => todo!(),
        StatementKind::TryExcept {
            try_block,
            handlers,
            else_block,
            finally_block,
        } => todo!(),
        StatementKind::ContextManager {
            expr,
            variable,
            block,
        } => todo!(),
        StatementKind::RegularImport(_) => todo!(),
        StatementKind::SelectiveImport { import_path, mode } => todo!(),
        StatementKind::Pass
        | StatementKind::Break
        | StatementKind::Continue
        | StatementKind::Assert(_)
        | StatementKind::Delete(_)
        | StatementKind::Return(_)
        | StatementKind::Nonlocal(_)
        | StatementKind::Global(_)
        | StatementKind::Raise(_)
        | StatementKind::Expression(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::*, types::*};

    use super::*;

    #[test]
    fn direct_assignment() {
        let body = ast![stmt_assign!(var!("x"), int!(10))];
        assert_eq!(collect_bindings(&body), HashSet::from([ident!("x")]));
    }

    #[test]
    fn compound_assignment() {
        let body = ast![stmt_compound_assign!(var!("x"), Add, int!(10))];
        assert_eq!(collect_bindings(&body), HashSet::from([ident!("x")]));
    }

    #[test]
    fn unpacking_assignment() {
        let body = ast![stmt_unpacking!(vec![var!("x"), var!("y")], int!(10))];
        assert_eq!(
            collect_bindings(&body),
            HashSet::from([ident!("x"), ident!("y")])
        );
    }

    #[test]
    fn multiple_assignment() {
        let body = ast![stmt_multiple_assign!(vec![var!("x"), var!("y")], int!(10))];
        assert_eq!(
            collect_bindings(&body),
            HashSet::from([ident!("x"), ident!("y")])
        );
    }

    #[test]
    fn if_else() {
        let body = ast![stmt!(StatementKind::IfElse {
            if_part: ConditionalAst::new(
                cmp_op!(var!("x"), GreaterThan, int!(0)),
                ast![stmt_assign!(var!("a"), int!(10))]
            ),
            elif_parts: vec![ConditionalAst::new(
                cmp_op!(var!("x"), GreaterThan, int!(-10)),
                ast![stmt_assign!(var!("b"), int!(10))]
            )],
            else_part: Some(ast![stmt_assign!(var!("c"), int!(10))]),
        })];
        assert_eq!(
            collect_bindings(&body),
            HashSet::from([ident!("a"), ident!("b"), ident!("c")])
        );
    }

    #[test]
    fn function_def() {
        let body = ast![stmt_def!(
            "foo",
            params!(),
            [stmt_assign!(var!("x"), int!(10))]
        )];
        assert_eq!(collect_bindings(&body), HashSet::from([ident!("foo")]));
    }

    #[test]
    fn class_def() {
        let body = ast![stmt_class!("Foo", [stmt_assign!(var!("x"), int!(10))])];
        assert_eq!(collect_bindings(&body), HashSet::from([ident!("Foo")]));
    }

    #[test]
    fn while_loop() {
        let body = ast![stmt!(StatementKind::WhileLoop(ConditionalAst::new(
            bool!(true),
            ast![stmt_assign!(var!("a"), int!(10))]
        )))];
        assert_eq!(collect_bindings(&body), HashSet::from([ident!("a")]));
    }
}
