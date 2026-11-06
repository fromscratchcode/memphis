use std::collections::HashSet;

use crate::{
    domain::Identifier,
    parser::types::{Ast, AstParams, Expr, StatementKind},
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
        match &stmt.kind {
            StatementKind::Assignment { left, right: _ } => {
                match left {
                    Expr::Variable(v) => {
                        bindings.insert(v.clone());
                    }
                    _ => {}
                };
            }
            StatementKind::UnpackingAssignment { left, right: _ }
            | StatementKind::MultipleAssignment { left, right: _ } => {
                for name in left {
                    match name {
                        Expr::Variable(v) => {
                            bindings.insert(v.clone());
                        }
                        _ => {}
                    };
                }
            }
            _ => {}
        }
    }
    bindings
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
}
