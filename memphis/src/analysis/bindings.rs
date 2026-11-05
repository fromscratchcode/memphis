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
            _ => {}
        }
    }
    bindings
}
