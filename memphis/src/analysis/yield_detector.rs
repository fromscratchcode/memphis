use crate::parser::types::Expr;

use super::visitor::Visitor;

pub struct YieldDetector {
    pub found_yield: bool,
}

impl YieldDetector {
    pub fn new() -> Self {
        Self { found_yield: false }
    }
}

impl Visitor for YieldDetector {
    fn visit_expr(&mut self, expr: &Expr) {
        if matches!(expr, Expr::Yield(_) | Expr::YieldFrom(_)) {
            self.found_yield = true;
        }
    }

    // We do not want to detect yields in nested functions, only the current function body.
    // Nested functions should use this visitor again in a later pass.
    fn should_enter_function_body(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::*, types::*};

    #[test]
    fn regular_statement() {
        let ast = ast![stmt_pass!()];
        assert!(!ast.has_yield());
    }

    #[test]
    fn yield_expr() {
        let ast = ast![stmt_expr!(yield_expr!())];
        assert!(ast.has_yield());
    }

    #[test]
    fn yield_from_expr() {
        let ast = ast![stmt_expr!(yield_from!(var!("a")))];
        assert!(ast.has_yield());
    }

    #[test]
    fn yield_on_rhs() {
        let ast = ast![stmt_assign!(var!("a"), yield_expr!())];
        assert!(ast.has_yield());
    }

    #[test]
    fn nested_function_does_not_make_outer_generator() {
        let ast = ast![stmt_def!(
            "inner",
            params![],
            [stmt_expr!(yield_expr!(int!(42))), stmt_return![int!(10)]]
        )];
        assert!(!ast.has_yield());
    }
}
