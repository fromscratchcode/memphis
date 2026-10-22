use crate::{
    analysis::AcceptsVisitor,
    parser::types::{Ast, Statement, StatementKind},
};
use std::fmt::{Display, Error, Formatter};

use super::visitor::Visitor;

#[derive(Clone, Debug)]
pub enum SemanticError {
    ReturnOutsideFunction,
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            SemanticError::ReturnOutsideFunction => write!(f, "'return' outside function"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Context {
    Module,
    Function,
    Class,
}

pub struct SemanticAnalyzer {
    context_stack: Vec<Context>,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        Self {
            context_stack: vec![Context::Module],
            errors: Vec::new(),
        }
    }

    fn first_error(&self) -> Option<&SemanticError> {
        self.errors.first()
    }

    fn context(&self) -> &Context {
        self.context_stack.last().expect("Context stack underflow")
    }

    fn exit_context(&mut self, expected: Context) {
        let actual = self.context_stack.pop().expect("Context stack underflow");
        assert_eq!(
            actual, expected,
            "Semantic context stack exited a different scope than it entered"
        );
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_statement(&mut self, statement: &Statement) {
        if matches!(statement.kind, StatementKind::Return(_))
            && !matches!(self.context(), Context::Function)
        {
            self.errors.push(SemanticError::ReturnOutsideFunction);
        }
    }

    fn enter_function_body(&mut self, _statement: &Statement) {
        self.context_stack.push(Context::Function);
    }

    fn exit_function_body(&mut self, _statement: &Statement) {
        self.exit_context(Context::Function);
    }

    fn enter_class_body(&mut self, _statement: &Statement) {
        self.context_stack.push(Context::Class);
    }

    fn exit_class_body(&mut self, _statement: &Statement) {
        self.exit_context(Context::Class);
    }
}

pub fn validate(ast: &Ast) -> Result<(), SemanticError> {
    let mut visitor = SemanticAnalyzer::new();
    ast.accept(&mut visitor);
    match visitor.first_error() {
        Some(e) => Err(e.clone()),
        None => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{test_utils::*, types::*};

    #[test]
    fn regular_statement() {
        let ast = ast![stmt_pass!()];
        validate(&ast).expect("Expected no error");
    }

    #[test]
    fn return_outside_function() {
        let ast = ast![stmt_return!()];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::ReturnOutsideFunction));
    }
}
