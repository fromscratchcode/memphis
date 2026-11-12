use crate::{
    analysis::{AcceptsVisitor, bindings::collect_function_bindings},
    domain::Identifier,
    parser::types::{Ast, FromImportMode, Statement, StatementKind},
};
use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
};

use super::visitor::Visitor;

#[derive(Clone, Debug)]
pub enum SemanticError {
    ReturnOutsideFunction,
    ImportStarNotAtModuleLevel,
    NonlocalAtModuleLevel,
    SymbolNotFoundInNonlocalEnvironment(String),
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            SemanticError::ReturnOutsideFunction => write!(f, "'return' outside function"),
            SemanticError::ImportStarNotAtModuleLevel => {
                write!(f, "import * only allowed at module level")
            }
            SemanticError::NonlocalAtModuleLevel => {
                write!(f, "'nonlocal' cannot be used at module level")
            }
            SemanticError::SymbolNotFoundInNonlocalEnvironment(symbol) => {
                write!(f, "'{symbol}' not found in nonlocal environment")
            }
        }
    }
}

struct FunctionScope {
    bindings: HashSet<Identifier>,
}

impl FunctionScope {
    fn new(bindings: HashSet<Identifier>) -> Self {
        Self { bindings }
    }

    fn contains(&self, name: &Identifier) -> bool {
        self.bindings.contains(name)
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
    function_scopes: Vec<FunctionScope>,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        Self {
            context_stack: vec![Context::Module],
            function_scopes: vec![],
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

    fn validate_return(&mut self) {
        if !matches!(self.context(), Context::Function) {
            self.errors.push(SemanticError::ReturnOutsideFunction);
        }
    }

    fn validate_selective_import(&mut self, mode: &FromImportMode) {
        if mode.is_all() && !matches!(self.context(), Context::Module) {
            self.errors.push(SemanticError::ImportStarNotAtModuleLevel);
        }
    }

    fn validate_nonlocal(&mut self, names: &[Identifier]) {
        let enclosing_scopes = match self.context() {
            Context::Module => {
                self.errors.push(SemanticError::NonlocalAtModuleLevel);
                return;
            }
            Context::Class if self.function_scopes.is_empty() => {
                self.errors.push(SemanticError::NonlocalAtModuleLevel);
                return;
            }
            // the nearest function must exist and enclose this class body
            Context::Class => &self.function_scopes,
            // skip ourselves because the current function cannot satify its own nonlocal, but
            // go all the way because global scope is not in this list
            Context::Function => &self.function_scopes[..self.function_scopes.len() - 1],
        };

        for name in names {
            let found = enclosing_scopes
                .iter()
                .rev()
                .any(|scope| scope.contains(name));

            if !found {
                self.errors
                    .push(SemanticError::SymbolNotFoundInNonlocalEnvironment(
                        name.to_string(),
                    ));
            }
        }
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Return(_) => self.validate_return(),
            StatementKind::SelectiveImport {
                import_path: _,
                mode,
            } => self.validate_selective_import(mode),
            StatementKind::Nonlocal(names) => self.validate_nonlocal(names),
            _ => {}
        }
    }

    fn enter_function_body(&mut self, statement: &Statement) {
        self.context_stack.push(Context::Function);
        let StatementKind::FunctionDef {
            name: _,
            args,
            body,
            decorators: _,
            is_async: _,
        } = &statement.kind
        else {
            panic!("Expected statement");
        };
        let bindings = collect_function_bindings(args, body);
        let function_scope = FunctionScope::new(bindings);
        self.function_scopes.push(function_scope);
    }

    fn exit_function_body(&mut self, _statement: &Statement) {
        self.function_scopes.pop();
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
    fn return_outside_function_invalid() {
        let ast = ast![stmt_return!()];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::ReturnOutsideFunction));
    }

    #[test]
    fn return_inside_function_valid() {
        let ast = ast![stmt_def!("foo", params!(), [stmt_return!()])];
        validate(&ast).expect("Expected no error");
    }

    #[test]
    fn return_inside_block_inside_function_valid() {
        let ast = ast![stmt_def!(
            "foo",
            params!(),
            [stmt!(StatementKind::IfElse {
                if_part: ConditionalAst::new(bool!(true), ast![stmt_return!()],),
                elif_parts: vec![],
                else_part: None,
            })]
        )];
        validate(&ast).expect("Expected no error");
    }

    #[test]
    fn return_inside_class_body_invalid() {
        let ast = ast![stmt_class!("Foo", [stmt_return!()])];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::ReturnOutsideFunction));
    }

    #[test]
    fn return_inside_method_valid() {
        let ast = ast![stmt_class!(
            "Foo",
            [stmt_def!("bar", params!(), [stmt_return!()])]
        )];
        validate(&ast).expect("Expected no error");
    }

    #[test]
    fn nonlocal_at_module_level() {
        let ast = ast![stmt_nonlocal!()];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::NonlocalAtModuleLevel));
    }

    #[test]
    fn nonlocal_not_found() {
        // no enclosing binding
        let ast = ast![stmt_def!("foo", params!(), [stmt_nonlocal!("x")])];
        let err = validate(&ast).expect_err("Expected error");
        assert!(
            matches!(err, SemanticError::SymbolNotFoundInNonlocalEnvironment(sym) if sym == "x")
        );

        // module-level binding only
        let ast = ast![
            stmt_assign!(var!("x"), int!(10)),
            stmt_def!("foo", params!(), [stmt_nonlocal!("x")])
        ];
        let err = validate(&ast).expect_err("Expected error");
        assert!(
            matches!(err, SemanticError::SymbolNotFoundInNonlocalEnvironment(sym) if sym == "x")
        );

        let ast = ast![stmt_def!(
            "outer",
            params!(),
            [
                stmt_assign!(var!("x"), int!(10)),
                stmt_def!("inner", params!(), [stmt_nonlocal!("x")])
            ]
        )];
        validate(&ast).expect("Expected no error");

        let ast = ast![stmt_def!(
            "outer",
            params!(),
            [
                stmt_def!("inner", params!(), [stmt_nonlocal!("x")]),
                stmt_assign!(var!("x"), int!(10))
            ]
        )];
        validate(&ast).expect("Expected no error");

        let ast = ast![stmt_def!(
            "outer",
            params!(param!("x")),
            [stmt_def!("inner", params!(), [stmt_nonlocal!("x")])]
        )];
        validate(&ast).expect("Expected no error");

        let ast = ast![stmt_def!(
            "outer",
            params!(),
            [
                stmt_assign!(var!("x"), int!(10)),
                stmt_def!(
                    "middle",
                    params!(),
                    [stmt_def!("inner", params!(), [stmt_nonlocal!("x")])]
                )
            ]
        )];
        validate(&ast).expect("Expected no error");
    }

    #[test]
    fn nonlocal_in_class_body_can_bind_an_enclosing_function_local() {
        let ast = ast![stmt_def!(
            "outer",
            params!(),
            [
                stmt_assign!(var!("x"), int!(10)),
                stmt_class!("Inner", [stmt_nonlocal!("x")]),
            ]
        )];
        validate(&ast).expect("Expected no error");
    }

    #[test]
    fn nonlocal_in_module_level_class_is_invalid() {
        let ast = ast![stmt_class!("Inner", [stmt_nonlocal!("x")])];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::NonlocalAtModuleLevel));
    }

    #[test]
    fn import_star_not_allowed_inside_class() {
        let ast = ast![stmt_class!(
            "Foo",
            [stmt_from_import!("x", from_import_all!())]
        )];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::ImportStarNotAtModuleLevel));
    }

    #[test]
    fn import_star_not_allowed_inside_function() {
        let ast = ast![stmt_def!(
            "foo",
            params!(),
            [stmt_from_import!("x", from_import_all!())]
        )];
        let err = validate(&ast).expect_err("Expected error");
        assert!(matches!(err, SemanticError::ImportStarNotAtModuleLevel));
    }
}
