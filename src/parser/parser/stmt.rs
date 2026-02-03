use crate::{
    core::{log, LogLevel},
    domain::Identifier,
    lexer::Token,
    parser::{
        types::{
            CompoundOperator, ConditionalAst, ExceptHandler, Expr, FromImportItem, FromImportMode,
            LoopIndex, RaiseKind, RegularImport, Statement, StatementKind,
        },
        Parser, ParserError,
    },
};

impl Parser<'_> {
    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        log(LogLevel::Trace, || "parse_statement".to_string());

        let start_line = self.line_number;
        let stmt = match self.current_token() {
            Token::Del => self.parse_delete(),
            Token::Def | Token::AtSign | Token::Async => self.parse_function_definition(),
            Token::Assert => {
                self.consume(&Token::Assert)?;
                let expr = self.parse_simple_expr()?;
                Ok(StatementKind::Assert(expr))
            }
            Token::Class => self.parse_class_definition(),
            Token::Return => self.parse_return(),
            Token::Pass => {
                self.consume(&Token::Pass)?;
                Ok(StatementKind::Pass)
            }
            Token::Break => {
                self.consume(&Token::Break)?;
                Ok(StatementKind::Break)
            }
            Token::Continue => {
                self.consume(&Token::Continue)?;
                Ok(StatementKind::Continue)
            }
            Token::Nonlocal => {
                self.consume(&Token::Nonlocal)?;
                let identifiers = self.parse_identifiers()?;
                Ok(StatementKind::Nonlocal(identifiers))
            }
            Token::Global => {
                self.consume(&Token::Global)?;
                let identifiers = self.parse_identifiers()?;
                Ok(StatementKind::Global(identifiers))
            }
            Token::If => self.parse_if_else(),
            Token::While => {
                self.consume(&Token::While)?;
                let condition = self.parse_simple_expr()?;
                self.consume(&Token::Colon)?;
                let block = self.parse_block()?;
                Ok(StatementKind::WhileLoop(ConditionalAst {
                    condition,
                    ast: block,
                }))
            }
            Token::For => self.parse_for_in_loop(),
            Token::Import => self.parse_regular_import(),
            Token::From => self.parse_selective_import(),
            Token::Try => self.parse_try_except(),
            Token::Raise => self.parse_raise(),
            Token::With => self.parse_context_manager(),
            _ => self.parse_statement_without_starting_keyword(),
        }?;

        Ok(Statement::new(start_line, stmt))
    }

    fn parse_statement_without_starting_keyword(&mut self) -> Result<StatementKind, ParserError> {
        let left = self.parse_expr()?;

        if self.current_token() == &Token::Assign {
            self.consume(&Token::Assign)?;
            match left {
                Expr::Tuple(vars) => Ok(StatementKind::UnpackingAssignment {
                    left: vars,
                    right: self.parse_expr()?,
                }),
                _ => {
                    let mut left_items = vec![left];

                    let mut right = self.parse_expr()?;
                    while self.current_token() == &Token::Assign {
                        self.consume(&Token::Assign)?;
                        left_items.push(right);
                        right = self.parse_expr()?;
                    }

                    if left_items.len() > 1 {
                        Ok(StatementKind::MultipleAssignment {
                            left: left_items,
                            right,
                        })
                    } else {
                        Ok(StatementKind::Assignment {
                            left: left_items[0].clone(),
                            right,
                        })
                    }
                }
            }
        } else if self.current_token().is_compound_assign() {
            let operator = match self.current_token() {
                Token::PlusEquals => CompoundOperator::Add,
                Token::MinusEquals => CompoundOperator::Subtract,
                Token::AsteriskEquals => CompoundOperator::Multiply,
                Token::SlashEquals => CompoundOperator::Divide,
                Token::BitwiseAndEquals => CompoundOperator::BitwiseAnd,
                Token::BitwiseOrEquals => CompoundOperator::BitwiseOr,
                Token::BitwiseXorEquals => CompoundOperator::BitwiseXor,
                Token::DoubleSlashEquals => CompoundOperator::IntegerDiv,
                Token::LeftShiftEquals => CompoundOperator::LeftShift,
                Token::RightShiftEquals => CompoundOperator::RightShift,
                Token::ModEquals => CompoundOperator::Mod,
                Token::MatMulEquals => CompoundOperator::MatMul,
                Token::ExpoEquals => CompoundOperator::Expo,
                _ => unreachable!(),
            };
            self.consume_current();

            let value = self.parse_simple_expr()?;
            Ok(StatementKind::CompoundAssignment {
                operator,
                target: Box::new(left),
                value: Box::new(value),
            })
        } else {
            Ok(StatementKind::Expression(left))
        }
    }

    fn parse_delete(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Del)?;
        let exprs = self.parse_comma_separated_expr()?;
        Ok(StatementKind::Delete(exprs))
    }

    fn parse_return(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Return)?;
        let exprs = if self.end_of_statement() {
            vec![]
        } else {
            self.parse_comma_separated_expr()?
        };
        Ok(StatementKind::Return(exprs))
    }

    fn parse_function_definition(&mut self) -> Result<StatementKind, ParserError> {
        let mut decorators: Vec<Expr> = vec![];

        while self.current_token() == &Token::AtSign {
            self.consume(&Token::AtSign)?;
            decorators.push(self.parse_simple_expr()?);

            // Each decorator must be ended by 1 and only 1 newline
            self.consume(&Token::Newline)?;
        }

        let is_async = if self.current_token() == &Token::Async {
            self.consume(&Token::Async)?;
            true
        } else {
            false
        };

        self.consume(&Token::Def)?;
        let name = self.parse_identifier()?;
        self.consume(&Token::LParen)?;
        let args = self.parse_function_def_args(Token::RParen)?;
        self.consume(&Token::RParen)?;

        // Support type hints in the return type
        if self.current_token() == &Token::ReturnTypeArrow {
            self.consume(&Token::ReturnTypeArrow)?;
            let _ = self.parse_simple_expr()?;
        }

        self.consume(&Token::Colon)?;
        let body = self.parse_block()?;

        Ok(StatementKind::FunctionDef {
            name,
            args,
            body,
            decorators,
            is_async,
        })
    }

    fn parse_class_definition(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Class)?;
        let name = self.parse_identifier()?;

        let mut parents = vec![];
        let mut metaclass = None;

        if self.current_token() == &Token::LParen {
            self.consume(&Token::LParen)?;
            while self.current_token() != &Token::RParen {
                let mc_ident = Identifier::new("metaclass").unwrap();
                if self
                    .tokens
                    .peek_ahead_contains(&[Token::Identifier(mc_ident.clone()), Token::Assign])
                {
                    // Support for metaclasses, i.e. the `__new__` method which constructs a class
                    // (instead of an object like the normal `__new__` method).
                    //
                    // Context: PEP 3115 (https://peps.python.org/pep-3115/)
                    // ```
                    // class ABC(metaclass=ABCMeta):
                    //     pass
                    // ```
                    self.consume(&Token::Identifier(mc_ident))?;
                    self.consume(&Token::Assign)?;
                    metaclass = Some(self.parse_identifier()?);
                    break;
                }

                parents.push(self.parse_parent_class()?);

                self.consume_optional(&Token::Comma);
            }
            self.consume(&Token::RParen)?;
        }

        self.consume(&Token::Colon)?;
        let body = self.parse_block()?;

        Ok(StatementKind::ClassDef {
            name,
            parents,
            metaclass,
            body,
        })
    }

    fn parse_if_else(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::If)?;
        let condition = self.parse_simple_expr()?;
        self.consume(&Token::Colon)?;
        let if_part = ConditionalAst {
            condition,
            ast: self.parse_block()?,
        };

        let mut elif_parts = vec![];
        while self.current_token() == &Token::Elif {
            self.consume(&Token::Elif)?;
            let condition = self.parse_simple_expr()?;
            self.consume(&Token::Colon)?;
            let elif_parts_part = ConditionalAst {
                condition,
                ast: self.parse_block()?,
            };

            // We must use push because these will be evaluated in order
            elif_parts.push(elif_parts_part);
        }

        let else_part = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(StatementKind::IfElse {
            if_part,
            elif_parts,
            else_part,
        })
    }

    fn parse_for_in_loop(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::For)?;

        let index_a = self.parse_identifier()?;
        let index = if self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            let index_b = self.parse_identifier()?;
            LoopIndex::Tuple(vec![index_a, index_b])
        } else {
            LoopIndex::Variable(index_a)
        };

        self.consume(&Token::In)?;
        let range = self.parse_simple_expr()?;
        self.consume(&Token::Colon)?;
        let body = self.parse_block()?;

        let else_block = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(StatementKind::ForInLoop {
            index,
            iterable: range,
            body,
            else_block,
        })
    }

    fn parse_try_except(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Try)?;
        self.consume(&Token::Colon)?;
        let try_block = self.parse_block()?;

        let mut handlers: Vec<ExceptHandler> = vec![];
        while self.current_token() == &Token::Except {
            self.consume(&Token::Except)?;
            if self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                let block = self.parse_block()?;
                handlers.push(ExceptHandler::default(block));
            } else {
                let expr = self.parse_simple_expr()?;
                let alias = self.parse_optional_alias()?;
                self.consume(&Token::Colon)?;
                let block = self.parse_block()?;
                handlers.push(ExceptHandler::typed(expr, alias, block));
            }
        }

        if handlers
            .iter()
            .take(handlers.len() - 1)
            .any(|h| h.is_default())
        {
            return Err(ParserError::syntax_error("default 'except:' must be last"));
        }

        let else_block = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        let finally_block = if self.current_token() == &Token::Finally {
            self.consume(&Token::Finally)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        if handlers.is_empty() && finally_block.is_none() {
            return Err(ParserError::syntax_error("invalid try/except"));
        }

        Ok(StatementKind::TryExcept {
            try_block,
            handlers,
            else_block,
            finally_block,
        })
    }

    fn parse_regular_import(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Import)?;

        let mut items = vec![];
        loop {
            let module_path = self.parse_module_path()?;
            let alias = self.parse_optional_alias()?;
            items.push(RegularImport { module_path, alias });

            if self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;
            } else {
                break;
            }
        }

        Ok(StatementKind::RegularImport(items))
    }

    fn parse_selective_import(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::From)?;
        let import_path = self.parse_import_path()?;

        self.consume(&Token::Import)?;
        let stmt = match self.current_token() {
            Token::Asterisk => {
                self.consume(&Token::Asterisk)?;
                StatementKind::SelectiveImport {
                    import_path,
                    mode: FromImportMode::All,
                }
            }
            _ => {
                self.consume_optional(&Token::LParen);

                let mut items = Vec::new();
                loop {
                    let symbol = self.parse_identifier()?;
                    let alias = self.parse_optional_alias()?;

                    let item = alias.map_or(FromImportItem::direct(symbol.clone()), |a| {
                        FromImportItem::aliased(symbol, a)
                    });
                    items.push(item);

                    if self.end_of_statement() {
                        break;
                    }

                    match self.current_token() {
                        Token::Comma => {
                            self.consume(&Token::Comma)?;
                            continue;
                        }
                        Token::RParen => {
                            self.consume(&Token::RParen)?;
                            break;
                        }
                        _ => {
                            return Err(ParserError::ExpectedToken(
                                Token::Comma,
                                self.current_token().clone(),
                            ));
                        }
                    }
                }

                StatementKind::SelectiveImport {
                    import_path,
                    mode: FromImportMode::List(items),
                }
            }
        };

        Ok(stmt)
    }

    fn parse_context_manager(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::With)?;
        let expr = self.parse_simple_expr()?;

        let variable = if self.current_token() == &Token::As {
            self.consume(&Token::As)?;
            Some(self.parse_identifier()?)
        } else {
            None
        };
        self.consume(&Token::Colon)?;
        let block = self.parse_block()?;

        Ok(StatementKind::ContextManager {
            expr,
            variable,
            block,
        })
    }

    fn parse_raise(&mut self) -> Result<StatementKind, ParserError> {
        self.consume(&Token::Raise)?;

        if self.end_of_statement() {
            return Ok(StatementKind::Raise(RaiseKind::Reraise));
        }

        let exception = self.parse_expr()?;

        if self.current_token() == &Token::From {
            self.consume(&Token::From)?;
            let cause = self.parse_expr()?;
            Ok(StatementKind::Raise(RaiseKind::RaiseFrom {
                exception,
                cause,
            }))
        } else {
            Ok(StatementKind::Raise(RaiseKind::Raise(exception)))
        }
    }

    /// Parse a parent class looking for one of the following syntaxes:
    /// ```python
    /// class Foo(Bar): pass
    /// class Foo(module.Baz): pass
    /// ```
    ///
    /// We use `parse_simple_expr` here because we do not want to catch any Expr::Tuple, which
    /// would be returned for multiple inheritance if we used `parse_expr`.
    // TODO is this right?? I wonder if this should wait until runtime to throw an error.
    fn parse_parent_class(&mut self) -> Result<Expr, ParserError> {
        let parent = self.parse_simple_expr()?;

        if !matches!(parent, Expr::Variable(_) | Expr::MemberAccess { .. }) {
            Err(ParserError::syntax_error("invalid parent"))
        } else {
            Ok(parent)
        }
    }

    fn parse_comma_separated_expr(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut exprs = vec![];
        loop {
            let expr = self.parse_simple_expr()?;
            exprs.push(expr);

            if self.current_token() != &Token::Comma {
                break;
            }
            self.consume(&Token::Comma)?;
        }

        Ok(exprs)
    }

    fn parse_identifiers(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut items = vec![self.parse_identifier()?];
        while self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            items.push(self.parse_identifier()?);
        }
        Ok(items)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{
        test_utils::*,
        types::{
            ast, CompoundOperator, ConditionalAst, ExceptHandler, LoopIndex, Params, StatementKind,
        },
    };

    fn ident(input: &str) -> Identifier {
        Identifier::new(input).expect("Invalid identifier")
    }

    #[test]
    fn variable_assignment() {
        let input = "a = 2";
        let expected_ast = stmt_assign!(var!("a"), int!(2));
        assert_stmt_eq!(input, expected_ast);

        let input = "b = a + 3";
        let expected_ast = stmt_assign!(var!("b"), bin_op!(var!("a"), Add, int!(3)));
        assert_stmt_eq!(input, expected_ast);

        let input = "a, b = (1, 2)";
        let expected_ast = stmt!(StatementKind::UnpackingAssignment {
            left: vec![var!("a"), var!("b")],
            right: tuple![int!(1), int!(2)],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a[1] = 0";
        let expected_ast = stmt_assign!(index_access!(var!("a"), int!(1)), int!(0));
        assert_stmt_eq!(input, expected_ast);

        let input = "x = yield from a";
        let expected_ast = stmt_assign!(var!("x"), yield_from!(var!("a")));
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn function_definition() {
        let input = "
def add(x, y):
    return x + y
";
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("add"),
            args: params![param!("x"), param!("y")],
            body: ast![stmt_return![bin_op!(var!("x"), Add, var!("y"))]],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "def _f(): pass";
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("_f"),
            args: params![],
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
def __init__(
    self, *, indent=None,
):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("__init__"),
            args: params![param!("self"), param!("indent", none!())],
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn return_statement() {
        let input = "return a, b";
        let expected_ast = stmt_return![var!("a"), var!("b")];
        assert_stmt_eq!(input, expected_ast);

        let input = "return None";
        let expected_ast = stmt_return![none!()];
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn if_else() {
        let input = r#"
if x > 0:
    print("Greater")
elif x > -10:
    print("Medium")
else:
    print("Less")
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(var!("x"), GreaterThan, int!(0)),
                ast: ast![stmt_expr!(func_call!("print", call_args![str!("Greater")]))],
            },
            elif_parts: vec![ConditionalAst {
                condition: cmp_op!(var!("x"), GreaterThan, int!(-10)),
                ast: ast![stmt_expr!(func_call!("print", call_args![str!("Medium")]))],
            }],
            else_part: Some(ast![stmt_expr!(func_call!(
                "print",
                call_args![str!("Less")]
            ))]),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
if x > 0:
    print("Greater")
elif x > -10:
    print("Medium")
elif x > -20:
    print("Less")
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(var!("x"), GreaterThan, int!(0)),
                ast: ast![stmt_expr!(func_call!("print", call_args![str!("Greater")]))],
            },
            elif_parts: vec![
                ConditionalAst {
                    condition: cmp_op!(var!("x"), GreaterThan, int!(-10)),
                    ast: ast![stmt_expr!(func_call!("print", call_args![str!("Medium")]))],
                },
                ConditionalAst {
                    condition: cmp_op!(var!("x"), GreaterThan, int!(-20)),
                    ast: ast![stmt_expr!(func_call!("print", call_args![str!("Less")]))],
                },
            ],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
if x > 0:
    print("Greater")
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: cmp_op!(var!("x"), GreaterThan, int!(0)),
                ast: ast![stmt_expr!(func_call!("print", call_args![str!("Greater")]))],
            },
            elif_parts: vec![],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
if True: return False
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bool!(true),
                ast: ast![stmt_return![bool!(false)]],
            },
            elif_parts: vec![],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
if (a == 1
        and b
        and c):
    pass
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: logic_op!(
                    logic_op!(cmp_op!(var!("a"), Equals, int!(1)), And, var!("b")),
                    And,
                    var!("c")
                ),
                ast: ast![stmt_pass!()],
            },
            elif_parts: vec![],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
if (a
    or b):
    pass
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: logic_op!(var!("a"), Or, var!("b")),
                ast: ast![stmt_pass!()],
            },
            elif_parts: vec![],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn while_loop() {
        let input = "
while True:
    print(\"busy loop\")
";
        let expected_ast = stmt!(StatementKind::WhileLoop(ConditionalAst {
            condition: bool!(true),
            ast: ast![stmt_expr!(func_call!(
                "print",
                call_args![str!("busy loop")]
            ))],
        }));
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn class_definition() {
        let input = r#"
class Foo:
    def __init__(self):
        self.x = 0

    def bar(self):
        print(self.x)
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![],
            metaclass: None,
            body: ast![
                stmt!(StatementKind::FunctionDef {
                    name: ident("__init__"),
                    args: params![param!("self")],
                    body: ast![stmt_assign!(member_access!(var!("self"), "x"), int!(0))],
                    decorators: vec![],
                    is_async: false,
                }),
                stmt!(StatementKind::FunctionDef {
                    name: ident("bar"),
                    args: params![param!("self")],
                    body: ast![stmt_expr!(func_call!(
                        "print",
                        call_args![member_access!(var!("self"), "x")]
                    ))],
                    decorators: vec![],
                    is_async: false,
                }),
            ],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "class Foo(Bar, Baz): pass";
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![var!("Bar"), var!("Baz")],
            metaclass: None,
            body: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "class Foo(module.Bar): pass";
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![member_access!(var!("module"), "Bar")],
            metaclass: None,
            body: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn regular_import() {
        let input = "import other";
        let expected_ast = stmt_reg_import![import!("other")];

        assert_stmt_eq!(input, expected_ast);

        // We test this inside a function so that it will attempt to parse more than one statement,
        // which is what originally caught the bug related to parsing the comma and beyond.
        let input = r#"
def foo():
    import a, b as c
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("foo"),
            args: params![],
            body: ast![stmt_reg_import![import!("a"), import!("b", "c"),]],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
import other as b
pass
"#;
        // Before we handling Token::As processing, this test would fail, but only once it began
        // parsing the next statement. We needed to parse two statements here to produce the
        // failing test.
        let expected_ast = ast![stmt_reg_import![import!("other", "b")], stmt_pass!()];
        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn selective_import() {
        let input = "from other import a";
        let expected_ast = stmt_from_import!("other", from_import_list![from_import_item!("a")]);
        assert_stmt_eq!(input, expected_ast);

        let input = "from other import a, b";
        let expected_ast = stmt_from_import!(
            "other",
            from_import_list![from_import_item!("a"), from_import_item!("b")]
        );
        assert_stmt_eq!(input, expected_ast);

        let input = "from other import *";
        let expected_ast = stmt_from_import!("other", from_import_all!());
        assert_stmt_eq!(input, expected_ast);

        let input = "from other import a, b as c";
        let expected_ast = stmt_from_import!(
            "other",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );
        assert_stmt_eq!(input, expected_ast);

        let input = "from other.module import a, b as c";
        let expected_ast = stmt_from_import!(
            "other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );
        assert_stmt_eq!(input, expected_ast);

        let input = "from . import a";
        let expected_ast = stmt_from_import!(".", from_import_list![from_import_item!("a")]);
        assert_stmt_eq!(input, expected_ast);

        let input = "from .other.module import a, b as c";
        let expected_ast = stmt_from_import!(
            ".other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );
        assert_stmt_eq!(input, expected_ast);

        let input = "from ..other.module import a, b as c";
        let expected_ast = stmt_from_import!(
            "..other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
from ..other.module import (a,
                            b as c)
"#;
        let expected_ast = stmt_from_import!(
            "..other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
from ..other.module import (a as b,
                            c)
"#;
        let expected_ast = stmt_from_import!(
            "..other.module",
            from_import_list![from_import_item!("a", "b"), from_import_item!("c")]
        );
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn for_in_loop() {
        let input = r#"
for i in a:
    print(i)
"#;
        let expected_ast = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Variable(ident("i")),
            iterable: var!("a"),
            body: ast![stmt_expr!(func_call!("print", call_args![var!("i")]))],
            else_block: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
for k, v in a.items():
    print(v)
"#;
        let expected_ast = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Tuple(vec![ident("k"), ident("v")]),
            iterable: method_call!(var!("a"), "items"),
            body: ast![stmt_expr!(func_call!("print", call_args![var!("v")]))],
            else_block: None,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn inheritance() {
        let input = r#"
class Foo(Parent):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![var!("Parent")],
            metaclass: None,
            body: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
class Foo(metaclass=Parent):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![],
            metaclass: Some(ident("Parent")),
            body: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
class Foo(Bar, metaclass=Parent):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![var!("Bar")],
            metaclass: Some(ident("Parent")),
            body: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
class InterfaceMeta(type):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("InterfaceMeta"),
            parents: vec![var!("type")],
            metaclass: None,
            body: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn async_await() {
        let input = r#"
async def main():
    task_1 = asyncio.create_task(task1())
    return await task_1
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("main"),
            args: params![],
            body: ast![
                stmt_assign!(
                    var!("task_1"),
                    method_call!(
                        var!("asyncio"),
                        "create_task",
                        call_args![func_call!("task1")]
                    )
                ),
                stmt_return![await_expr!(var!("task_1"))],
            ],
            decorators: vec![],
            is_async: true,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let expected_ast = stmt!(StatementKind::Assert(bool!(true)));
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn try_except_finally() {
        let input = r#"
try:
    4 / 0
except:
    a = 2
finally:
    a = 3
"#;
        let expected_ast = stmt!(StatementKind::TryExcept {
            try_block: ast![stmt_expr!(bin_op!(int!(4), Div, int!(0)))],
            handlers: vec![ExceptHandler::default(ast![stmt_assign!(
                var!("a"),
                int!(2)
            )])],
            else_block: None,
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except ZeroDivisionError as e:
    a = 2
finally:
    a = 3
"#;
        let expected_ast = stmt!(StatementKind::TryExcept {
            try_block: ast![stmt_expr!(bin_op!(int!(4), Div, int!(0)))],
            handlers: vec![ExceptHandler::typed(
                var!("ZeroDivisionError"),
                Some(ident("e")),
                ast![stmt_assign!(var!("a"), int!(2))]
            )],
            else_block: None,
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except (ZeroDivisionError, IOError) as e:
    a = 2
"#;
        let expected_ast = stmt!(StatementKind::TryExcept {
            try_block: ast![stmt_expr!(bin_op!(int!(4), Div, int!(0)))],
            handlers: vec![ExceptHandler::typed(
                tuple![var!("ZeroDivisionError"), var!("IOError")],
                Some(ident("e")),
                ast![stmt_assign!(var!("a"), int!(2))]
            )],
            else_block: None,
            finally_block: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except excs():
    a = 2
"#;
        let expected_ast = stmt!(StatementKind::TryExcept {
            try_block: ast![stmt_expr!(bin_op!(int!(4), Div, int!(0)))],
            handlers: vec![ExceptHandler::typed(
                func_call!("excs", call_args![]),
                None,
                ast![stmt_assign!(var!("a"), int!(2))]
            )],
            else_block: None,
            finally_block: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
try:
    4 / 0
except ZeroDivisionError as e:
    a = 2
else:
    a = 4
finally:
    a = 3
"#;
        let expected_ast = stmt!(StatementKind::TryExcept {
            try_block: ast![stmt_expr!(bin_op!(int!(4), Div, int!(0)))],
            handlers: vec![ExceptHandler::typed(
                var!("ZeroDivisionError"),
                Some(ident("e")),
                ast![stmt_assign!(var!("a"), int!(2))]
            )],
            else_block: Some(ast![stmt_assign!(var!("a"), int!(4))]),
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
try:
    pass
except:
    return
a = 1
"#;
        let expected_ast = ast![
            stmt!(StatementKind::TryExcept {
                try_block: ast![stmt_pass!()],
                handlers: vec![ExceptHandler::default(ast![stmt_return![]])],
                else_block: None,
                finally_block: None,
            }),
            stmt_assign!(var!("a"), int!(1))
        ];
        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn try_except_default_handler_before_typed() {
        let input = r#"
try:
    pass
except:
    return
except ZeroDivisionError:
    return
"#;
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("default 'except:' must be last".to_string())
        );
    }

    #[test]
    fn try_except_two_default_handlers() {
        let input = r#"
try:
    pass
except:
    return
except:
    return
"#;
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("default 'except:' must be last".to_string())
        );
    }

    #[test]
    fn args_and_kwargs() {
        let input = r#"
def test_args(*args):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("test_args"),
            args: Params {
                args: vec![],
                args_var: Some(ident("args")),
                kwargs_var: None,
            },
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
def test_args(*args, **kwargs):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("test_args"),
            args: Params {
                args: vec![],
                args_var: Some(ident("args")),
                kwargs_var: Some(ident("kwargs")),
            },
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
def test_kwargs(**kwargs):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("test_kwargs"),
            args: Params {
                args: vec![],
                args_var: None,
                kwargs_var: Some(ident("kwargs")),
            },
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
def test_default(file=None):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("test_default"),
            args: params![param!("file", none!())],
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn decorators() {
        let input = r#"
@test_decorator
def get_val():
    return 2
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("get_val"),
            args: params![],
            body: ast![stmt_return![int!(2)]],
            decorators: vec![var!("test_decorator")],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn raise() {
        let input = "raise";
        let expected_ast = stmt_raise!();
        assert_stmt_eq!(input, expected_ast);

        let input = "raise Exception";
        let expected_ast = stmt_raise!(var!("Exception"));
        assert_stmt_eq!(input, expected_ast);

        let input = r#"raise Exception("message")"#;
        let expected_ast = stmt_raise!(func_call!("Exception", call_args![str!("message")]));
        assert_stmt_eq!(input, expected_ast);

        let input = r#"raise Exception("message") from None"#;
        let expected_ast = stmt_raise!(
            func_call!("Exception", call_args![str!("message")]),
            none!()
        );
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn context_manager() {
        let input = r#"
with open('test.txt') as f:
    pass
"#;
        let expected_ast = stmt!(StatementKind::ContextManager {
            expr: func_call!("open", call_args![str!("test.txt")]),
            variable: Some(ident("f")),
            block: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
with open('test.txt'):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ContextManager {
            expr: func_call!("open", call_args![str!("test.txt")]),
            variable: None,
            block: ast![stmt_pass!()],
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn delete() {
        let input = "del a";
        let expected_ast = stmt!(StatementKind::Delete(vec![var!("a")]));
        assert_stmt_eq!(input, expected_ast);

        let input = "del a, b, c";
        let expected_ast = stmt!(StatementKind::Delete(vec![var!("a"), var!("b"), var!("c")]));
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn compound_operator() {
        let input = "a += 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Add,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a -= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Subtract,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a *= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Multiply,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a /= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Divide,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a &= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseAnd,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a |= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseOr,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a ^= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseXor,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a //= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::IntegerDiv,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a <<= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::LeftShift,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a >>= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::RightShift,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a %= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Mod,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a @= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::MatMul,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a **= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Expo,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn control_flow() {
        let input = r#"
for i in a:
    break
"#;
        let expected_ast = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Variable(ident("i")),
            iterable: var!("a"),
            body: ast![stmt!(StatementKind::Break)],
            else_block: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
for i in a:
    continue
"#;
        let expected_ast = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Variable(ident("i")),
            iterable: var!("a"),
            body: ast![stmt!(StatementKind::Continue)],
            else_block: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = r#"
for i in a:
    break
else:
    pass
"#;
        let expected_ast = stmt!(StatementKind::ForInLoop {
            index: LoopIndex::Variable(ident("i")),
            iterable: var!("a"),
            body: ast![stmt!(StatementKind::Break)],
            else_block: Some(ast![stmt_pass!()]),
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn type_hints() {
        let input = "
def add(x: str, y: str) -> str:
    pass
";
        // For now, we just ensure the type hints are ignored.
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("add"),
            args: params![param!("x"), param!("y")],
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn closures() {
        let input = "
def outer():
    a = 1
    b = 2
    def inner():
        b = 3
        print(a)
";
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("outer"),
            args: params![],
            body: ast![
                stmt_assign!(var!("a"), int!(1)),
                stmt_assign!(var!("b"), int!(2)),
                stmt!(StatementKind::FunctionDef {
                    name: ident("inner"),
                    args: params![],
                    body: ast![
                        stmt_assign!(var!("b"), int!(3)),
                        stmt_expr!(func_call!("print", call_args![var!("a")])),
                    ],
                    decorators: vec![],
                    is_async: false,
                }),
            ],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn scope_modifiers() {
        let input = "nonlocal var";
        let expected_ast = stmt!(StatementKind::Nonlocal(vec![ident("var")]));
        assert_stmt_eq!(input, expected_ast);

        let input = "nonlocal var, var2";
        let expected_ast = stmt!(StatementKind::Nonlocal(vec![ident("var"), ident("var2")]));
        assert_stmt_eq!(input, expected_ast);

        let input = "global var";
        let expected_ast = stmt!(StatementKind::Global(vec![ident("var")]));
        assert_stmt_eq!(input, expected_ast);

        let input = "global var, var2";
        let expected_ast = stmt!(StatementKind::Global(vec![ident("var"), ident("var2")]));
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn default_args() {
        let input = r#"
def foo(data=None):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("foo"),
            args: params![param!("data", none!())],
            body: ast![stmt_pass!()],
            decorators: vec![],
            is_async: false,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn unpacking_assignment() {
        let input = r#"
if True:
    a, b = b, a
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bool!(true),
                ast: ast![stmt!(StatementKind::UnpackingAssignment {
                    left: vec![var!("a"), var!("b")],
                    right: tuple![var!("b"), var!("a"),],
                })],
            },
            elif_parts: vec![],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);

        let input = "a, = b,";
        let expected_ast = stmt!(StatementKind::UnpackingAssignment {
            left: vec![var!("a")],
            right: tuple![var!("b")],
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn multiple_assignment() {
        let input = "a = b = True";
        let expected_ast = stmt!(StatementKind::MultipleAssignment {
            left: vec![var!("a"), var!("b")],
            right: bool!(true),
        });
        assert_stmt_eq!(input, expected_ast);
    }
}
