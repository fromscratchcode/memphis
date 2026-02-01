use crate::{
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
        self.consume_optional_many(&Token::Newline);
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
                let block = self.parse_indented_block()?;
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

        self.consume_optional_many(&Token::Newline);
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
            self.consume_current()?;

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
                ast: self.parse_indented_block()?,
            };

            // We must use push because these will be evaluated in order
            elif_parts.push(elif_parts_part);
        }

        let else_part = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
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
        let body = self.parse_indented_block()?;

        let else_block = if self.current_token() == &Token::Else {
            self.consume(&Token::Else)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
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
        let try_block = self.parse_indented_block()?;

        let mut handlers: Vec<ExceptHandler> = vec![];
        while self.current_token() == &Token::Except {
            self.consume(&Token::Except)?;
            if self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                let block = self.parse_indented_block()?;
                handlers.push(ExceptHandler::default(block));
            } else {
                let expr = self.parse_simple_expr()?;
                let alias = self.parse_optional_alias()?;
                self.consume(&Token::Colon)?;
                let block = self.parse_indented_block()?;
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
            Some(self.parse_indented_block()?)
        } else {
            None
        };

        let finally_block = if self.current_token() == &Token::Finally {
            self.consume(&Token::Finally)?;
            self.consume(&Token::Colon)?;
            Some(self.parse_indented_block()?)
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
        let block = self.parse_indented_block()?;

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
}
