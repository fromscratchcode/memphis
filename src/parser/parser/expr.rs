use std::collections::HashSet;

use crate::{
    core::{log, LogLevel},
    domain::Identifier,
    lexer::Token,
    parser::{
        types::{
            BinOp, CallArg, CallArgs, Callee, CompareOp, DictOperation, Expr, ExprFormat,
            FStringPart, ForClause, FormatOption, KwargsOperation, LogicalOp, SliceParams,
            TypeNode, UnaryOp,
        },
        Parser, ParserError,
    },
};

impl Parser<'_> {
    /// Parse an expression in a context where tuples may be expected. A good option if you're not
    /// sure. By tuples here, we mean those that are not indicated by parentheses (those are
    /// handled by detecting a LParen in `parse_factor`).
    ///
    /// ```python
    /// 4, 5
    /// a = 4, 5
    /// a = 1,
    /// ```
    ///
    /// All other expression parsing is immediately delegated to `parse_simple_expr`.
    pub fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_expr".to_string());
        let left = self.parse_simple_expr()?;

        if self.current_token() == &Token::Comma {
            let mut items = vec![left];
            while self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;

                // We need this for the case of a trailing comma, which is most often used for a
                // tuple with a single element.
                //
                // The [`Token::Assign`] is when this happens on the LHS.
                if self.end_of_statement() || self.current_token() == &Token::Assign {
                    break;
                }
                items.push(self.parse_simple_expr()?);
            }

            Ok(Expr::Tuple(items))
        } else {
            Ok(left)
        }
    }

    /// Parse an expression where open tuples are not expected. If you need to support this in a
    /// given context (i.e. a = 4, 5), try `parse_expr`.
    pub fn parse_simple_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_simple_expr".to_string());
        if self.current_token() == &Token::Await {
            self.parse_await_expr()
        } else {
            self.parse_ternary_expr()
        }
    }

    fn parse_await_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_await_expr".to_string());
        self.consume(&Token::Await)?;
        let right = self.parse_ternary_expr()?;
        Ok(Expr::Await(Box::new(right)))
    }

    /// Implements the Python precedence order in reverse call stack order, meaning the operators
    /// evaluated last will be detected first during this recursive descent.
    ///
    /// Python precedence order is:
    /// - Exponentiation (**) - `parse_exponentiation`
    /// - Literals, Identifiers - `parse_factor`
    /// - Member Access, Index Access - `parse_access_operations`
    /// - Multiplication, Division, Modulo, and Comparison Operators - `parse_term`
    /// - Logical operators (AND/OR) - `parse_logical_term`
    /// - Addition, Subtraction - `parse_add_sub`
    /// - Bitwise Shifts (<<, >>) - `parse_bitwise_shift`
    /// - Bitwise AND (&), OR (|), XOR (^) - `parse_binary_expr`
    /// - Ternary Expression (inline-if) - `parse_ternary_expr`
    fn parse_ternary_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_ternary_expr".to_string());
        let if_value = self.parse_binary_expr()?;

        if self.current_token() == &Token::If {
            self.consume(&Token::If)?;
            let condition = self.parse_binary_expr()?;
            self.consume(&Token::Else)?;
            let else_value = self.parse_binary_expr()?;

            return Ok(Expr::TernaryOp {
                condition: Box::new(condition),
                if_value: Box::new(if_value),
                else_value: Box::new(else_value),
            });
        }

        Ok(if_value)
    }

    fn parse_binary_expr(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_binary_expr".to_string());
        let mut left = self.parse_bitwise_shift()?;

        while matches!(
            self.current_token(),
            Token::BitwiseAnd | Token::BitwiseOr | Token::BitwiseXor
        ) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_bitwise_shift()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_add_sub(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_add_sub".to_string());
        let mut left = self.parse_logical_term()?;

        while matches!(self.current_token(), Token::Plus | Token::Minus) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_logical_term()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_bitwise_shift(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_bitwise_shift".to_string());
        let mut left = self.parse_add_sub()?;

        while matches!(self.current_token(), Token::LeftShift | Token::RightShift) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_add_sub()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_member_access(&mut self, left: Expr) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_member_access".to_string());
        self.consume(&Token::Dot)?;
        let field = self.parse_identifier()?;

        Ok(Expr::MemberAccess {
            object: Box::new(left),
            field,
        })
    }

    fn parse_index_access(&mut self, left: Expr) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_index_access".to_string());
        self.consume(&Token::LBracket)?;
        // [::2]
        let params = if self
            .tokens
            .peek_ahead_contains(&[Token::Colon, Token::Colon])
        {
            self.consume(&Token::Colon)?;
            self.consume(&Token::Colon)?;
            let step = Some(self.parse_simple_expr()?);
            (true, None, None, step)
            // [:] - this syntax is useful to replace the items in a list without changing the
            // list's reference
        } else if self
            .tokens
            .peek_ahead_contains(&[Token::Colon, Token::RBracket])
        {
            self.consume(&Token::Colon)?;
            (true, None, None, None)
            // [:2]
        } else if self.tokens.peek_ahead_contains(&[Token::Colon]) {
            self.consume(&Token::Colon)?;
            let stop = Some(self.parse_simple_expr()?);
            (true, None, stop, None)
            // [2:]
            // if there is a Colon immediately before the next RBracket
        } else if self.tokens.has(&Token::Colon)
            && self.tokens.num_away(&Token::Colon)? + 1 == self.tokens.num_away(&Token::RBracket)?
        {
            let start = Some(self.parse_simple_expr()?);
            self.consume(&Token::Colon)?;
            (true, start, None, None)
            // [1:1:1] or [2:5]
            // if there is a Colon before the next RBracket
        } else if self.tokens.has(&Token::Colon)
            && self.tokens.num_away(&Token::Colon)? < self.tokens.num_away(&Token::RBracket)?
        {
            let start = Some(self.parse_simple_expr()?);
            self.consume(&Token::Colon)?;
            let stop = Some(self.parse_simple_expr()?);
            let step = if self.current_token() == &Token::Colon {
                self.consume(&Token::Colon)?;
                Some(self.parse_simple_expr()?)
            } else {
                None
            };
            (true, start, stop, step)
            // [1]
        } else {
            let index = Some(self.parse_simple_expr()?);
            (false, index, None, None)
        };
        self.consume(&Token::RBracket)?;

        if !params.0 {
            Ok(Expr::IndexAccess {
                object: Box::new(left),
                index: Box::new(params.1.unwrap()),
            })
        } else {
            Ok(Expr::SliceOperation {
                object: Box::new(left),
                params: Box::new(SliceParams {
                    start: params.1,
                    stop: params.2,
                    step: params.3,
                }),
            })
        }
    }

    /// This is recursive to the right to create a right-associativity binary operator.
    fn parse_exponentiation(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_exponentiation".to_string());
        let mut left = self.parse_factor()?;

        while self.current_token() == &Token::DoubleAsterisk {
            self.consume(&Token::DoubleAsterisk)?;
            let right = self.parse_exponentiation()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op: BinOp::Expo,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_access_operations(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_access_operations".to_string());
        let mut left = self.parse_exponentiation()?;

        while matches!(
            self.current_token(),
            Token::Dot | Token::LBracket | Token::LParen
        ) {
            left = match self.current_token() {
                Token::Dot => self.parse_member_access(left)?,
                Token::LBracket => self.parse_index_access(left)?,
                Token::LParen => {
                    let args = self.parse_function_call_args()?;
                    Expr::FunctionCall {
                        callee: Callee::Expr(Box::new(left)),
                        args,
                    }
                }
                _ => unreachable!(),
            };
        }

        Ok(left)
    }

    fn parse_logical_term(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_logical_term".to_string());
        let mut left = self.parse_term()?;

        while matches!(self.current_token(), Token::And | Token::Or) {
            let op = LogicalOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_term()?;
            left = Expr::LogicalOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_term".to_string());
        let mut left = self.parse_access_operations()?;

        while matches!(
            self.current_token(),
            Token::Asterisk | Token::Slash | Token::DoubleSlash | Token::Modulo | Token::AtSign
        ) {
            let op = BinOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
            self.consume_current()?;
            let right = self.parse_access_operations()?;
            left = Expr::BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        let mut cmp_ops = vec![];
        while matches!(
            self.current_token(),
            Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual
                | Token::Equal
                | Token::NotEqual
                | Token::In
                | Token::Is
        ) || self.tokens.peek_ahead_contains(&[Token::Not, Token::In])
            || self.tokens.peek_ahead_contains(&[Token::Is, Token::Not])
        {
            // Handle two tokens to produce one `BinOp::NotIn` operation. If this gets too messy,
            // we could look to move multi-word tokens into the lexer.
            let op = if self.tokens.peek_ahead_contains(&[Token::Not, Token::In]) {
                self.consume(&Token::Not)?;
                self.consume(&Token::In)?;
                CompareOp::NotIn
            } else if self.tokens.peek_ahead_contains(&[Token::Is, Token::Not]) {
                self.consume(&Token::Is)?;
                self.consume(&Token::Not)?;
                CompareOp::IsNot
            } else {
                let op =
                    CompareOp::try_from(self.current_token()).unwrap_or_else(|_| unreachable!());
                self.consume_current()?;
                op
            };

            // Since we are building a flat loop of compare ops, we call the next level down, NOT
            // parse_term again.
            let right = self.parse_access_operations()?;
            cmp_ops.push((op, right));
        }

        if !cmp_ops.is_empty() {
            left = Expr::ComparisonChain {
                left: Box::new(left),
                ops: cmp_ops,
            };
        }

        Ok(left)
    }

    fn parse_minus(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::Minus)?;
        match self.current_token().clone() {
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(Expr::Integer(-(i as i64)))
            }
            Token::FloatingPoint(i) => {
                self.consume(&Token::FloatingPoint(i))?;
                Ok(Expr::Float(-i))
            }
            _ => {
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Minus,
                    right: Box::new(right),
                })
            }
        }
    }

    /// The unary plus operator is a no-op for integers and floats, but exists to provide custom
    /// behaviors using `Dunder::Pos`.
    fn parse_plus(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::Plus)?;
        match self.current_token().clone() {
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(Expr::Integer(i as i64))
            }
            Token::FloatingPoint(i) => {
                self.consume(&Token::FloatingPoint(i))?;
                Ok(Expr::Float(i))
            }
            _ => {
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Plus,
                    right: Box::new(right),
                })
            }
        }
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || {
            format!("parse_factor: {:?}", self.current_token())
        });
        match self.current_token().clone() {
            Token::Minus => self.parse_minus(),
            Token::Plus => self.parse_plus(),
            Token::Asterisk => {
                self.consume(&Token::Asterisk)?;
                let right = self.parse_simple_expr()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Unpack,
                    right: Box::new(right),
                })
            }
            Token::DoubleAsterisk => {
                self.consume(&Token::DoubleAsterisk)?;
                let right = self.parse_simple_expr()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::DictUnpack,
                    right: Box::new(right),
                })
            }
            Token::Yield => {
                self.consume(&Token::Yield)?;

                if self.current_token() == &Token::From {
                    self.consume(&Token::From)?;
                    let expr = self.parse_simple_expr()?;
                    Ok(Expr::YieldFrom(Box::new(expr)))
                // The [`Token::RParen`] can be found on generator lambdas.
                } else if self.end_of_statement() || self.current_token() == &Token::RParen {
                    Ok(Expr::Yield(None))
                } else {
                    let expr = self.parse_simple_expr()?;
                    Ok(Expr::Yield(Some(Box::new(expr))))
                }
            }
            Token::Not => {
                self.consume(&Token::Not)?;
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::Not,
                    right: Box::new(right),
                })
            }
            Token::BitwiseNot => {
                self.consume(&Token::BitwiseNot)?;
                let right = self.parse_term()?;
                Ok(Expr::UnaryOperation {
                    op: UnaryOp::BitwiseNot,
                    right: Box::new(right),
                })
            }
            Token::None => {
                self.consume(&Token::None)?;
                Ok(Expr::None)
            }
            Token::NotImplemented => {
                self.consume(&Token::NotImplemented)?;
                Ok(Expr::NotImplemented)
            }
            Token::Ellipsis => {
                self.consume(&Token::Ellipsis)?;
                Ok(Expr::Ellipsis)
            }
            Token::Integer(i) => {
                self.consume(&Token::Integer(i))?;
                Ok(Expr::Integer(i as i64))
            }
            Token::FloatingPoint(i) => {
                self.consume(&Token::FloatingPoint(i))?;
                Ok(Expr::Float(i))
            }
            Token::BooleanLiteral(b) => {
                self.consume(&Token::BooleanLiteral(b))?;
                Ok(Expr::Boolean(b))
            }
            Token::Identifier(_) => {
                if self.tokens.peek(1) == &Token::LParen {
                    let name = self.parse_identifier()?;
                    let args = self.parse_function_call_args()?;

                    Ok(Expr::FunctionCall {
                        callee: Callee::Symbol(name),
                        args,
                    })
                } else if self.current_token().is_type() {
                    let type_node = self.parse_type_node()?;

                    match type_node {
                        TypeNode::Basic(type_) => Ok(Expr::Variable(type_)),
                        _ => Ok(Expr::TypeNode(type_node)),
                    }
                } else {
                    Ok(Expr::Variable(self.parse_identifier()?))
                }
            }
            Token::LParen => self.parse_tuple(),
            Token::LBracket => self.parse_list(),
            Token::LBrace => self.parse_set(),
            Token::Lambda => self.parse_lambda(),
            Token::StringLiteral(literal) => {
                self.consume(&Token::StringLiteral(literal.clone()))?;
                Ok(Expr::StringLiteral(literal))
            }
            Token::RawStringLiteral(literal) => {
                // TODO store the raw-ness here so that we do not escape characters
                self.consume(&Token::RawStringLiteral(literal.clone()))?;
                Ok(Expr::StringLiteral(literal))
            }
            Token::BytesLiteral(bytes) => {
                self.consume(&Token::BytesLiteral(bytes.clone()))?;
                Ok(Expr::BytesLiteral(bytes))
            }
            Token::BinaryLiteral(literal) => self.parse_binary_literal(literal),
            Token::OctalLiteral(literal) => self.parse_octal_literal(literal),
            Token::HexLiteral(literal) => self.parse_hex_literal(literal),
            Token::FStringStart => self.parse_f_string(),
            _ => Err(ParserError::UnexpectedToken(self.current_token().clone())),
        }
    }

    fn parse_binary_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::BinaryLiteral(literal.clone()))?;

        let result = i64::from_str_radix(&literal[2..], 2).map_err(|_| ParserError::SyntaxError)?;
        Ok(Expr::Integer(result))
    }

    fn parse_octal_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::OctalLiteral(literal.clone()))?;

        let result = i64::from_str_radix(&literal[2..], 8).map_err(|_| ParserError::SyntaxError)?;
        Ok(Expr::Integer(result))
    }

    fn parse_hex_literal(&mut self, literal: String) -> Result<Expr, ParserError> {
        self.consume(&Token::HexLiteral(literal.clone()))?;

        let result =
            i64::from_str_radix(&literal[2..], 16).map_err(|_| ParserError::SyntaxError)?;
        Ok(Expr::Integer(result))
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::Lambda)?;
        let args = self.parse_function_def_args(Token::Colon)?;
        self.consume(&Token::Colon)?;

        let expr = if self.current_token() == &Token::LParen {
            self.consume(&Token::LParen)?;
            let expr = self.parse_simple_expr()?;
            self.consume(&Token::RParen)?;
            expr
        } else {
            self.parse_simple_expr()?
        };

        Ok(Expr::Lambda {
            args,
            expr: Box::new(expr),
        })
    }

    fn parse_list(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_list".to_string());
        let mut items = Vec::new();

        self.consume(&Token::LBracket)?;
        while self.current_token() != &Token::RBracket {
            let expr = self.parse_simple_expr()?;
            items.push(expr.clone());

            if self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;

                // Handle trailing comma
                if self.current_token() == &Token::RBracket {
                    self.consume(&Token::RBracket)?;
                    return Ok(Expr::List(items));
                }
            } else if self.current_token() == &Token::RBracket {
                self.consume(&Token::RBracket)?;
                return Ok(Expr::List(items));
            }

            if self.current_token() == &Token::For {
                let clauses = self.parse_comprehension_clauses()?;
                self.consume(&Token::RBracket)?;

                return Ok(Expr::ListComprehension {
                    body: Box::new(expr),
                    clauses,
                });
            }
        }

        // You should only get here if this was an empty literal.
        assert_eq!(items.len(), 0);
        self.consume(&Token::RBracket)?;
        Ok(Expr::List(vec![]))
    }

    fn parse_f_string(&mut self) -> Result<Expr, ParserError> {
        self.consume(&Token::FStringStart)?;

        let mut parts = vec![];
        while self.current_token() != &Token::FStringEnd {
            match self.current_token().clone() {
                Token::StringLiteral(s) => {
                    self.consume(&Token::StringLiteral(s.to_string()))?;
                    parts.push(FStringPart::String(s.to_string()));
                }
                Token::LBrace => {
                    // Start consuming the expression within braces
                    self.consume(&Token::LBrace)?;
                    let expr = self.parse_simple_expr()?;

                    let format = if self.current_token() == &Token::Exclamation {
                        self.consume(&Token::Exclamation)?;
                        if let Token::Identifier(ident) = self.current_token().clone() {
                            self.consume(&Token::Identifier(ident.clone()))?;
                            match ident.as_str() {
                                "r" => FormatOption::Repr,
                                "s" => FormatOption::Str,
                                "a" => FormatOption::Ascii,
                                _ => {
                                    return Err(ParserError::UnexpectedToken(
                                        self.current_token().clone(),
                                    ));
                                }
                            }
                        } else {
                            return Err(ParserError::UnexpectedToken(self.current_token().clone()));
                        }
                    } else {
                        FormatOption::Str
                    };

                    self.consume(&Token::RBrace)?;
                    parts.push(FStringPart::Expr(ExprFormat {
                        expr: Box::new(expr),
                        format,
                    }));
                }
                _ => {
                    return Err(ParserError::UnexpectedToken(self.current_token().clone()));
                }
            }
        }

        self.consume(&Token::FStringEnd)?;
        Ok(Expr::FString(parts))
    }

    fn parse_set(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_set".to_string());
        let mut pairs = vec![];
        let mut set = HashSet::new();

        self.consume(&Token::LBrace)?;
        while self.current_token() != &Token::RBrace {
            let key = self.parse_simple_expr()?;

            match self.current_token() {
                // A Comma or an RBrace indicates the end of an element, which means this element
                // will not be a key-value (i.e. have a colon). However, only an RBrace indicates
                // an end to the entire literal.
                Token::Comma | Token::RBrace => {
                    match key {
                        Expr::UnaryOperation {
                            op: UnaryOp::DictUnpack,
                            right,
                        } => {
                            pairs.push(DictOperation::Unpack(*right));
                        }
                        _ => {
                            set.insert(key);
                        }
                    };
                    self.consume_optional(&Token::Comma);
                    if self.current_token() == &Token::RBrace {
                        break;
                    }
                }
                Token::For => {
                    let clauses = self.parse_comprehension_clauses()?;
                    self.consume(&Token::RBrace)?;
                    return Ok(Expr::SetComprehension {
                        body: Box::new(key),
                        clauses,
                    });
                }
                Token::Colon => {
                    self.consume(&Token::Colon)?;
                    let value = self.parse_simple_expr()?;

                    match self.current_token() {
                        // A Comma or an RBrace indicates the end of an element, which means this
                        // element _will_ be a key-value (i.e. have a colon). Only an RBrace
                        // indicates an end to the entire literal.
                        Token::Comma | Token::RBrace => {
                            pairs.push(DictOperation::Pair(key, value));
                            self.consume_optional(&Token::Comma);
                            if self.current_token() == &Token::RBrace {
                                break;
                            }
                        }
                        Token::For => {
                            let clauses = self.parse_comprehension_clauses()?;
                            self.consume(&Token::RBrace)?;
                            return Ok(Expr::DictComprehension {
                                clauses,
                                key_body: Box::new(key),
                                value_body: Box::new(value),
                            });
                        }
                        _ => {
                            return Err(ParserError::UnexpectedToken(self.current_token().clone()))
                        }
                    }
                }
                _ => return Err(ParserError::UnexpectedToken(self.current_token().clone())),
            }
        }

        self.consume(&Token::RBrace)?;
        if set.is_empty() {
            Ok(Expr::Dict(pairs))
        } else if pairs.is_empty() {
            Ok(Expr::Set(set))
        } else {
            Err(ParserError::SyntaxError)
        }
    }

    /// Single elements without a comma will be returned as is, everything else will be wrapped in
    /// `Expr::Tuple`.
    ///
    /// For example:
    ///
    /// (4) => int(4)
    /// (4,) => Expr::Tuple(vec!\[int(4)\])
    ///
    fn parse_tuple(&mut self) -> Result<Expr, ParserError> {
        log(LogLevel::Trace, || "parse_tuple".to_string());
        self.consume(&Token::LParen)?;

        let mut args = Vec::new();
        let mut is_single_element = true;
        while self.current_token() != &Token::RParen {
            let expr = self.parse_simple_expr()?;
            args.push(expr.clone());

            if self.current_token() == &Token::Comma {
                self.consume(&Token::Comma)?;
                is_single_element = false;
            }

            if self.current_token() == &Token::For {
                // If you saw a For token, we must be in list comprehension.
                assert_eq!(args.len(), 1);
                let gen_comp = self.parse_generator_comprehension(&expr)?;

                self.consume(&Token::RParen)?;
                return Ok(gen_comp);
            }
        }

        self.consume(&Token::RParen)?;

        if args.len() == 1 && is_single_element {
            Ok(args.into_iter().next().unwrap())
        } else {
            Ok(Expr::Tuple(args))
        }
    }

    fn parse_function_call_args(&mut self) -> Result<CallArgs, ParserError> {
        self.consume(&Token::LParen)?;

        let mut args = Vec::new();
        let mut kwargs = vec![];
        let mut args_var = None;
        while self.current_token() != &Token::RParen {
            if self.current_token() == &Token::Asterisk {
                self.consume(&Token::Asterisk)?;
                args_var = Some(Box::new(self.parse_simple_expr()?));

                // If *args is not at the end of the args (only kwargs can come after), we must
                // allow for a comma. This is similar to how we optionally consume a comma as the
                // last step of each loop iteration.
                self.consume_optional(&Token::Comma);
                continue;
            }

            // This is to support the formats
            // - foo(**{'a': 2, 'b': 1})
            // - foo(**args)
            if self.current_token() == &Token::DoubleAsterisk {
                self.consume(&Token::DoubleAsterisk)?;
                let kwargs_expr = self.parse_simple_expr()?;
                match kwargs_expr {
                    Expr::Dict(dict_ops) => {
                        for op in dict_ops {
                            match op {
                                DictOperation::Pair(key, value) => {
                                    let key_name =
                                        key.as_string().ok_or(ParserError::SyntaxError)?;
                                    let ident = Identifier::new(key_name).expect("Invalid key");
                                    kwargs.push(KwargsOperation::Pair(ident, value));
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                    Expr::Variable(_) | Expr::MemberAccess { .. } => {
                        kwargs.push(KwargsOperation::Unpacking(kwargs_expr));
                    }
                    _ => return Err(ParserError::SyntaxError),
                };
                self.consume_optional(&Token::Comma);
                continue;
            }

            match self.parse_function_call_arg()? {
                // This is to support the format foo(a=2, b=1)
                CallArg::Keyword { arg, expr } => {
                    kwargs.push(KwargsOperation::Pair(arg, expr));
                }
                CallArg::Positional(expr) => {
                    args.push(expr);
                }
            }

            self.consume_optional(&Token::Comma);
        }

        self.consume(&Token::RParen)?;

        Ok(CallArgs {
            args,
            kwargs,
            args_var,
        })
    }

    /// An argument in a function call can be either variable `a` or contain an equals such as
    /// `a = 4`. We originally (and ignorantly) called `parse_statement` but that contains too many
    /// other cases to be safely used inside function call parsing.
    fn parse_function_call_arg(&mut self) -> Result<CallArg, ParserError> {
        let expr = self.parse_simple_expr()?;
        match self.current_token() {
            Token::Assign => {
                self.consume(&Token::Assign)?;
                let arg = expr.as_variable().ok_or(ParserError::SyntaxError)?;
                Ok(CallArg::Keyword {
                    arg: arg.clone(),
                    expr: self.parse_simple_expr()?,
                })
            }
            Token::For => Ok(CallArg::Positional(
                self.parse_generator_comprehension(&expr)?,
            )),
            _ => Ok(CallArg::Positional(expr)),
        }
    }

    fn parse_generator_comprehension(&mut self, body: &Expr) -> Result<Expr, ParserError> {
        let clauses = self.parse_comprehension_clauses()?;
        Ok(Expr::GeneratorComprehension {
            body: Box::new(body.clone()),
            clauses,
        })
    }

    fn parse_comprehension_clauses(&mut self) -> Result<Vec<ForClause>, ParserError> {
        let mut clauses = vec![];
        while self.current_token() == &Token::For {
            clauses.push(self.parse_comprehension_clause()?);
        }
        Ok(clauses)
    }

    fn parse_comprehension_clause(&mut self) -> Result<ForClause, ParserError> {
        self.consume(&Token::For)?;

        // The parentheses are optional here, but if one is present, both must be present
        let mut need_rparen = false;
        if self.current_token() == &Token::LParen {
            self.consume(&Token::LParen)?;
            need_rparen = true;
        }

        let mut indices = vec![self.parse_identifier()?];
        while self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            indices.push(self.parse_identifier()?);
        }

        if need_rparen {
            self.consume(&Token::RParen)?;
        }

        self.consume(&Token::In)?;

        // We do not use `parse_expr` here because it can think that an expression of the
        // form `a if True` is the start of a ternary operation and expect an `else` token
        // next. By calling `parse_binary_expr`, we enter the parse tree below where
        // ternary operations are handled.
        let iterable = self.parse_binary_expr()?;

        let condition = if self.current_token() == &Token::If {
            self.consume(&Token::If)?;
            Some(self.parse_simple_expr()?)
        } else {
            None
        };

        Ok(ForClause {
            indices,
            iterable,
            condition,
        })
    }
}
