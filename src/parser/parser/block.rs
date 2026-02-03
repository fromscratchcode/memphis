use crate::{
    lexer::Token,
    parser::{
        types::{ast, Ast},
        Parser, ParserError,
    },
};

impl Parser<'_> {
    pub fn parse_block(&mut self) -> Result<Ast, ParserError> {
        if self.current_token() == &Token::Newline {
            self.consume_current();
            self.parse_indented_block()
        } else {
            self.parse_single_line_block()
        }
    }

    pub fn parse_statement_list_until<F, G>(
        &mut self,
        is_terminator: F,
        is_separator: G,
    ) -> Result<Ast, ParserError>
    where
        F: Fn(&Token) -> bool,
        G: Fn(&Token) -> bool,
    {
        let mut stmts = ast![];

        while !is_terminator(self.current_token()) {
            stmts.push(self.parse_statement()?);

            while is_separator(self.current_token()) {
                self.consume_current();
            }
        }

        Ok(stmts)
    }

    fn parse_indented_block(&mut self) -> Result<Ast, ParserError> {
        self.consume(&Token::Indent)?;
        self.consume_newlines();

        let stmts = self.parse_statement_list_until(
            |tok| matches!(tok, Token::Dedent),
            |tok| matches!(tok, Token::Newline | Token::Semicolon),
        )?;
        self.consume(&Token::Dedent)?;

        Ok(stmts)
    }

    /// Support single-line functions, classes, or any blocks.
    ///
    /// Examples:
    /// def _f() : pass
    /// def four(): return 4
    /// class Foo: pass
    /// def a(): pass; pass
    fn parse_single_line_block(&mut self) -> Result<Ast, ParserError> {
        let stmts = self.parse_statement_list_until(
            |tok| matches!(tok, Token::Newline),
            |tok| matches!(tok, Token::Semicolon),
        )?;
        self.consume(&Token::Newline)?;
        Ok(stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{
        test_utils::*,
        types::{ast, ConditionalAst, StatementKind},
    };

    #[test]
    fn invalid_identifier() {
        let input = "a.123";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("invalid identifier".to_string())
        );
    }

    #[test]
    fn semicolon() {
        let input = "a = 10; 4 + a";
        let expected_ast = ast![
            stmt_assign!(var!("a"), int!(10)),
            stmt_expr!(bin_op!(int!(4), Add, var!("a")))
        ];
        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn single_line_blocks() {
        let input = "
if True: a = 4
else: a = 6
";
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bool!(true),
                ast: ast![stmt_assign!(var!("a"), int!(4))],
            },
            elif_parts: vec![],
            else_part: Some(ast![stmt_assign!(var!("a"), int!(6))]),
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn single_line_blocks_with_semicolons() {
        let input = "
if True: a = 4; b = 8
else: a = 6; b = 7
";
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bool!(true),
                ast: ast![
                    stmt_assign!(var!("a"), int!(4)),
                    stmt_assign!(var!("b"), int!(8))
                ],
            },
            elif_parts: vec![],
            else_part: Some(ast![
                stmt_assign!(var!("a"), int!(6)),
                stmt_assign!(var!("b"), int!(7))
            ]),
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn semicolons_in_indented_block() {
        let input = "
if True:
    a = 4; b = 8
";
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: bool!(true),
                ast: ast![
                    stmt_assign!(var!("a"), int!(4)),
                    stmt_assign!(var!("b"), int!(8))
                ],
            },
            elif_parts: vec![],
            else_part: None,
        });
        assert_stmt_eq!(input, expected_ast);
    }

    #[test]
    fn single_line_block_split_lines() {
        let input = "
if True: a = 3
    b = 8
";
        let e = expect_error!(input);
        assert_eq!(e, ParserError::UnexpectedToken(Token::Indent));
    }
}
