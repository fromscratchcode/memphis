use std::collections::HashSet;

use crate::{
    lexer::Token,
    parser::{
        Parser, ParserError, ParserResult,
        types::{AstParams, Param},
    },
};

#[derive(Debug, PartialEq)]
enum ParamPhase {
    BeforeSlash,
    AfterSlash,
    AfterStar,
}

impl Parser<'_> {
    pub fn parse_function_def_args(&mut self, end_token: Token) -> ParserResult<AstParams> {
        let mut params = AstParams::default();
        let mut phase = ParamPhase::BeforeSlash;

        while self.current_token() != &end_token {
            match self.current_token() {
                // This is to support positional-only parameters.
                // Context: PEP 570 (https://peps.python.org/pep-0570/)
                Token::Slash => match phase {
                    ParamPhase::AfterStar => {
                        return Err(ParserError::SyntaxError("/ must be ahead of *".to_string()));
                    }
                    ParamPhase::AfterSlash => {
                        return Err(ParserError::SyntaxError(
                            "/ may appear only once".to_string(),
                        ));
                    }
                    ParamPhase::BeforeSlash => {
                        assert!(
                            params.positional_only.is_empty(),
                            "Internal error: positional_only params set unexpectedly"
                        );
                        if params.positional_or_keyword.is_empty() {
                            return Err(ParserError::SyntaxError(
                                "/ must follow at least one named parameter".to_string(),
                            ));
                        }
                        self.consume(&Token::Slash)?;

                        phase = ParamPhase::AfterSlash;
                        params.positional_only = std::mem::take(&mut params.positional_or_keyword);
                    }
                },
                Token::Asterisk => match phase {
                    ParamPhase::AfterStar => {
                        return Err(ParserError::SyntaxError(
                            "* argument may appear only once".to_string(),
                        ));
                    }
                    ParamPhase::BeforeSlash | ParamPhase::AfterSlash => {
                        self.consume(&Token::Asterisk)?;
                        phase = ParamPhase::AfterStar;

                        // We will see an asterisk without a trailing identifier for keyword-only
                        // parameters.
                        // Context: PEP 3102 (https://peps.python.org/pep-3102/)
                        if matches!(self.current_token(), Token::Identifier(_)) {
                            params.args_var = Some(self.parse_identifier()?);
                        }
                    }
                },
                Token::DoubleAsterisk => {
                    self.consume(&Token::DoubleAsterisk)?;
                    params.kwargs_var = Some(self.parse_identifier()?);

                    // **kwargs is the end, only an optional trailing comma may follow
                    self.consume_optional(&Token::Comma);
                    if self.current_token() != &end_token {
                        return Err(ParserError::SyntaxError(
                            "arguments cannot follow var-keyword argument".to_string(),
                        ));
                    }
                    break;
                }
                _ => {
                    let arg = self.parse_identifier()?;
                    let default = if self.current_token() == &Token::Assign {
                        self.consume(&Token::Assign)?;
                        Some(self.parse_simple_expr()?)
                    } else {
                        None
                    };

                    let param = Param { arg, default };
                    match phase {
                        ParamPhase::BeforeSlash | ParamPhase::AfterSlash => {
                            params.positional_or_keyword.push(param)
                        }
                        ParamPhase::AfterStar => params.keyword_only.push(param),
                    }

                    // Support for type hints. Will there be reason to store these alongside the
                    // params themselves? Perhaps for future toolings like memphis-lsp.
                    //
                    // We have to guard with RParen here so the type hint case isn't triggered for
                    // lambdas.
                    if end_token == Token::RParen && self.current_token() == &Token::Colon {
                        self.consume(&Token::Colon)?;
                        let _type = self.parse_simple_expr()?;
                    }
                }
            }

            if self.current_token() != &end_token {
                self.consume(&Token::Comma)?;
            }
        }

        let mut names = HashSet::new();
        for name in params.names() {
            if !names.insert(name) {
                return Err(ParserError::SyntaxError(format!(
                    "duplicate argument '{name}' in function definition"
                )));
            }
        }

        let mut saw_default = false;
        for param in params.positional_params() {
            if param.default.is_some() {
                saw_default = true;
            } else if saw_default {
                return Err(ParserError::SyntaxError(
                    "parameter without a default follows parameter with a default".to_string(),
                ));
            }
        }

        if phase == ParamPhase::AfterStar
            && params.args_var.is_none()
            && params.keyword_only.is_empty()
        {
            return Err(ParserError::SyntaxError(
                "named arguments must follow bare *".to_string(),
            ));
        }

        Ok(params)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Token,
        parser::{ParserError, test_utils::*, types::AstParams},
    };

    #[test]
    fn default_args() {
        let input = r#"def foo(data=None): pass"#;
        let expected = params![param!("data", none!())];
        assert_signature_eq!(input, expected);
    }

    #[test]
    fn type_hints() {
        let input = "def add(x: str, y: str) -> str: pass";
        // For now, we just ensure the type hints are ignored.
        let expected = params![param!("x"), param!("y")];
        assert_signature_eq!(input, expected);
    }

    #[test]
    fn args_and_kwargs() {
        let input = r#"def test_args(*args): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: Some(ident!("args")),
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def test_args(*args, **kwargs): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: Some(ident!("args")),
            keyword_only: vec![],
            kwargs_var: Some(ident!("kwargs")),
        };
        assert_signature_eq!(input, expected);

        let input = r#"def test_kwargs(**kwargs): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: Some(ident!("kwargs")),
        };
        assert_signature_eq!(input, expected);

        let input = r#"def test_default(file=None): pass"#;
        let expected = params![param!("file", none!())];
        assert_signature_eq!(input, expected);
    }

    #[test]
    fn positional_only_and_keyword_only() {
        let input = r#"def f(a, /, b, *, c): pass"#;
        let expected = AstParams {
            positional_only: vec![param!("a")],
            positional_or_keyword: vec![param!("b")],
            args_var: None,
            keyword_only: vec![param!("c")],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);
    }

    #[test]
    fn signature_truth_table() {
        let input = r#"def f(): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![param!("a")],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, b): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![param!("a"), param!("b")],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, /): pass"#;
        let expected = AstParams {
            positional_only: vec![param!("a")],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, b, /): pass"#;
        let expected = AstParams {
            positional_only: vec![param!("a"), param!("b")],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, /, b): pass"#;
        let expected = AstParams {
            positional_only: vec![param!("a")],
            positional_or_keyword: vec![param!("b")],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, b, /, c, d=1): pass"#;
        let expected = AstParams {
            positional_only: vec![param!("a"), param!("b")],
            positional_or_keyword: vec![param!("c"), param!("d", int!(1))],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(*args): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: Some(ident!("args")),
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(**kwargs): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![],
            kwargs_var: Some(ident!("kwargs")),
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(*args, **kwargs): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: Some(ident!("args")),
            keyword_only: vec![],
            kwargs_var: Some(ident!("kwargs")),
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(*, required): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![param!("required")],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(*, optional=1): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![param!("optional", int!(1))],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, *, required, optional=1): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![param!("a")],
            args_var: None,
            keyword_only: vec![param!("required"), param!("optional", int!(1))],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, *args): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![param!("a")],
            args_var: Some(ident!("args")),
            keyword_only: vec![],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, *args, required, optional=1): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![param!("a")],
            args_var: Some(ident!("args")),
            keyword_only: vec![param!("required"), param!("optional", int!(1))],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);

        let input = r#"def f(a, /, b, *args, required, optional=1, **kwargs): pass"#;
        let expected = AstParams {
            positional_only: vec![param!("a")],
            positional_or_keyword: vec![param!("b")],
            args_var: Some(ident!("args")),
            keyword_only: vec![param!("required"), param!("optional", int!(1))],
            kwargs_var: Some(ident!("kwargs")),
        };
        assert_signature_eq!(input, expected);
    }

    #[test]
    fn lambda_signature() {
        let input = "lambda: None";
        let expected_ast = lambda!(params![], none!());
        assert_expr_eq!(input, expected_ast);

        let input = "lambda a, /, b, *, c: None";
        let expected_ast = lambda!(
            AstParams {
                positional_only: vec![param!("a")],
                positional_or_keyword: vec![param!("b")],
                args_var: None,
                keyword_only: vec![param!("c")],
                kwargs_var: None
            },
            none!()
        );
        assert_expr_eq!(input, expected_ast);
    }

    #[test]
    fn signature_errors() {
        let input = "def f(/): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("/ must follow at least one named parameter".to_string())
        );

        let input = "def f(a, /, b, /): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("/ may appear only once".to_string())
        );

        let input = "def f(a, *, *args): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("* argument may appear only once".to_string())
        );

        let input = "def f(a, *args, *more): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("* argument may appear only once".to_string())
        );

        let input = "def f(a, **kwargs, b): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("arguments cannot follow var-keyword argument".to_string())
        );

        let input = "def f(a, *, b, /): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("/ must be ahead of *".to_string())
        );

        let input = "def f(*): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("named arguments must follow bare *".to_string())
        );

        let input = "def f(*, **kwargs): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("named arguments must follow bare *".to_string())
        );
    }

    #[test]
    fn should_reject_nondefault_after_default() {
        let input = "def f(a=1, b): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError(
                "parameter without a default follows parameter with a default".to_string()
            )
        );

        let input = "def f(a=1, /, b): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError(
                "parameter without a default follows parameter with a default".to_string()
            )
        );

        // But this is allowed for keyword only, since they can be provided in any order
        let input = r#"def f(*, optional=1, required): pass"#;
        let expected = AstParams {
            positional_only: vec![],
            positional_or_keyword: vec![],
            args_var: None,
            keyword_only: vec![param!("optional", int!(1)), param!("required")],
            kwargs_var: None,
        };
        assert_signature_eq!(input, expected);
    }

    #[test]
    fn should_reject_duplicate_parameter_names() {
        let input = "def f(a, a): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("duplicate argument 'a' in function definition".to_string())
        );

        let input = "def f(a, /, a): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("duplicate argument 'a' in function definition".to_string())
        );

        let input = "def f(a, *a): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError("duplicate argument 'a' in function definition".to_string())
        );

        let input = "def f(*args, args): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError(
                "duplicate argument 'args' in function definition".to_string()
            )
        );

        let input = "def f(*, option, **option): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::SyntaxError(
                "duplicate argument 'option' in function definition".to_string()
            )
        );
    }

    #[test]
    fn should_reject_missing_comma() {
        let input = "def f(a, / b): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::ExpectedToken(Token::Comma, Token::Identifier(ident!("b")))
        );

        let input = "def f(a b): pass";
        let e = expect_error!(input);
        assert_eq!(
            e,
            ParserError::ExpectedToken(Token::Comma, Token::Identifier(ident!("b")))
        );
    }
}
