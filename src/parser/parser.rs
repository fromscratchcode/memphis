use crate::{
    core::{log, LogLevel},
    domain::Identifier,
    lexer::{Lexer, Token},
    parser::{
        types::{ast, Ast, Expr, TypeNode},
        ParserError, TokenBuffer,
    },
};

mod expr;
mod import;
mod signature;
mod stmt;

/// A recursive-descent parser which attempts to encode the full Python grammar.
pub struct Parser<'a> {
    tokens: TokenBuffer<'a>,
    line_number: usize,
    delimiter_depth: usize,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Parser {
            tokens: TokenBuffer::new(lexer),
            line_number: 1,
            delimiter_depth: 0,
        }
    }

    /// Return the full AST. This will consume all the tokens.
    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut stmts = vec![];
        while !self.is_finished() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        Ok(Ast::new(stmts))
    }

    pub fn is_finished(&mut self) -> bool {
        self.current_token() == &Token::Eof
    }

    fn current_token(&mut self) -> &Token {
        self.tokens.peek(0)
    }

    fn end_of_statement(&mut self) -> bool {
        self.is_finished() || self.current_token() == &Token::Newline
    }

    fn inside_delimiter(&self) -> bool {
        self.delimiter_depth > 0
    }

    /// If we are inside a string literal, we must check for newline characters rather than
    /// tokens. These are produced by `Lexer::emit_newline`.
    fn advance_line_number_if_needed(&mut self) {
        if self.current_token() == &Token::Newline {
            self.line_number += 1;
        } else if let Token::StringLiteral(string) = &self.current_token() {
            self.line_number += string.matches('\n').count();
        } else if let Token::RawStringLiteral(string) = &self.current_token() {
            self.line_number += string.matches('\n').count();
        }
    }

    fn consume_current(&mut self) -> Result<(), ParserError> {
        let token = self.tokens.peek(0).clone();
        self.consume(&token)
    }

    fn consume(&mut self, expected: &Token) -> Result<(), ParserError> {
        let current = self.tokens.peek(0);

        log(LogLevel::Trace, || format!("Token: {current:?}"));

        if current != expected {
            return Err(ParserError::ExpectedToken(
                expected.clone(),
                current.clone(),
            ));
        }

        if matches!(current, Token::LParen | Token::LBracket | Token::LBrace) {
            self.delimiter_depth += 1;
        } else if matches!(current, Token::RParen | Token::RBracket | Token::RBrace) {
            self.delimiter_depth -= 1;
        }

        self.advance_line_number_if_needed();

        self.tokens.consume();

        if self.inside_delimiter() {
            self.consume_optional_many(&Token::Newline);
        }

        Ok(())
    }

    fn consume_optional(&mut self, expected: &Token) {
        if self.current_token() == expected {
            let _ = self.consume(expected);
        }
    }

    fn consume_optional_many(&mut self, expected: &Token) {
        while self.current_token() == expected {
            let _ = self.consume(expected);
        }
    }

    fn parse_indented_block(&mut self) -> Result<Ast, ParserError> {
        self.consume_optional_many(&Token::Newline);
        self.consume(&Token::Indent)?;

        let mut statements = Vec::new();
        while self.current_token() != &Token::Dedent {
            statements.push(self.parse_statement()?);
        }
        self.consume(&Token::Dedent)?;
        self.consume_optional_many(&Token::Newline);

        Ok(Ast::new(statements))
    }

    fn parse_type_node(&mut self) -> Result<TypeNode, ParserError> {
        let mut nodes = vec![];

        loop {
            let node = match self.current_token() {
                Token::Identifier(ref ident) => {
                    let i = ident.clone();
                    match ident.as_str() {
                        "int" | "str" | "dict" => {
                            self.consume(&Token::Identifier(i.clone()))?;
                            TypeNode::Basic(i.clone())
                        }
                        "list" => {
                            self.consume(&Token::Identifier(i.clone()))?;

                            if self.current_token() == &Token::LBracket {
                                self.consume(&Token::LBracket)?;
                                let parameters = self.parse_type_node()?;
                                self.consume(&Token::RBracket)?;

                                TypeNode::Generic {
                                    base_type: i.clone(),
                                    parameters: vec![parameters],
                                }
                            } else {
                                TypeNode::Basic(i.clone())
                            }
                        }
                        _ => unimplemented!(),
                    }
                }
                Token::Ellipsis => {
                    self.consume(&Token::Ellipsis)?;
                    // this is from _collections_abc.py: EllipsisType = type(...)
                    TypeNode::Ellipsis
                }
                _ => unimplemented!(),
            };

            nodes.push(node);

            if self.current_token() != &Token::BitwiseOr {
                break;
            }
            self.consume(&Token::BitwiseOr)?;
        }

        if nodes.len() == 1 {
            Ok(nodes[0].clone())
        } else {
            Ok(TypeNode::Union(nodes))
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

    fn parse_block(&mut self) -> Result<Ast, ParserError> {
        if self.current_token() == &Token::Newline {
            self.parse_indented_block()
        } else {
            // Support single-line functions or classes
            // Examples:
            // def _f() : pass
            // def four(): return 4
            // class Foo: pass
            Ok(ast![self.parse_statement()?])
        }
    }

    fn parse_identifiers(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut items = vec![self.parse_identifier()?];
        while self.current_token() == &Token::Comma {
            self.consume(&Token::Comma)?;
            items.push(self.parse_identifier()?);
        }
        Ok(items)
    }

    /// Parse a `Token::Identifier` without any semantic analysis.
    fn parse_identifier(&mut self) -> Result<Identifier, ParserError> {
        match self.current_token().clone() {
            Token::Identifier(ident) => {
                self.consume(&Token::Identifier(ident.clone()))?;
                Ok(ident)
            }
            _ => Err(ParserError::SyntaxError),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{
        test_utils::*,
        types::{
            CallArgs, CompoundOperator, ConditionalAst, ExceptHandler, ExprFormat, FStringPart,
            ForClause, FormatOption, KwargsOperation, LoopIndex, Params, RaiseKind, Statement,
            StatementKind,
        },
    };

    fn ident(input: &str) -> Identifier {
        Identifier::new(input).expect("Invalid identifier")
    }

    #[test]
    fn expression() {
        let input = "2 + 3 * (4 - 1)";
        let expected_ast = bin_op!(
            int!(2),
            Add,
            bin_op!(int!(3), Mul, bin_op!(int!(4), Sub, int!(1)))
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 // 3";
        let expected_ast = bin_op!(int!(2), IntegerDiv, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn string_literal() {
        let input = "\"Hello\"";
        let expected_ast = str!("Hello");

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "\"\".join([])";
        let expected_ast = method_call!(str!(""), "join", call_args![list![]]);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn variable_assignment() {
        let input = "a = 2";
        let expected_ast = stmt_assign!(var!("a"), int!(2));

        assert_ast_eq!(input, expected_ast);

        let input = "b = a + 3";
        let expected_ast = stmt_assign!(var!("b"), bin_op!(var!("a"), Add, int!(3)));

        assert_ast_eq!(input, expected_ast);

        let input = "a, b = (1, 2)";
        let expected_ast = stmt!(StatementKind::UnpackingAssignment {
            left: vec![var!("a"), var!("b")],
            right: tuple![int!(1), int!(2)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn function_call() {
        let input = "print(\"Hello, World!\")";
        let expected_ast = func_call!("print", call_args![str!("Hello, World!")]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a(*self.args, **self.kwargs)";
        let expected_ast = func_call!(
            "a",
            CallArgs {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(member_access!(
                    var!("self"),
                    "kwargs"
                ))],
                args_var: Some(Box::new(member_access!(var!("self"), "args"))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);
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

        assert_ast_eq!(input, expected_ast);

        let input = "def _f(): pass";
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("_f"),
            args: params![],
            body: ast![stmt!(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "lambda: 4";
        let expected_ast = lambda!(params![], int!(4));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "lambda index: 4";
        let expected_ast = lambda!(params![param!("index")], int!(4));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "lambda index, val: 4";
        let expected_ast = lambda!(params![param!("index"), param!("val")], int!(4));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "lambda: (yield)";
        let expected_ast = lambda!(params![], yield_expr!());

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "(lambda: (yield))()";
        let expected_ast = func_call_callee!(lambda!(params![], yield_expr!()), call_args![]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
def __init__(
    self, *, indent=None,
):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("__init__"),
            args: params![param!("self"), param!("indent", none!())],
            body: ast![stmt!(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "return a, b";
        let expected_ast = stmt_return![var!("a"), var!("b")];

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn boolean_expressions() {
        let input = "x and y";
        let expected_ast = logic_op!(var!("x"), And, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x or y";
        let expected_ast = logic_op!(var!("x"), Or, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x or not y";
        let expected_ast = logic_op!(var!("x"), Or, unary_op!(Not, var!("y")));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "not (x or y)";
        let expected_ast = unary_op!(Not, logic_op!(var!("x"), Or, var!("y")));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
if (a
    or b):
    pass
"#;
        let expected_ast = stmt!(StatementKind::IfElse {
            if_part: ConditionalAst {
                condition: logic_op!(var!("a"), Or, var!("b")),
                ast: ast![stmt!(StatementKind::Pass)],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn comparison_operators() {
        let input = "x == y";
        let expected_ast = cmp_op!(var!("x"), Equals, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x != y";
        let expected_ast = cmp_op!(var!("x"), NotEquals, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x < y";
        let expected_ast = cmp_op!(var!("x"), LessThan, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x > y";
        let expected_ast = cmp_op!(var!("x"), GreaterThan, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x >= y";
        let expected_ast = cmp_op!(var!("x"), GreaterThanOrEqual, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x <= y";
        let expected_ast = cmp_op!(var!("x"), LessThanOrEqual, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x in y";
        let expected_ast = cmp_op!(var!("x"), In, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x not in y";
        let expected_ast = cmp_op!(var!("x"), NotIn, var!("y"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x is None";
        let expected_ast = cmp_op!(var!("x"), Is, none!());

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x is not None";
        let expected_ast = cmp_op!(var!("x"), IsNot, none!());

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn boolean_operators() {
        let input = "x = True";
        let expected_ast = stmt_assign!(var!("x"), bool!(true));

        assert_ast_eq!(input, expected_ast);

        let input = "True or False";
        let expected_ast = logic_op!(bool!(true), Or, bool!(false));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x = None";
        let expected_ast = stmt_assign!(var!("x"), none!());

        assert_ast_eq!(input, expected_ast);

        let input = "return None";
        let expected_ast = stmt_return![none!()];

        assert_ast_eq!(input, expected_ast);
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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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
                ast: ast![stmt!(StatementKind::Pass)],
            },
            elif_parts: vec![],
            else_part: None,
        });

        assert_ast_eq!(input, expected_ast);
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

        assert_ast_eq!(input, expected_ast);
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

        assert_ast_eq!(input, expected_ast);

        let input = "class Foo(Bar, Baz): pass";
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![var!("Bar"), var!("Baz")],
            metaclass: None,
            body: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = "class Foo(module.Bar): pass";
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![member_access!(var!("module"), "Bar")],
            metaclass: None,
            body: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn class_instantiation() {
        let input = "foo = Foo()";
        let expected_ast = stmt_assign!(var!("foo"), func_call!("Foo", call_args![]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn method_invocation() {
        let input = "foo.bar()";
        let expected_ast = method_call!(var!("foo"), "bar");

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"Response.text().to_bytes()"#;
        let expected_ast = method_call!(method_call!(var!("Response"), "text"), "to_bytes");

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn regular_import() {
        let input = "import other";
        let expected_ast = stmt_reg_import![import!("other")];

        assert_ast_eq!(input, expected_ast);

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

        // We test this inside a function so that it will attempt to parse more than one statement,
        // which is what originally caught the bug related to parsing the comma and beyond.
        assert_ast_eq!(input, expected_ast);

        let input = r#"
import other as b
pass
"#;
        let expected_ast = stmt_reg_import![import!("other", "b")];

        // Before we handling Token::As processing, this test would fail, but only once it began
        // parsing the next statement. We needed to parse two statements here to produce the
        // failing test.
        let asts = parse_all(input);
        assert_eq!(asts.len(), 2);
        assert_stmt_eq!(asts.get(0).unwrap(), expected_ast);
        assert_stmt_eq!(asts.get(1).unwrap(), stmt!(StatementKind::Pass));

        let input = "mypackage.myothermodule.add('1', '1')";
        let expected_ast = func_call_callee!(
            member_access!(member_access!(var!("mypackage"), "myothermodule"), "add"),
            call_args![str!("1"), str!("1")]
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "cls._abc_registry.add(subclass)";
        let expected_ast = func_call_callee!(
            member_access!(member_access!(var!("cls"), "_abc_registry"), "add"),
            call_args![var!("subclass")]
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn selective_import() {
        let input = "from other import a";
        let expected_ast = stmt_from_import!("other", from_import_list![from_import_item!("a")]);

        assert_ast_eq!(input, expected_ast);

        let input = "from other import a, b";
        let expected_ast = stmt_from_import!(
            "other",
            from_import_list![from_import_item!("a"), from_import_item!("b")]
        );

        assert_ast_eq!(input, expected_ast);

        let input = "from other import *";
        let expected_ast = stmt_from_import!("other", from_import_all!());

        assert_ast_eq!(input, expected_ast);

        let input = "from other import a, b as c";
        let expected_ast = stmt_from_import!(
            "other",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );

        assert_ast_eq!(input, expected_ast);

        let input = "from other.module import a, b as c";
        let expected_ast = stmt_from_import!(
            "other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );

        assert_ast_eq!(input, expected_ast);

        let input = "from . import a";
        let expected_ast = stmt_from_import!(".", from_import_list![from_import_item!("a")]);

        assert_ast_eq!(input, expected_ast);

        let input = "from .other.module import a, b as c";
        let expected_ast = stmt_from_import!(
            ".other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );

        assert_ast_eq!(input, expected_ast);

        let input = "from ..other.module import a, b as c";
        let expected_ast = stmt_from_import!(
            "..other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );

        assert_ast_eq!(input, expected_ast);

        let input = r#"
from ..other.module import (a,
                            b as c)
"#;
        let expected_ast = stmt_from_import!(
            "..other.module",
            from_import_list![from_import_item!("a"), from_import_item!("b", "c")]
        );

        assert_ast_eq!(input, expected_ast);

        let input = r#"
from ..other.module import (a as b,
                            c)
"#;
        let expected_ast = stmt_from_import!(
            "..other.module",
            from_import_list![from_import_item!("a", "b"), from_import_item!("c")]
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn floating_point() {
        let input = "a = 3.14";
        let expected_ast = stmt_assign!(var!("a"), float!(3.14));

        assert_ast_eq!(input, expected_ast);

        let input = "b = a + 2.5e-3";
        let expected_ast = stmt_assign!(var!("b"), bin_op!(var!("a"), Add, float!(2.5e-3)));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn negative_numbers() {
        let input = "-3.14";
        let expected_ast = float!(-3.14);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-3";
        let expected_ast = int!(-3);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 - 3";
        let expected_ast = bin_op!(int!(2), Sub, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-2e-3";
        let expected_ast = float!(-2e-3);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 + -3";
        let expected_ast = bin_op!(int!(2), Add, int!(-3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-(3)";
        let expected_ast = unary_op!(Minus, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "+(3)";
        let expected_ast = unary_op!(Plus, int!(3));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "-(2 + 3)";
        let expected_ast = unary_op!(Minus, bin_op!(int!(2), Add, int!(3)));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn lists() {
        let input = "[1,2,3]";
        let expected_ast = list![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "[1, 2, 3]";
        let expected_ast = list![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
a = [1,
    2,
    3
]"#;
        let expected_ast = stmt_assign!(var!("a"), list![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "a = [1, 2, 3]";
        let expected_ast = stmt_assign!(var!("a"), list![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "list([1, 2, 3])";
        let expected_ast = func_call!("list", call_args![list![int!(1), int!(2), int!(3)]]);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn sets() {
        let input = "{1,2,3}";
        let expected_ast = set![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "{1, 2, 3}";
        let expected_ast = set![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a = {1, 2, 3}";
        let expected_ast = stmt_assign!(var!("a"), set![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "set({1, 2, 3})";
        let expected_ast = func_call!("set", call_args![set![int!(1), int!(2), int!(3)]]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
{
    1,
    2,
    3
}"#;
        let expected_ast = stmt_expr!(set![int!(1), int!(2), int!(3),]);

        assert_ast_eq!(input, expected_ast);

        let input = r#"
{
    1,
    2,
    3,
}"#;
        let expected_ast = stmt_expr!(set![int!(1), int!(2), int!(3),]);

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn tuples() {
        let input = "(1,2,3)";
        let expected_ast = tuple![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "(1, 2, 3)";
        let expected_ast = tuple![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "1, 2, 3";
        let expected_ast = tuple![int!(1), int!(2), int!(3)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "1,";
        let expected_ast = tuple![int!(1)];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a = (1, 2, 3)";
        let expected_ast = stmt_assign!(var!("a"), tuple![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "a = 1, 2, 3";
        let expected_ast = stmt_assign!(var!("a"), tuple![int!(1), int!(2), int!(3)]);

        assert_ast_eq!(input, expected_ast);

        let input = "tuple((1, 2, 3))";
        let expected_ast = func_call!("tuple", call_args![tuple![int!(1), int!(2), int!(3)]]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
tuple((1,
       2,
       3))
"#;
        let expected_ast = stmt_expr!(func_call!(
            "tuple",
            call_args![tuple![int!(1), int!(2), int!(3)]]
        ));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn index_access() {
        let input = "a[0]";
        let expected_ast = Expr::IndexAccess {
            object: Box::new(var!("a")),
            index: Box::new(int!(0)),
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "[0,1][1]";
        let expected_ast = Expr::IndexAccess {
            object: Box::new(list![int!(0), int!(1)]),
            index: Box::new(int!(1)),
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[1] = 0";
        let expected_ast = stmt_assign!(
            Expr::IndexAccess {
                object: Box::new(var!("a")),
                index: Box::new(int!(1)),
            },
            int!(0)
        );

        assert_ast_eq!(input, expected_ast);
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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn list_comprehension() {
        let input = "[ i * 2 for i in a ]";
        let expected_ast = Expr::ListComprehension {
            body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
            clauses: vec![ForClause {
                indices: vec![ident("i")],
                iterable: var!("a"),
                condition: None,
            }],
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "[i*2 for i in a if True]";
        let expected_ast = Expr::ListComprehension {
            body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
            clauses: vec![ForClause {
                indices: vec![ident("i")],
                iterable: var!("a"),
                condition: Some(bool!(true)),
            }],
        };

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn generators() {
        let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n = n - 1
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("countdown"),
            args: params![param!("n")],
            body: ast![stmt!(StatementKind::WhileLoop(ConditionalAst {
                condition: cmp_op!(var!("n"), GreaterThan, int!(0)),
                ast: ast![
                    stmt_expr!(yield_expr!(var!("n"))),
                    stmt_assign!(var!("n"), bin_op!(var!("n"), Sub, int!(1))),
                ],
            }))],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "yield from a";
        let expected_ast = yield_from!(var!("a"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "x = yield from a";
        let expected_ast = stmt_assign!(var!("x"), yield_from!(var!("a")));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn inheritance() {
        let input = r#"
class Foo(Parent):
    def __init__(self):
        self.x = 0
"#;
        let ast = parse!(input, Statement);
        let expected_parent = vec![var!("Parent")];

        let StatementKind::ClassDef { parents, .. } = ast.kind else {
            panic!("Expected a class def!")
        };
        assert_eq!(parents, expected_parent);

        let input = r#"
class Foo(metaclass=Parent):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![],
            metaclass: Some(ident("Parent")),
            body: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
class Foo(Bar, metaclass=Parent):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("Foo"),
            parents: vec![var!("Bar")],
            metaclass: Some(ident("Parent")),
            body: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
class InterfaceMeta(type):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ClassDef {
            name: ident("InterfaceMeta"),
            parents: vec![var!("type")],
            metaclass: None,
            body: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn dictionaries() {
        let input = r#"{ "b": 4, 'c': 5 }"#;
        let expected_ast = dict![
            dict_pair!(str!("b"), int!(4)),
            dict_pair!(str!("c"), int!(5)),
        ];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
namespace = {
    '__name__': 4,
}
"#;
        let expected_ast = stmt_assign!(
            var!("namespace"),
            dict![dict_pair!(str!("__name__"), int!(4))]
        );

        assert_ast_eq!(input, expected_ast);

        let input = r#"{ **first, **second }"#;
        let expected_ast = dict![dict_unpack!(var!("first")), dict_unpack!(var!("second")),];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"{ **first, **second, }"#;
        let expected_ast = dict![dict_unpack!(var!("first")), dict_unpack!(var!("second")),];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "{ 2, **second }";
        let e = expect_error!(input, Expr);
        assert_eq!(e, ParserError::SyntaxError);

        let input = "{ 2, **second, }";
        let e = expect_error!(input, Expr);
        assert_eq!(e, ParserError::SyntaxError);
    }

    #[test]
    fn dict_comprehension() {
        let input = r#"{ key: val * 2 for key, val in d }"#;
        let expected_ast = Expr::DictComprehension {
            clauses: vec![ForClause {
                indices: vec![ident("key"), ident("val")],
                iterable: var!("d"),
                condition: None,
            }],
            key_body: Box::new(var!("key")),
            value_body: Box::new(bin_op!(var!("val"), Mul, int!(2))),
        };

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"{ key: val * 2 for (key, val) in d }"#;
        let expected_ast = Expr::DictComprehension {
            clauses: vec![ForClause {
                indices: vec![ident("key"), ident("val")],
                iterable: var!("d"),
                condition: None,
            }],
            key_body: Box::new(var!("key")),
            value_body: Box::new(bin_op!(var!("val"), Mul, int!(2))),
        };

        assert_ast_eq!(input, expected_ast, Expr);
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

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn assert() {
        let input = r#"
assert True
"#;
        let expected_ast = stmt!(StatementKind::Assert(bool!(true)));

        assert_ast_eq!(input, expected_ast);
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
            handlers: vec![ExceptHandler::bare(ast![stmt_assign!(var!("a"), int!(2))])],
            else_block: None,
            finally_block: Some(ast![stmt_assign!(var!("a"), int!(3))]),
        });

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

        let input = r#"
try:
    pass
except:
    return
a = 1
"#;
        let expected_ast = stmt!(StatementKind::TryExcept {
            try_block: ast![stmt!(StatementKind::Pass)],
            handlers: vec![ExceptHandler::bare(ast![stmt_return![]])],
            else_block: None,
            finally_block: None,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn binary_literal() {
        let input = "a = 0b0010";
        let expected_ast = stmt_assign!(var!("a"), int!(2));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn octal_literal() {
        let input = "a = 0o0010";
        let expected_ast = stmt_assign!(var!("a"), int!(8));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn hex_literal() {
        let input = "a = 0x0010";
        let expected_ast = stmt_assign!(var!("a"), int!(16));

        assert_ast_eq!(input, expected_ast);
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
            body: ast![stmt!(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

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
            body: ast![stmt!(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
def test_kwargs(**kwargs):
    print(kwargs['a'])
"#;
        let ast = parse!(input, Statement);
        let expected_args = Params {
            args: vec![],
            args_var: None,
            kwargs_var: Some(ident("kwargs")),
        };
        let StatementKind::FunctionDef { args, .. } = ast.kind else {
            panic!("Expected function def")
        };
        assert_eq!(expected_args, args);

        let input = r#"
def test_default(file=None):
    pass
"#;
        let expected_ast = stmt!(StatementKind::FunctionDef {
            name: ident("test_default"),
            args: params![param!("file", none!())],
            body: ast![stmt!(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);

        let input = "test_kwargs(a=1, b=2)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair(ident("a"), int!(1)),
                    KwargsOperation::Pair(ident("b"), int!(2)),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**{'a':1, 'b':2})";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair(ident("a"), int!(1)),
                    KwargsOperation::Pair(ident("b"), int!(2)),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**{'a':1, 'b':2}, **{'c': 3})";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Pair(ident("a"), int!(1)),
                    KwargsOperation::Pair(ident("b"), int!(2)),
                    KwargsOperation::Pair(ident("c"), int!(3)),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**first, **second)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![
                    KwargsOperation::Unpacking(var!("first")),
                    KwargsOperation::Unpacking(var!("second")),
                ],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(**kwargs)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(var!("kwargs"))],
                args_var: None,
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(*args)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![],
                args_var: Some(Box::new(var!("args"))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "test_kwargs(*args, **kwargs)";
        let expected_ast = func_call!(
            "test_kwargs",
            CallArgs {
                args: vec![],
                kwargs: vec![KwargsOperation::Unpacking(var!("kwargs"))],
                args_var: Some(Box::new(var!("args"))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"
deprecated("collections.abc.ByteString",
)
"#;
        // TODO we have to use StatementKind::Expression here because it is on multiple lines, I
        // don't think this should technically be required
        let expected_ast = stmt_expr!(func_call!(
            "deprecated",
            call_args![str!("collections.abc.ByteString")]
        ));

        assert_ast_eq!(input, expected_ast);
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

        assert_ast_eq!(input, expected_ast);

        let input = "test_decorator(get_val_undecorated)()";
        let expected_ast = func_call_callee!(
            func_call!("test_decorator", call_args![var!("get_val_undecorated")]),
            call_args![]
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn raise() {
        let input = "raise";
        let expected_ast = stmt!(StatementKind::Raise(RaiseKind::Reraise));

        assert_ast_eq!(input, expected_ast);

        let input = "raise Exception";
        let expected_ast = stmt!(StatementKind::Raise(RaiseKind::Raise(var!("Exception"))));

        assert_ast_eq!(input, expected_ast);

        let input = r#"raise Exception("message")"#;
        let expected_ast = stmt!(StatementKind::Raise(RaiseKind::Raise(func_call!(
            "Exception",
            call_args![str!("message")]
        ))));

        assert_ast_eq!(input, expected_ast);

        let input = r#"raise Exception("message") from None"#;
        let expected_ast = stmt!(StatementKind::Raise(RaiseKind::RaiseFrom {
            exception: func_call!("Exception", call_args![str!("message")]),
            cause: none!()
        }));

        assert_ast_eq!(input, expected_ast);
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
            block: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);

        let input = r#"
with open('test.txt'):
    pass
"#;
        let expected_ast = stmt!(StatementKind::ContextManager {
            expr: func_call!("open", call_args![str!("test.txt")]),
            variable: None,
            block: ast![stmt!(StatementKind::Pass)],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn type_alias() {
        let input = "a = list[int]";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::TypeNode(TypeNode::Generic {
                base_type: ident("list"),
                parameters: vec![TypeNode::Basic(ident("int"))],
            })
        );

        assert_ast_eq!(input, expected_ast);

        let input = "u = int | str";
        let expected_ast = stmt_assign!(
            var!("u"),
            Expr::TypeNode(TypeNode::Union(vec![
                TypeNode::Basic(ident("int")),
                TypeNode::Basic(ident("str")),
            ]))
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn delete() {
        let input = "del a";
        let expected_ast = stmt!(StatementKind::Delete(vec![var!("a")]));

        assert_ast_eq!(input, expected_ast);

        let input = "del a, b, c";
        let expected_ast = stmt!(StatementKind::Delete(vec![var!("a"), var!("b"), var!("c")]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn byte_string() {
        let input = "a = b'hello'";
        let expected_ast = stmt_assign!(var!("a"), Expr::BytesLiteral("hello".into()));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn compound_operator() {
        let input = "a += 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Add,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a -= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Subtract,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a *= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Multiply,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a /= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Divide,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });
        assert_ast_eq!(input, expected_ast);

        let input = "a &= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseAnd,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a |= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseOr,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a ^= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::BitwiseXor,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a //= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::IntegerDiv,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a <<= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::LeftShift,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a >>= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::RightShift,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a %= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Mod,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a @= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::MatMul,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "a **= 1";
        let expected_ast = stmt!(StatementKind::CompoundAssignment {
            operator: CompoundOperator::Expo,
            target: Box::new(var!("a")),
            value: Box::new(int!(1)),
        });

        assert_ast_eq!(input, expected_ast);

        let input = "~a";
        let expected_ast = unary_op!(BitwiseNot, var!("a"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a % b";
        let expected_ast = bin_op!(var!("a"), Mod, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a @ b";
        let expected_ast = bin_op!(var!("a"), MatMul, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn f_strings() {
        let input = r#"f"Hello {name}.""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("name")),
                format: FormatOption::Str,
            }),
            FStringPart::String(".".into()),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"{first}{last}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("first")),
                format: FormatOption::Str,
            }),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("last")),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Hello""#;
        let expected_ast = Expr::FString(vec![FStringPart::String("Hello".into())]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Hello {name} goodbye {other}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("name")),
                format: FormatOption::Str,
            }),
            FStringPart::String(" goodbye ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("other")),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Age: {num + 1}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Age: ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(bin_op!(var!("num"), Add, int!(1))),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"environ({{{formatted_items}}})""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("environ({".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("formatted_items")),
                format: FormatOption::Str,
            }),
            FStringPart::String("})".into()),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = r#"f"Hello {name!r} goodbye {other}""#;
        let expected_ast = Expr::FString(vec![
            FStringPart::String("Hello ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("name")),
                format: FormatOption::Repr,
            }),
            FStringPart::String(" goodbye ".into()),
            FStringPart::Expr(ExprFormat {
                expr: Box::new(var!("other")),
                format: FormatOption::Str,
            }),
        ]);

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn binary_operators() {
        let input = "a & b";
        let expected_ast = bin_op!(var!("a"), BitwiseAnd, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a | b";
        let expected_ast = bin_op!(var!("a"), BitwiseOr, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a ^ b";
        let expected_ast = bin_op!(var!("a"), BitwiseXor, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a << b";
        let expected_ast = bin_op!(var!("a"), LeftShift, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a >> b";
        let expected_ast = bin_op!(var!("a"), RightShift, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a ** b";
        let expected_ast = bin_op!(var!("a"), Expo, var!("b"));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "2 * 3 << 2 + 4 & 205";
        let expected_ast = bin_op!(
            bin_op!(
                bin_op!(int!(2), Mul, int!(3)),
                LeftShift,
                bin_op!(int!(2), Add, int!(4))
            ),
            BitwiseAnd,
            int!(205)
        );

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn operator_chaining() {
        let input = "a == b == c";
        let expected_ast = cmp_chain!(var!("a"), [(Equals, var!("b")), (Equals, var!("c")),]);

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a == b < c > d";
        let expected_ast = cmp_chain!(
            var!("a"),
            [
                (Equals, var!("b")),
                (LessThan, var!("c")),
                (GreaterThan, var!("d")),
            ]
        );

        assert_ast_eq!(input, expected_ast, Expr);
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

        assert_ast_eq!(input, expected_ast);

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

        assert_ast_eq!(input, expected_ast);

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
            else_block: Some(ast![stmt!(StatementKind::Pass)]),
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn type_hints() {
        let input = "
def add(x: str, y: str) -> str:
    return x + y
";
        let ast = parse!(input, Statement);
        let expected_args = params![param!("x"), param!("y")];

        let StatementKind::FunctionDef { args, .. } = ast.kind else {
            panic!("Expected function def!")
        };

        assert_eq!(args, expected_args)
    }

    #[test]
    fn slices() {
        let input = "a[1:1:1]";
        let expected_ast = slice_op!(
            var!("a"),
            slice!(Some(int!(1)), Some(int!(1)), Some(int!(1)))
        );

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[2:5]";
        let expected_ast = slice_op!(var!("a"), slice!(Some(int!(2)), Some(int!(5)), None));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[:5]";
        let expected_ast = slice_op!(var!("a"), slice!(None, Some(int!(5)), None));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[3:]";
        let expected_ast = slice_op!(var!("a"), slice!(Some(int!(3)), None, None));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[::2]";
        let expected_ast = slice_op!(var!("a"), slice!(None, None, Some(int!(2))));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "a[:]";
        let expected_ast = slice_op!(var!("a"), slice!(None, None, None));

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "new_bases[i+shift:shift+1]";
        let expected_ast = slice_op!(
            var!("new_bases"),
            slice!(
                Some(bin_op!(var!("i"), Add, var!("shift"))),
                Some(bin_op!(var!("shift"), Add, int!(1))),
                None
            )
        );

        assert_ast_eq!(input, expected_ast, Expr);
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

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn scope_modifiers() {
        let input = "nonlocal var";
        let expected_ast = stmt!(StatementKind::Nonlocal(vec![ident("var")]));

        assert_ast_eq!(input, expected_ast);

        let input = "nonlocal var, var2";
        let expected_ast = stmt!(StatementKind::Nonlocal(vec![ident("var"), ident("var2")]));

        assert_ast_eq!(input, expected_ast);

        let input = "global var";
        let expected_ast = stmt!(StatementKind::Global(vec![ident("var")]));

        assert_ast_eq!(input, expected_ast);

        let input = "global var, var2";
        let expected_ast = stmt!(StatementKind::Global(vec![ident("var"), ident("var2")]));

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn ternary_operation() {
        let input = "a = 4 if True else 5";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::TernaryOp {
                condition: Box::new(bool!(true)),
                if_value: Box::new(int!(4)),
                else_value: Box::new(int!(5)),
            }
        );

        assert_ast_eq!(input, expected_ast);

        let input = "a = 4 + x if b == 6 else 5 << 2";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::TernaryOp {
                condition: Box::new(cmp_op!(var!("b"), Equals, int!(6))),
                if_value: Box::new(bin_op!(int!(4), Add, var!("x"))),
                else_value: Box::new(bin_op!(int!(5), LeftShift, int!(2))),
            }
        );

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn more_tokens() {
        let input = "Ellipsis";
        let expected_ast = Expr::Ellipsis;

        assert_ast_eq!(input, expected_ast, Expr);
    }

    #[test]
    fn generator_comprehension() {
        let input = "a = (i * 2 for i in b)";
        let expected_ast = stmt_assign!(
            var!("a"),
            Expr::GeneratorComprehension {
                body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
                clauses: vec![ForClause {
                    indices: vec![ident("i")],
                    iterable: var!("b"),
                    condition: None,
                }],
            }
        );

        assert_ast_eq!(input, expected_ast);

        let input = "foo(i * 2 for i in b)";
        let expected_ast = func_call!(
            "foo",
            call_args![Expr::GeneratorComprehension {
                body: Box::new(bin_op!(var!("i"), Mul, int!(2))),
                clauses: vec![ForClause {
                    indices: vec![ident("i")],
                    iterable: var!("b"),
                    condition: None,
                }],
            }]
        );

        assert_ast_eq!(input, expected_ast, Expr);
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
            body: ast![stmt!(StatementKind::Pass)],
            decorators: vec![],
            is_async: false,
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn unpacking() {
        let input = "(*l,)";
        let expected_ast = tuple![unary_op!(Unpack, var!("l"))];

        assert_ast_eq!(input, expected_ast, Expr);

        let input = "foo(a, *b[1:])";
        let expected_ast = func_call!(
            "foo",
            CallArgs {
                args: vec![var!("a")],
                kwargs: vec![],
                args_var: Some(Box::new(slice_op!(
                    var!("b"),
                    slice!(Some(int!(1)), None, None)
                ))),
            }
        );

        assert_ast_eq!(input, expected_ast, Expr);

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

        assert_ast_eq!(input, expected_ast);

        let input = "a, = b,";
        let expected_ast = stmt!(StatementKind::UnpackingAssignment {
            left: vec![var!("a")],
            right: tuple![var!("b")],
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn multiple_assignment() {
        let input = "a = b = True";
        let expected_ast = stmt!(StatementKind::MultipleAssignment {
            left: vec![var!("a"), var!("b")],
            right: bool!(true),
        });

        assert_ast_eq!(input, expected_ast);
    }

    #[test]
    fn invalid_identifier() {
        let input = "a.123";
        let e = expect_error!(input, Expr);
        assert_eq!(e, ParserError::SyntaxError);
    }
}
