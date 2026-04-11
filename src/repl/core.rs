use crate::{domain::Text, repl::parser::repl_parser, Engine, MemphisContext};

#[derive(Debug, PartialEq)]
pub enum ReplResult {
    None,
    Ok(String),
    Err(String),
}

pub enum ReplStep {
    Complete { result: ReplResult },
    Incomplete { indent: usize },
}

impl ReplStep {
    pub fn initial() -> Self {
        Self::Complete {
            result: ReplResult::None,
        }
    }

    pub fn indent_level(&self) -> usize {
        match self {
            ReplStep::Complete { .. } => 0,
            ReplStep::Incomplete { indent } => *indent,
        }
    }

    pub fn is_complete(&self) -> bool {
        matches!(self, ReplStep::Complete { .. })
    }

    pub fn output(&self) -> Option<&ReplResult> {
        match self {
            ReplStep::Complete { result } => Some(result),
            ReplStep::Incomplete { .. } => None,
        }
    }
}

pub struct ReplCore {
    /// The current statement being constructed.
    input: String,

    context: MemphisContext,
}

impl ReplCore {
    pub fn new(engine: Engine) -> Self {
        Self {
            input: String::new(),
            context: MemphisContext::stdin(engine),
        }
    }

    pub fn reset(&mut self) {
        self.input.clear();
    }

    pub fn input_line(&mut self, line: &str) -> ReplStep {
        self.input.push_str(line);

        let text = Text::new(&self.input);
        let parse_step = repl_parser::analyze(&text);

        match parse_step {
            repl_parser::ParseStep::Incomplete { indent } => ReplStep::Incomplete { indent },
            repl_parser::ParseStep::Complete | repl_parser::ParseStep::Error => {
                // We still run parser errors through eval because that pipeline will generate the
                // correct errors, some of which may be heap allocated.
                let result = self.eval(text);
                self.input.clear();

                ReplStep::Complete { result }
            }
        }
    }

    fn eval(&mut self, text: Text) -> ReplResult {
        // In order to not special-case parser errors still run those through the eval method.
        match self.context.eval(text) {
            Ok(result) => match result.is_none() {
                true => ReplResult::None,
                false => ReplResult::Ok(result.to_string()),
            },
            Err(err) => ReplResult::Err(err.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out = core.input_line("1 + 2\n");

        match out {
            ReplStep::Complete { result } => {
                assert_eq!(result, ReplResult::Ok("3".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_statement_has_no_output() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out = core.input_line("a = 5\n");

        match out {
            ReplStep::Complete { result } => {
                assert_eq!(result, ReplResult::None);
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_multiline_block() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out1 = core.input_line("def foo():\n");
        match out1 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out2 = core.input_line("    return 10\n");
        match out2 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out3 = core.input_line("\n");
        match out3 {
            ReplStep::Complete { .. } => {}
            _ => panic!("expected complete"),
        }

        let out4 = core.input_line("foo()\n");
        match out4 {
            ReplStep::Complete { result } => {
                assert_eq!(result, ReplResult::Ok("10".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_reset_clears_incomplete_input() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out1 = core.input_line("if x:\n");
        match out1 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        core.reset();

        let out2 = core.input_line("123\n");
        match out2 {
            ReplStep::Complete { result } => {
                assert_eq!(result, ReplResult::Ok("123".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_error_does_not_poison_future_input() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out1 = core.input_line("undefined_var\n");
        match out1 {
            ReplStep::Complete { result } => {
                assert!(matches!(result, ReplResult::Err(_)));
            }
            _ => panic!("expected complete"),
        }

        let out2 = core.input_line("1 + 1\n");
        match out2 {
            ReplStep::Complete { result } => {
                assert_eq!(result, ReplResult::Ok("2".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }
}
