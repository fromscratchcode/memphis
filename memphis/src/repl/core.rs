use crate::{
    Container, Engine, HostIo, MemphisContext, ModuleOrigin,
    core::Capture,
    domain::Text,
    repl::{
        ReplResult, ReplStep,
        parser::{self, ParseStep},
        types::ReplOutput,
    },
};

pub struct ReplCore {
    /// The current statement being constructed.
    input: String,
    context: MemphisContext,
    capture: Container<Capture>,
}

impl ReplCore {
    pub fn new(engine: Engine, io: impl HostIo + 'static, capture: Container<Capture>) -> Self {
        Self {
            input: String::new(),
            context: MemphisContext::new(engine, ModuleOrigin::Stdin, io),
            capture,
        }
    }

    pub fn engine(&self) -> &Engine {
        self.context.engine()
    }

    pub fn reset(&mut self) {
        self.input.clear();
    }

    pub fn input_line(&mut self, line: &str) -> ReplStep {
        self.input.push_str(line);

        let text = Text::new(&self.input);
        let parse_step = parser::analyze(&text);

        match parse_step {
            ParseStep::Incomplete { indent } => ReplStep::Incomplete { indent },
            ParseStep::Complete | ParseStep::Error => {
                // We still run parser errors through eval because that pipeline will generate the
                // correct errors, some of which may be heap allocated.
                let result = self.eval(text);
                self.input.clear();

                let stdout = self.capture.borrow_mut().take_output();
                let output = ReplOutput { stdout, result };
                ReplStep::Complete(output)
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
    use crate::test_io::TestIo;

    use super::*;

    fn init() -> ReplCore {
        let (io, capture) = TestIo::new();
        ReplCore::new(Engine::Treewalk, io, capture)
    }

    #[test]
    fn test_expr() {
        let mut core = init();

        let out = core.input_line("1 + 2\n");

        match out {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::Ok("3".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_statement_has_no_output() {
        let mut core = init();

        let out = core.input_line("a = 5\n");

        match out {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::None);
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_print_statement_has_no_output_but_has_side_effects() {
        let mut core = init();

        let out = core.input_line("print(123)\n");

        match out {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from("123\n"));
                assert_eq!(output.result, ReplResult::None);
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_multiline_block() {
        let mut core = init();

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
            ReplStep::Complete(_) => {}
            _ => panic!("expected complete"),
        }

        let out4 = core.input_line("foo()\n");
        match out4 {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::Ok("10".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_multiline_list() {
        let mut core = init();

        let out1 = core.input_line("[\n");
        match out1 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out2 = core.input_line("\n");
        match out2 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out3 = core.input_line("1\n");
        match out3 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out4 = core.input_line("]\n");
        match out4 {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::Ok("[1]".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_multiline_string() {
        let mut core = init();

        let out1 = core.input_line("\"\"\"\n");
        match out1 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out2 = core.input_line("\n");
        match out2 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out3 = core.input_line("1\n");
        match out3 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        let out4 = core.input_line("\"\"\"\n");
        match out4 {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::Ok("\n\n1\n".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_reset_clears_incomplete_input() {
        let mut core = init();

        let out1 = core.input_line("if x:\n");
        match out1 {
            ReplStep::Incomplete { .. } => {}
            _ => panic!("expected incomplete"),
        }

        core.reset();

        let out2 = core.input_line("123\n");
        match out2 {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::Ok("123".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }

    #[test]
    fn test_error_does_not_poison_future_input() {
        let mut core = init();

        let out1 = core.input_line("undefined_var\n");
        match out1 {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert!(matches!(output.result, ReplResult::Err(_)));
            }
            _ => panic!("expected complete"),
        }

        let out2 = core.input_line("1 + 1\n");
        match out2 {
            ReplStep::Complete(output) => {
                assert_eq!(output.stdout, String::from(""));
                assert_eq!(output.result, ReplResult::Ok("2".to_string()));
            }
            _ => panic!("expected complete"),
        }
    }
}
