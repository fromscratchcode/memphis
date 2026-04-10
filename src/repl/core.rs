use crate::{domain::Text, repl::parser::ReplParser, Engine, MemphisContext};

#[derive(Debug, PartialEq)]
pub enum ReplResult {
    None,
    Ok(String),
    Err(String),
}

#[derive(Debug)]
pub struct ReplOutput {
    pub result: ReplResult,
    #[allow(unused)]
    pub is_complete: bool,
    #[allow(unused)]
    pub indent_level: usize,
}

pub struct ReplCore {
    context: MemphisContext,

    parser: ReplParser,

    /// The current statement being constructed.
    input: String,
}

impl ReplCore {
    pub fn new(engine: Engine) -> Self {
        Self {
            context: MemphisContext::stdin(engine),
            parser: ReplParser::new(),
            input: String::new(),
        }
    }

    pub fn reset(&mut self) {
        self.input.clear();
        self.parser = ReplParser::new();
    }

    pub fn input_line(&mut self, line: &str) -> ReplOutput {
        self.input.push_str(line);

        let text = Text::new(&self.input);
        self.parser.analyze_text(&text);

        let result = if !self.is_incomplete() {
            let result = self.eval(text);
            self.input.clear();
            result
        } else {
            ReplResult::None
        };

        ReplOutput {
            result,
            is_complete: !self.is_incomplete(),
            indent_level: self.indent_level(),
        }
    }

    pub fn is_incomplete(&self) -> bool {
        self.parser.is_incomplete()
    }

    pub fn indent_level(&self) -> usize {
        self.parser.indent_level()
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

        assert!(out.is_complete);
        assert_eq!(out.indent_level, 0);
        assert_eq!(out.result, ReplResult::Ok("3".to_string()));
    }

    #[test]
    fn test_statement_has_no_output() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out = core.input_line("a = 5\n");

        assert!(out.is_complete);
        assert_eq!(out.indent_level, 0);
        assert_eq!(out.result, ReplResult::None);
    }

    #[test]
    fn test_multiline_block() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out1 = core.input_line("def foo():\n");
        assert!(!out1.is_complete);

        let out2 = core.input_line("    return 10\n");
        assert!(!out2.is_complete);

        let out3 = core.input_line("\n");
        assert!(out3.is_complete);

        let out4 = core.input_line("foo()\n");
        assert_eq!(out4.result, ReplResult::Ok("10".to_string()));
    }

    #[test]
    fn test_reset_clears_incomplete_input() {
        let mut core = ReplCore::new(Engine::Treewalk);

        core.input_line("if x:\n");
        assert!(core.is_incomplete());

        core.reset();

        assert!(!core.is_incomplete());

        let out = core.input_line("123\n");
        assert_eq!(out.result, ReplResult::Ok("123".to_string()));
    }

    #[test]
    fn test_error_does_not_poison_future_input() {
        let mut core = ReplCore::new(Engine::Treewalk);

        let out1 = core.input_line("undefined_var\n");
        assert!(matches!(out1.result, ReplResult::Err(_)));

        let out2 = core.input_line("1 + 1\n");
        assert_eq!(out2.result, ReplResult::Ok("2".to_string()));
    }
}
