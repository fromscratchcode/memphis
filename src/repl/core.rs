use crate::{domain::Text, repl::parser::ReplParser, Engine, MemphisContext};

#[derive(Debug)]
pub enum ReplResult {
    None,
    Ok(String),
    Err(String),
}

#[derive(Debug)]
pub struct ReplOutput {
    pub result: ReplResult,
    pub is_complete: bool,
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
