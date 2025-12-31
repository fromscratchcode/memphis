use crate::{
    domain::{MemphisResult, MemphisValue, Source},
    Engine, MemphisContext,
};

pub struct IncrementalContext {
    context: MemphisContext,
}

impl IncrementalContext {
    pub fn new(engine: Engine) -> Self {
        Self {
            context: MemphisContext::new(engine, Source::from_text("")),
        }
    }

    pub fn add_line(&mut self, line: &str) {
        self.context.add_line(line);
    }

    pub fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.context.run()
    }
}
