use crate::{
    domain::{MemphisResult, MemphisValue, Text},
    Engine, MemphisContext,
};

pub struct IncrementalContext {
    context: MemphisContext,
}

impl IncrementalContext {
    pub fn new(engine: Engine) -> Self {
        Self {
            context: MemphisContext::from_text(engine, Text::default()),
        }
    }

    pub fn add_text(&mut self, line: Text) {
        self.context.add_text(line);
    }

    pub fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.context.run()
    }
}
