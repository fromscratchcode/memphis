use crate::domain::{MemphisResult, MemphisValue, Text};

pub trait Interpreter {
    fn eval(&mut self, text: Text) -> MemphisResult<MemphisValue>;
}
