/// Represents a finished piece of Python code (not in progress!).
#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct Text(String);

impl Text {
    pub fn new(text: &str) -> Self {
        Self(text.to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}
