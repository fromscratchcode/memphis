#[derive(Clone, Debug, PartialEq)]
pub enum ReplResult {
    None,
    Ok(String),
    Err(String),
}

pub struct ReplOutput {
    pub stdout: String,
    pub result: ReplResult,
}

pub enum ReplStep {
    Complete(ReplOutput),
    Incomplete { indent: usize },
}

impl ReplStep {
    pub fn initial() -> Self {
        let output = ReplOutput {
            stdout: String::from(""),
            result: ReplResult::None,
        };
        Self::Complete(output)
    }

    pub fn indent_level(&self) -> usize {
        match self {
            ReplStep::Complete(_) => 0,
            ReplStep::Incomplete { indent } => *indent,
        }
    }

    pub fn is_complete(&self) -> bool {
        matches!(self, ReplStep::Complete { .. })
    }
}
