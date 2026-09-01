#[derive(Clone, Debug, PartialEq)]
pub enum ReplResult {
    None,
    Ok(String),
    Err(String),
}

pub enum ReplStep {
    Complete(ReplResult),
    Incomplete { indent: usize },
}

impl ReplStep {
    pub fn initial() -> Self {
        Self::Complete(ReplResult::None)
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

    pub fn result(&self) -> Option<&ReplResult> {
        match self {
            ReplStep::Complete(result) => Some(result),
            ReplStep::Incomplete { .. } => None,
        }
    }
}
