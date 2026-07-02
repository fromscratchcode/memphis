use std::{
    io,
    path::{Path, PathBuf},
};

use crate::domain::Text;

/// Represents a Python source which comes from a file.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Source {
    path: PathBuf,
    text: Text,
}

impl Source {
    pub fn from_path<P>(filepath: P) -> io::Result<Self>
    where
        P: AsRef<Path>,
    {
        let text = std::fs::read_to_string(&filepath)?;
        let absolute_path = filepath.as_ref().canonicalize()?;
        Ok(Self::new(absolute_path, Text::new(&text)))
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn text(&self) -> &Text {
        &self.text
    }

    fn new(path: PathBuf, text: Text) -> Self {
        Self { path, text }
    }
}
