use std::{
    io,
    path::{Path, PathBuf},
};

use crate::domain::Text;

#[derive(Debug, Clone)]
pub struct ScriptPath(PathBuf);

impl ScriptPath {
    pub fn new(path: impl AsRef<Path>) -> io::Result<Self> {
        let path = path.as_ref().canonicalize()?;
        // In the future, we could look for __main__ in this case
        if !path.metadata()?.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "script path must point to a file",
            ));
        }
        Ok(Self(path))
    }

    pub fn as_path(&self) -> &Path {
        &self.0
    }
}

/// Represents a Python source which comes from a file.
#[derive(Debug)]
pub struct Source {
    path: ScriptPath,
    text: Text,
}

impl Source {
    pub fn from_path(path: impl AsRef<Path>) -> io::Result<Self> {
        let text = std::fs::read_to_string(&path)?;
        let path = ScriptPath::new(path)?;
        Ok(Self::new(path, Text::new(&text)))
    }

    pub fn from_script_path(path: ScriptPath) -> io::Result<Self> {
        let text = std::fs::read_to_string(path.as_path())?;
        Ok(Self::new(path, Text::new(&text)))
    }

    pub fn path(&self) -> &ScriptPath {
        &self.path
    }

    pub fn text(&self) -> &Text {
        &self.text
    }

    fn new(path: ScriptPath, text: Text) -> Self {
        Self { path, text }
    }
}
