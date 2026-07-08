use std::path::PathBuf;

use crate::domain::ScriptPath;

#[derive(Debug, Clone)]
pub enum ModuleOrigin {
    File(ScriptPath),
    Stdin,
    Builtin,
    Synthetic,
}

impl ModuleOrigin {
    pub fn path(&self) -> PathBuf {
        match self {
            ModuleOrigin::File(p) => p.as_path().to_path_buf(),
            ModuleOrigin::Stdin => PathBuf::from("<stdin>"),
            ModuleOrigin::Builtin => PathBuf::from("<builtin>"),
            ModuleOrigin::Synthetic => PathBuf::from("<synthetic>"),
        }
    }

    pub fn path_str(&self) -> String {
        self.path()
            .to_str()
            .expect("Path contains invalid unicode")
            .to_string()
    }
}
