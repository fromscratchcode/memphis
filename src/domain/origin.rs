use std::path::PathBuf;

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleOrigin {
    File(PathBuf),
    Stdin,
    Builtin,
    Synthetic,
}

impl ModuleOrigin {
    pub fn path(&self) -> PathBuf {
        match self {
            ModuleOrigin::File(p) => p.to_path_buf(),
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
