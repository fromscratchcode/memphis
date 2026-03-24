use std::path::{Path, PathBuf};

/// A store of directories searched during each import.
#[derive(Default)]
pub struct ImportResolver {
    search_paths: Vec<PathBuf>,
}

impl ImportResolver {
    pub fn new() -> Self {
        Self {
            // treat the lib directory as a Memphis-compatible Python stdlib
            search_paths: vec![PathBuf::from("./lib")],
        }
    }

    /// Subsequent absolute imports will use the provided [`PathBuf`] to search for modules.
    pub fn register_root(&mut self, path: &Path) {
        let path = path
            .parent()
            .map_or_else(|| PathBuf::from("./"), |parent| parent.to_path_buf());

        // Insert at the start of the paths so this directory is searched first on subsequent
        // module imports
        // Also, avoid duplicate paths
        if !self.search_paths.contains(&path) {
            self.search_paths.insert(0, path);
        }
    }

    pub fn search_paths(&self) -> &[PathBuf] {
        &self.search_paths
    }
}
