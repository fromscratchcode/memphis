use std::io;

use crate::{
    Source,
    domain::{ModuleName, ScriptPath},
};

/// Result of resolving a module import.
///
/// Captures the semantic identity of the module (`__name__`, `__package__`) alongside its physical
/// source location.
///
/// This struct is intentionally short-lived: it represents the boundary between import resolution
/// (what module this is) and execution (loading and evaluating its code).
pub struct ResolvedModule {
    pub name: ModuleName,            // __name__
    pub package: Option<ModuleName>, // __package__
    pub path: ScriptPath,
}

impl ResolvedModule {
    pub fn load(self) -> io::Result<LoadedModule> {
        let source = Source::from_script_path(self.path)?;
        Ok(LoadedModule {
            name: self.name,
            package: self.package,
            source,
        })
    }
}

/// A module after import resolution has loaded its source text from disk.
/// Carries the semantic module identity together with executable source.
pub struct LoadedModule {
    pub name: ModuleName,            // __name__
    pub package: Option<ModuleName>, // __package__
    pub source: Source,
}
