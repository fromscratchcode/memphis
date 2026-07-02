use std::path::PathBuf;

use crate::domain::ModuleName;

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
    pub path: PathBuf,
}
