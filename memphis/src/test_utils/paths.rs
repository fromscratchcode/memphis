use std::path::{Path, PathBuf};

pub fn resolve_workspace_path(path: &str) -> PathBuf {
    // Test helpers take paths relative to the workspace root, for example
    // `fixtures/test.py` or `memphis/src/fixtures/imports/regular_import.py`.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("memphis crate should live under the workspace root")
        .join(path)
}
