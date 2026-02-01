use std::path::{Path, PathBuf};

use crate::domain::{Dunder, FromImportPath, ModuleName, ModulePath, ResolvedModule};

pub enum ImportResolutionError {
    NoParentPackage,
    BeyondTopLevel,
}

impl ImportResolutionError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::NoParentPackage => "attempted relative import with no known parent package",
            Self::BeyondTopLevel => "attempted relative import beyond top-level package",
        }
    }
}

pub fn resolve_import_path(
    import_path: &FromImportPath,
    current_package: &Option<ModuleName>,
) -> Result<ModuleName, ImportResolutionError> {
    match import_path {
        FromImportPath::Absolute(mp) => Ok(resolve_absolute_path(mp)),
        FromImportPath::Relative(levels, tail) => {
            if let Some(package) = current_package {
                // - One leading dot (`.`) refers to the current package
                // - Additional dots (`..`, `...`) walk upward in the package hierarchy
                //
                // Since `current_package` already names the containing package,
                // we strip `levels - 1` segments to compute the base.
                let base = package
                    .strip_last(*levels - 1)
                    .ok_or(ImportResolutionError::BeyondTopLevel)?;
                Ok(base.join(tail.segments_as_str()))
            } else {
                Err(ImportResolutionError::NoParentPackage)
            }
        }
    }
}

// Convert from a parser `ModulePath` into a runtime `ModuleName`. For absolute paths, this is a
// direct mapping.
pub fn resolve_absolute_path(module_path: &ModulePath) -> ModuleName {
    ModuleName::from_segments(&module_path.segments_as_str())
}

/// Finds a module but does not read it (returns absolute path).
pub fn resolve(requested: &ModuleName, search_paths: &[PathBuf]) -> Option<ResolvedModule> {
    for root in search_paths {
        let candidates = expand_path(root, requested.segments());

        for path in candidates {
            if path.exists() {
                let path = path.canonicalize().ok()?;

                let package = if path.ends_with(Dunder::Init.py_file()) {
                    // package
                    Some(requested.clone())
                } else {
                    // regular module
                    requested.parent()
                };

                return Some(ResolvedModule {
                    name: requested.clone(),
                    package,
                    path,
                });
            }
        }
    }

    None
}

/// For a given path and segments, this returns both the `../base.py` and `../base/__init__.py`
/// versions.
fn expand_path(path: &Path, segments: &[String]) -> [PathBuf; 2] {
    // Split the slice into the last segment and the rest
    let (last, rest) = match segments.split_last() {
        Some((last, rest)) => (last, rest),
        None => panic!("Path segments must not be empty!"),
    };

    let append_segment = |mut acc: PathBuf, segment: &String| {
        acc.push(segment);
        acc
    };

    // Build the `../base/segment_one/segment_two.py` path
    let base_path = rest
        .iter()
        .fold(path.to_path_buf(), append_segment)
        .join(format!("{last}.py"));

    // Build the `../base/segment_one/segment_two/__init__.py` path
    let init_path = segments
        .iter()
        .fold(path.to_path_buf(), append_segment)
        .join(Dunder::Init.py_file());

    [base_path, init_path]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_path_with_multiple_segments() {
        let path = Path::new("/base");
        let segments = ["subdir".to_string(), "file".to_string()];

        let [base_path, init_path] = expand_path(path, &segments);

        assert_eq!(base_path, Path::new("/base/subdir/file.py"));
        assert_eq!(init_path, Path::new("/base/subdir/file/__init__.py"));
    }

    #[test]
    fn test_expand_path_with_single_segment() {
        let path = Path::new("/base");
        let segments = ["file".to_string()];

        let [base_path, init_path] = expand_path(path, &segments);

        assert_eq!(base_path, Path::new("/base/file.py"));
        assert_eq!(init_path, Path::new("/base/file/__init__.py"));
    }

    #[test]
    #[should_panic]
    fn test_expand_path_with_empty_segments() {
        let path = Path::new("/base");
        let segments = [];

        let _ = expand_path(path, &segments);
    }
}
