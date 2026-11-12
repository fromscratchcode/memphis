use crate::domain::Identifier;
#[cfg(feature = "wasm")]
use serde::Serialize;

/// A raw module path exactly as written by the user.
/// No resolution, no filesystem meaning, only validated to be non-empty.
/// Used only during parsing.
#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ModulePath(Vec<Identifier>);

impl ModulePath {
    pub fn new(segments: Vec<Identifier>) -> Self {
        assert!(!segments.is_empty());
        Self(segments)
    }

    pub fn as_str(&self) -> String {
        self.segments_as_str().join(".")
    }

    pub fn segments(&self) -> &[Identifier] {
        &self.0
    }

    pub fn segments_as_str(&self) -> Vec<String> {
        self.0.iter().map(|s| s.to_string()).collect::<Vec<_>>()
    }

    pub fn head(&self) -> &Identifier {
        self.0.first().expect("invariant: empty module path")
    }

    #[cfg(test)]
    pub fn from(path_str: &str) -> Option<Self> {
        if path_str.is_empty() {
            None
        } else {
            let segments = path_str
                .split('.')
                .map(|s| Identifier::new(s).expect("Invalid identifier"))
                .collect();
            Some(ModulePath::new(segments))
        }
    }
}

/// Parser representation of the module path that is found in selective import statements.
/// This specifically holds inside the brackets:
/// from [a.b.c] import *
/// from [.a.b] import *
#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum FromImportPath {
    /// When an import begins with no dots, it is an absolute import and this just wraps a
    /// ModulePath.
    Absolute(ModulePath),
    /// Any import path which _starts_ with a dot. The `usize` represents the number of levels up
    /// to look for the path. This means that this example is in the same directory.
    ///
    /// ```python
    /// from .package.module import symbol
    /// ```
    ///
    /// While this would look one directory up
    ///
    /// ```python
    /// from ..package.module import symbol
    /// ```
    Relative {
        levels: usize,
        path: Option<ModulePath>,
    },
}

impl FromImportPath {
    #[cfg(test)]
    pub fn from(path_str: &str) -> Self {
        // Count leading dots (if any)
        let mut levels = 0;
        for c in path_str.chars() {
            if c == '.' {
                levels += 1;
            } else {
                break;
            }
        }

        // Split remaining text by '.'
        let rest = &path_str[levels..];
        let path = ModulePath::from(rest);

        if levels == 0 {
            FromImportPath::Absolute(path.expect("Expected path"))
        } else {
            FromImportPath::Relative { levels, path }
        }
    }
}

#[cfg(test)]
mod test_module_path {
    use crate::parser::test_utils::ident;

    use super::*;

    #[test]
    fn module_path_as_str() {
        let p = ModulePath::new(vec![ident!("a"), ident!("b")]);
        assert_eq!(p.as_str(), "a.b");
    }

    #[test]
    fn module_path_head() {
        let p = ModulePath::new(vec![ident!("a"), ident!("b")]);
        assert_eq!(p.head(), &ident!("a"));
    }
}
