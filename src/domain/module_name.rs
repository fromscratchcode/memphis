use std::fmt::{Display, Error, Formatter};

use crate::domain::Dunder;

/// A resolved, absolute module name used at runtime.
/// Always valid, never relative. Built by the resolver
/// or by the runtime for builtin modules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    pub fn new(segments: Vec<String>) -> Self {
        assert!(!segments.is_empty());
        Self(segments)
    }

    pub fn from_segments<S: AsRef<str>>(segments: &[S]) -> Self {
        Self::new(segments.iter().map(|s| s.as_ref().to_string()).collect())
    }

    pub fn from_dotted(s: &str) -> Self {
        let segments = s.split('.').map(|s| s.to_string()).collect();
        Self::new(segments)
    }

    pub fn main() -> Self {
        Self::from_segments(&[Dunder::Main])
    }

    pub fn as_str(&self) -> String {
        self.0.join(".")
    }

    pub fn segments(&self) -> &[String] {
        &self.0
    }

    pub fn head(&self) -> &str {
        self.0
            .first()
            .map(|s| s.as_str())
            .expect("Invalid ModuleName")
    }

    pub fn tail(&self) -> &str {
        self.0
            .last()
            .map(|s| s.as_str())
            .expect("Invalid ModuleName")
    }

    pub fn parent(&self) -> Option<ModuleName> {
        self.strip_last(1)
    }

    /// Removes `n` segments from the end of the module name.
    ///
    /// This operation is structural, not semantic: it represents walking upward in the module
    /// hierarchy.
    ///
    /// Returns `None` if removing `n` segments would underflow or erase the module name entirely.
    ///
    /// Python-specific relative import semantics (e.g. dot handling) are layered on top of this
    /// operation in the resolver.
    pub fn strip_last(&self, n: usize) -> Option<ModuleName> {
        if n >= self.0.len() {
            return None;
        }

        let new_len = self.0.len() - n;
        Some(ModuleName(self.0[..new_len].to_vec()))
    }

    /// Joins additional segments onto the module name (used for relative imports).
    pub fn join<I>(&self, tail: I) -> ModuleName
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let mut segments = self.0.clone();
        for s in tail {
            segments.push(s.as_ref().to_string());
        }
        ModuleName(segments)
    }

    /// Iterate from the full module name downward to its parents,
    /// excluding the full name itself.
    ///
    /// Example:
    ///   "a.b.c" -> yields ["a.b", "a"]
    pub fn parents(&self) -> impl DoubleEndedIterator<Item = ModuleName> + '_ {
        (1..self.0.len()).filter_map(move |n| self.strip_last(n))
    }
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.as_str())
    }
}

impl From<&ModuleName> for String {
    fn from(value: &ModuleName) -> Self {
        value.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parents_of_three_segments() {
        let m = ModuleName::from_segments(&["a", "b", "c"]);
        let parents: Vec<_> = m.parents().collect();

        assert_eq!(
            parents,
            vec![
                ModuleName::from_segments(&["a", "b"]),
                ModuleName::from_segments(&["a"]),
            ]
        );
    }

    #[test]
    fn parents_of_two_segments() {
        let m = ModuleName::from_segments(&["a", "b"]);
        let parents: Vec<_> = m.parents().collect();

        assert_eq!(parents, vec![ModuleName::from_segments(&["a"]),]);
    }

    #[test]
    fn parents_of_one_segment_is_empty() {
        let m = ModuleName::from_segments(&["a"]);
        let parents: Vec<_> = m.parents().collect();

        assert!(parents.is_empty());
    }

    #[test]
    fn parents_is_double_ended_iterator() {
        let m = ModuleName::from_segments(&["x", "y", "z"]);
        let mut it = m.parents();

        assert_eq!(it.next(), Some(ModuleName::from_segments(&["x", "y"])));
        assert_eq!(it.next_back(), Some(ModuleName::from_segments(&["x"])));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn from_dotted() {
        let m = ModuleName::from_dotted("pkg.mod");
        assert_eq!(m, ModuleName::from_segments(&["pkg", "mod"]));
    }

    #[test]
    fn parent_of_three_segments() {
        let m = ModuleName::from_segments(&["a", "b", "c"]);
        assert_eq!(m.parent(), Some(ModuleName::from_segments(&["a", "b"])));
    }

    #[test]
    fn parent_of_one_segment_is_none() {
        let m = ModuleName::from_segments(&["a"]);
        assert_eq!(m.parent(), None);
    }
}
