use crate::treewalk::{macros::*, TreewalkValue};

impl_iterable!(DictKeysIter);

#[derive(Debug, PartialEq, Clone)]
pub struct DictKeys {
    items: Vec<TreewalkValue>,
}

impl DictKeys {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }
}

impl IntoIterator for DictKeys {
    type Item = TreewalkValue;
    type IntoIter = DictKeysIter;

    fn into_iter(self) -> Self::IntoIter {
        DictKeysIter::new(self)
    }
}

#[derive(Clone)]
pub struct DictKeysIter(DictKeys);

impl DictKeysIter {
    pub fn new(dict_keys: DictKeys) -> Self {
        DictKeysIter(dict_keys)
    }
}

impl Iterator for DictKeysIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            Some(removed)
        }
    }
}
