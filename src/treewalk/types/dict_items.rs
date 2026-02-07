use crate::treewalk::{
    macros::*,
    types::{DictKeys, DictValues, Tuple},
    TreewalkValue,
};

impl_iterable!(DictItemsIter);

#[derive(Default, PartialEq, Clone)]
pub struct DictItems {
    items: Vec<(TreewalkValue, TreewalkValue)>,
}

impl DictItems {
    pub fn new(items: Vec<(TreewalkValue, TreewalkValue)>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[(TreewalkValue, TreewalkValue)] {
        &self.items
    }

    fn keys(&self) -> Vec<TreewalkValue> {
        self.items.iter().map(|i| i.0.clone()).collect()
    }

    fn values(&self) -> Vec<TreewalkValue> {
        self.items.iter().map(|i| i.1.clone()).collect()
    }

    pub fn to_keys(&self) -> DictKeys {
        DictKeys::new(self.keys())
    }

    pub fn to_values(&self) -> DictValues {
        DictValues::new(self.values())
    }
}

impl IntoIterator for DictItems {
    type Item = TreewalkValue;
    type IntoIter = DictItemsIter;

    fn into_iter(self) -> Self::IntoIter {
        DictItemsIter::new(self)
    }
}

#[derive(Clone)]
pub struct DictItemsIter(DictItems);

impl DictItemsIter {
    pub fn new(dict_items: DictItems) -> Self {
        DictItemsIter(dict_items)
    }
}

impl Iterator for DictItemsIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            let key = removed.0;
            let value = removed.1;
            Some(TreewalkValue::Tuple(Tuple::new(vec![key, value])))
        }
    }
}
