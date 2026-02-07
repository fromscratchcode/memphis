use crate::treewalk::{
    macros::*,
    type_system::CloneableIterable,
    types::{Dict, DictKeys, DictValues, Exception, Tuple},
    DomainResult, TreewalkValue,
};

impl_iterable!(DictItemsIter);

#[derive(Default, PartialEq, Clone)]
pub struct DictItems {
    items: Vec<(TreewalkValue, TreewalkValue)>,
}

impl DictItems {
    pub fn from_iterable(iter: Box<dyn CloneableIterable>) -> DomainResult<Self> {
        let mut pairs: Vec<(TreewalkValue, TreewalkValue)> = vec![];
        for (index, item) in iter.enumerate() {
            // The item is often a tuple, but can really be any iterable which yields 2 values.
            let pair: Vec<_> = item.as_iterator()?.collect();

            // We cannot convert directly from a Vec to a tuple, we must first attempt to convert
            // to an array of a known and fixed length of 2.
            let pair_arr: [TreewalkValue; 2] = pair.clone().try_into().map_err(|_| {
                Exception::value_error(format!(
                    "dictionary update sequence element #{} has length {}; 2 is required",
                    index,
                    pair.len()
                ))
            })?;

            pairs.push(pair_arr.into());
        }

        Ok(Self::new(pairs))
    }

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

    pub fn to_dict(&self) -> Dict {
        let mut items = vec![];
        for pair in DictItemsIter::new(self.clone()) {
            let tuple = pair.as_tuple().unwrap();
            items.push((tuple.first().clone(), tuple.second().clone()));
        }

        Dict::from_items(items).unwrap()
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
