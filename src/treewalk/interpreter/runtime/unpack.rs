use crate::treewalk::{
    result::Raise, types::Exception, TreewalkInterpreter, TreewalkResult, TreewalkValue,
};

impl TreewalkInterpreter {
    pub fn unpack_iterable(
        &self,
        value: TreewalkValue,
        expected_len: usize,
    ) -> TreewalkResult<Vec<TreewalkValue>> {
        let iter = value.as_iterable().raise(self)?.into_iter();
        let items: Vec<_> = iter.collect();

        let actual_len = items.len();

        if actual_len > expected_len {
            return Exception::value_error(format!(
                "too many values to unpack (expected {})",
                expected_len
            ))
            .raise(self);
        }

        if actual_len < expected_len {
            return Exception::value_error(format!(
                "not enough values to unpack (expected {}, got {})",
                expected_len, actual_len
            ))
            .raise(self);
        }

        Ok(items)
    }
}
