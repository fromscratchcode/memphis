use crate::treewalk::{
    TreewalkInterpreter, TreewalkResult, TreewalkValue, iterator::collect, result::Raise,
    types::Exception,
};

impl TreewalkInterpreter {
    pub fn unpack_iterable(
        &self,
        value: TreewalkValue,
        expected_len: usize,
    ) -> TreewalkResult<Vec<TreewalkValue>> {
        let iter = value.as_iterator().raise(self)?;
        let items = collect(iter)?;

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
