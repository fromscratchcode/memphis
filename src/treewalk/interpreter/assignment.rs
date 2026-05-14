use crate::{
    parser::types::{Expr, LoopIndex},
    treewalk::{
        result::Raise, types::Exception, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    /// Assignment functionality shared by traditional assignment such as `a = 1` and compound
    /// assignment such as `a += 1`.
    pub fn execute_assignment(&self, name: &Expr, value: TreewalkValue) -> TreewalkResult<()> {
        match name {
            Expr::Variable(name) => {
                self.store_var(name.as_str(), value);
            }
            Expr::IndexAccess { object, index } => {
                let object_result = self.evaluate_expr(object)?;
                let index_result = self.evaluate_expr(index)?;
                self.store_index(object_result, index_result, value)?;
            }
            Expr::MemberAccess { object, field } => {
                let object_result = self.evaluate_expr(object)?;
                self.store_member(object_result, field.as_str(), value)?;
            }
            _ => return Exception::type_error("cannot assign").raise(self),
        }

        Ok(())
    }

    pub fn execute_loop_index_assignment(
        &self,
        index: &LoopIndex,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        match index {
            LoopIndex::Variable(name) => {
                self.store_var(name.as_str(), value);
            }
            LoopIndex::Tuple(names) => {
                let values = self.unpack_iterable(value, names.len())?;
                for (key, value) in names.iter().zip(values) {
                    self.store_var(key.as_str(), value);
                }
            }
        };

        Ok(())
    }

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
