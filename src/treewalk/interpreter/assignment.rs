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
                self.state.write(name.as_str(), value);
            }
            Expr::IndexAccess { object, index } => {
                let index_result = self.evaluate_expr(index)?;
                let object_result = self.evaluate_expr(object)?;
                object_result
                    .clone()
                    .into_index_write(self)?
                    .ok_or_else(|| {
                        Exception::type_error(format!(
                            "'{}' object does not support item assignment",
                            object_result.get_type()
                        ))
                    })
                    .raise(self)?
                    .setitem(self, index_result, value)?;
            }
            Expr::MemberAccess { object, field } => {
                let result = self.evaluate_expr(object)?;
                result
                    .clone()
                    .into_member_writer()
                    .ok_or_else(|| {
                        Exception::attribute_error(self.state.class_name(&result), field.as_str())
                    })
                    .raise(self)?
                    .set_member(self, field.as_str(), value)?;
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
                self.state.write(name.as_str(), value);
            }
            LoopIndex::Tuple(names) => {
                let values = self.unpack_iterable(value, names.len())?;
                for (key, value) in names.iter().zip(values) {
                    self.state.write(key.as_str(), value);
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
