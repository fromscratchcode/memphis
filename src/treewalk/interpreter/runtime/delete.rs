use crate::{
    domain::Dunder,
    treewalk::{
        result::Raise, types::Exception, utils::args, TreewalkDisruption, TreewalkInterpreter,
        TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn delete_var(&self, name: &str) {
        self.state.delete(name);
    }

    pub fn delete_index(&self, obj: TreewalkValue, index: TreewalkValue) -> TreewalkResult<()> {
        match self.call_method(&obj, Dunder::DelItem, args![index]) {
            Ok(_) => {}
            Err(TreewalkDisruption::Error(e)) if e.exception.is_missing_attr(&Dunder::DelItem) => {
                // Remap this error so we don't raise AttributeError: __delitem__
                // This should really be handled via slots dispatch, which we don't support
                return Exception::type_error(format!(
                    "'{}' object does not support item deletion",
                    obj.get_type()
                ))
                .raise(self);
            }
            Err(e) => return Err(e),
        };
        Ok(())
    }

    pub fn delete_member(&self, obj: TreewalkValue, field: &str) -> TreewalkResult<()> {
        obj.clone()
            .into_member_writer()
            .ok_or_else(|| Exception::attribute_error(self.state.class_name(&obj), field))
            .raise(self)?
            .delete_member(self, field)?;
        Ok(())
    }
}
