use crate::{
    domain::Dunder,
    treewalk::{
        result::Raise, types::Exception, utils::args, TreewalkDisruption, TreewalkInterpreter,
        TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn store_var(&self, name: &str, value: TreewalkValue) {
        self.state.write(name, value);
    }

    pub fn store_index(
        &self,
        obj: TreewalkValue,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        match self.call_method(&obj, Dunder::SetItem, args![index, value]) {
            Ok(_) => {}
            Err(TreewalkDisruption::Error(e)) if e.exception.is_missing_attr(&Dunder::SetItem) => {
                // Remap this error so we don't raise AttributeError: __setitem__
                // This should really be handled via slots dispatch, which we don't support
                return Exception::type_error(format!(
                    "'{}' object does not support item assignment",
                    obj.get_type()
                ))
                .raise(self);
            }
            Err(e) => return Err(e),
        };
        Ok(())
    }

    pub fn store_member(
        &self,
        obj: TreewalkValue,
        field: &str,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        obj.clone()
            .into_member_writer()
            .ok_or_else(|| Exception::attribute_error(self.state.class_name(&obj), field))
            .raise(self)?
            .set_member(self, field, value)?;
        Ok(())
    }
}
