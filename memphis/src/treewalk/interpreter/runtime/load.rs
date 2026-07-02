use crate::{
    core::{log, LogLevel},
    domain::Dunder,
    treewalk::{
        result::Raise, type_system::CloneableCallable, types::Exception, utils::args,
        TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn load_var(&self, name: &str) -> TreewalkResult<TreewalkValue> {
        self.state
            .read(name)
            .ok_or_else(|| Exception::name_error(name))
            .raise(self)
    }

    pub fn load_callable(&self, name: &str) -> TreewalkResult<Box<dyn CloneableCallable>> {
        self.load_var(name)?.as_callable().raise(self)
    }

    pub fn load_index(
        &self,
        object: &TreewalkValue,
        index: &TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        match self.call_method(object, Dunder::GetItem, args![index.clone()]) {
            Ok(i) => Ok(i),
            Err(TreewalkDisruption::Error(e)) if e.exception.is_missing_attr(&Dunder::GetItem) => {
                // Remap this error so we don't raise AttributeError: __getitem__
                // This should really be handled via slots dispatch, which we don't support
                Exception::type_error(format!(
                    "'{}' object is not subscriptable",
                    object.get_type()
                ))
                .raise(self)
            }
            Err(e) => Err(e),
        }
    }

    pub fn load_member<S>(&self, result: &TreewalkValue, field: S) -> TreewalkResult<TreewalkValue>
    where
        S: AsRef<str>,
    {
        log(LogLevel::Debug, || {
            format!("Member access {:?}.{}", result, field.as_ref())
        });
        result
            .clone()
            .into_member_reader(self)
            .get_member(self, field.as_ref())?
            .ok_or_else(|| {
                Exception::attribute_error(self.state.class_name(result), field.as_ref())
            })
            .raise(self)
    }

    pub fn load_method<S>(
        &self,
        receiver: &TreewalkValue,
        name: S,
    ) -> TreewalkResult<Box<dyn CloneableCallable>>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        self.load_member(receiver, name)?.as_callable().raise(self)
    }
}
