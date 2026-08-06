use crate::{
    core::Container,
    domain::Dunder,
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        types::{Class, Method},
        utils::InvokeArgs,
    },
};

impl TreewalkInterpreter {
    pub fn create_object(
        &self,
        class: Container<Class>,
        args: InvokeArgs,
    ) -> TreewalkResult<TreewalkValue> {
        // The [`Class`] must be explicitly passed to the [`Dunder::New`] method as this method is
        // never bound.
        // We clone here because these args will be consumed by the `Dunder::New` method call and
        // we still need a version of these for method call to `Dunder::Init`.
        let new_args = args
            .clone()
            .with_bound_new(TreewalkValue::Class(class.clone()));
        let object =
            self.call_method(&TreewalkValue::Class(class.clone()), Dunder::New, new_args)?;

        if self.is_instance_of(&object, &class) {
            self.call_method(&object, Dunder::Init, args)?;
        }

        Ok(object)
    }

    pub fn is_instance_of(&self, object: &TreewalkValue, class: &Container<Class>) -> bool {
        let obj_class = self.state.class_of(object);

        let effective_class = match object {
            TreewalkValue::Class(_) => obj_class.borrow().metaclass(),
            _ => obj_class,
        };

        effective_class.is_subclass_of(class)
    }

    pub fn resolve_descriptor(
        &self,
        attr: &TreewalkValue,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        // Similar to callable below, ideally we'd be able to handle this inside
        // `Result::as_nondata_descriptor` but we don't yet have a way to downcast in this way
        // (i.e. treat `S` as a different `dyn T` when `S : T`)
        if let Some(descriptor) = attr.clone().into_data_descriptor(self)? {
            return descriptor.get_attr(self, instance, owner);
        }

        // I'd love to find a way to combine this into [`Result::as_nondata_descriptor`] and move
        // this functionality onto the [`Callable`] trait somehow.
        if let Ok(callable) = attr.clone().as_callable() {
            // The new method is never bound. When called explicitly inside other metaclasses, the
            // class must be passed in by the calling metaclass.
            if callable.name() == String::from(Dunder::New) {
                return Ok(attr.clone());
            }

            return Ok(match instance {
                Some(instance) => {
                    TreewalkValue::Method(Container::new(Method::new(instance, callable)))
                }
                None => attr.clone(),
            });
        }

        match attr.clone().into_nondata_descriptor(self)? {
            Some(descriptor) => descriptor.get_attr(self, instance, owner),
            None => Ok(attr.clone()),
        }
    }
}
