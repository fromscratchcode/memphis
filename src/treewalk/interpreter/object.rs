use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{types::Class, utils::Args, TreewalkInterpreter, TreewalkResult, TreewalkValue},
};

impl TreewalkInterpreter {
    pub fn create_object(
        &self,
        class: Container<Class>,
        args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        // We have to handle calls to `type()` with only one parameter as a special case because
        // this doesn't actually call the `Type::Type` `Dunder::New` method, which expects more
        // arguments and would return a new class. Overloading the `Dunder::Init` method
        // here on `Type::Type` would also create unintended behaviors.
        if class.borrow().is_type(&Type::Type) {
            assert_eq!(args.len(), 1);
            return Ok(self.state.class_of_value(&args.get_arg(0)));
        };

        // The [`Class`] must be explicitly passed to the [`Dunder::New`] method as this method is
        // never bound.
        // We clone here because these args will be consumed by the `Dunder::New` method call and
        // we still need a version of these for method call to `Dunder::Init`.
        let new_args = args
            .clone()
            .with_bound_new(TreewalkValue::Class(class.clone()));
        let object = self.call_method(&TreewalkValue::Class(class), Dunder::New, new_args)?;

        self.call_method(&object, Dunder::Init, args)?;

        Ok(object)
    }
}
