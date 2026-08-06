use std::any::Any;

use crate::{
    core::{Container, LogLevel, log},
    domain::FunctionType,
    treewalk::{
        Scope, TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal,
        TreewalkValue,
        result::Raise,
        type_system::CloneableCallable,
        types::{Coroutine, Exception, Function, Generator, iterators::GeneratorIter},
        utils::{BoundArgs, InvokeArgs, bind_args},
        value::RuntimeCallable,
    },
};

impl TreewalkInterpreter {
    pub fn bind_callable(
        &self,
        callable: Box<dyn CloneableCallable>,
        args: InvokeArgs,
    ) -> TreewalkResult<BoundArgs> {
        bind_args(callable.receiver(), args, &callable.signature())
            .map_err(|e| e.into_exception(&callable.name()))
            .raise(self)
    }

    pub fn call(
        &self,
        callable: Box<dyn CloneableCallable>,
        args: InvokeArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let bound_args = self.bind_callable(callable.clone(), args)?;
        self.dispatch_callable(callable, bound_args)
    }

    pub fn call_method<S>(
        &self,
        receiver: &TreewalkValue,
        name: S,
        args: InvokeArgs,
    ) -> TreewalkResult<TreewalkValue>
    where
        S: AsRef<str>,
    {
        log(LogLevel::Debug, || {
            format!("Calling method {:?}.{}", receiver, name.as_ref())
        });
        log(LogLevel::Trace, || {
            format!("... from module: {:?}", self.state.current_module())
        });
        log(LogLevel::Trace, || {
            format!(
                "... from path: {}",
                self.state.current_module().borrow().path().display()
            )
        });
        if let Some(class) = self.state.current_class() {
            log(LogLevel::Trace, || format!("... from class: {class:?}"));
        }

        let method = self.load_method(receiver, name)?;
        self.call(method, args)
    }

    pub fn expect_function(
        &self,
        callable: RuntimeCallable,
    ) -> TreewalkResult<Container<Function>> {
        (callable.as_ref() as &dyn Any)
            .downcast_ref::<Container<Function>>()
            .cloned()
            .ok_or_else(|| Exception::type_error("Expected a function"))
            .raise(self)
    }

    fn dispatch_callable(
        &self,
        callable: Box<dyn CloneableCallable>,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        match callable.function_type() {
            FunctionType::Generator => {
                // TODO we may want to support builtin generators in the future. For now, we only
                // support user-defined so we are safe to downcast to `Container<Function>`.
                let function = self.expect_function(callable)?;
                let scope = Container::new(Scope::new(args.into_symbol_table()));
                let generator_function = Generator::new(scope, function);
                let generator_iterator = GeneratorIter::new(generator_function, self.clone());
                Ok(TreewalkValue::Generator(generator_iterator))
            }
            FunctionType::Async => {
                let function = self.expect_function(callable)?;
                let scope = Container::new(Scope::new(args.into_symbol_table()));
                let coroutine = Coroutine::new(scope, function);
                Ok(TreewalkValue::Coroutine(Container::new(coroutine)))
            }
            FunctionType::Regular => match callable.call(self, args) {
                Err(TreewalkDisruption::Signal(TreewalkSignal::Return(result))) => Ok(result),
                Err(e) => Err(e),
                Ok(result) => Ok(result),
            },
        }
    }
}
