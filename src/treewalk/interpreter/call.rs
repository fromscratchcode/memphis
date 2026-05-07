use std::any::Any;

use crate::{
    core::{log, Container, LogLevel},
    domain::FunctionType,
    parser::types::Callee,
    treewalk::{
        result::Raise,
        type_system::CloneableCallable,
        types::{iterators::GeneratorIter, Coroutine, Exception, Function, Generator},
        utils::Args,
        value::RuntimeCallable,
        TreewalkDisruption, TreewalkInterpreter, TreewalkResult, TreewalkSignal, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn call(
        &self,
        callable: Box<dyn CloneableCallable>,
        args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        let args = args.with_bound_receiver(callable.receiver());
        self.dispatch_callable(callable, args)
    }

    pub fn call_method<S>(
        &self,
        receiver: &TreewalkValue,
        name: S,
        args: Args,
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

    pub fn evaluate_callable(&self, callee: &Callee) -> TreewalkResult<RuntimeCallable> {
        match callee {
            Callee::Expr(callee) => self.evaluate_expr(callee)?.as_callable().raise(self),
            Callee::Symbol(name) => self.load_callable(name.as_str()),
        }
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
        args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        match callable.function_type() {
            FunctionType::Generator => {
                // TODO we may want to support builtin generators in the future. For now, we only
                // support user-defined so we are safe to downcast to `Container<Function>`.
                let function = self.expect_function(callable)?;
                let scope = function.borrow().create_scope(&args).raise(self)?;
                let generator_function = Generator::new(scope, function);
                let generator_iterator = GeneratorIter::new(generator_function, self.clone());
                Ok(TreewalkValue::Generator(generator_iterator))
            }
            FunctionType::Async => {
                let function = self.expect_function(callable)?;
                let scope = function.borrow().create_scope(&args).raise(self)?;
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
