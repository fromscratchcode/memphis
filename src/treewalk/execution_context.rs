use crate::{
    core::Container,
    treewalk::{
        types::{Class, Function},
        RaisedException, TreewalkValue,
    },
};

/// This struct stores data for operations related to function calls and class/instance contexts.
pub struct ExecutionContextManager {
    /// A stack to hold the current [`Class`] being defined (i.e. its lexical scope). We need this
    /// so we can associate a function with its class.
    lexical_class_stack: Vec<Container<Class>>,

    /// A stack to hold the current [`Function`] being evaluated. A method will push something onto
    /// this stack and the receiver stack below.
    current_function_stack: Vec<Container<Function>>,

    /// A stack to hold the current [`TreewalkValue`] being evaluated on. We need this for whenver
    /// `super()` is called.
    ///
    /// We do not need a container here because the [`Object`] and [`Class`] variants of
    /// [`TreewalkValue`] already are wrapped in a [`Container`].
    current_receiver_stack: Vec<TreewalkValue>,

    /// A stack of pending `yield from` completion values for the currently running generators.
    /// The top value is consumed by `evaluate_yield_from()` when a delegated generator finishes.
    current_yield_from_result_stack: Vec<Option<TreewalkValue>>,

    current_exception: Option<RaisedException>,
}

impl ExecutionContextManager {
    pub fn new() -> Self {
        Self {
            lexical_class_stack: vec![],
            current_function_stack: vec![],
            current_receiver_stack: vec![],
            current_yield_from_result_stack: vec![],
            current_exception: None,
        }
    }

    pub fn current_exception(&self) -> Option<RaisedException> {
        self.current_exception.clone()
    }

    pub fn set_current_exception(&mut self, exception: RaisedException) {
        self.current_exception = Some(exception);
    }

    pub fn clear_current_exception(&mut self) {
        self.current_exception = None;
    }

    pub fn push_class(&mut self, class: Container<Class>) {
        self.lexical_class_stack.push(class);
    }

    pub fn pop_class(&mut self) -> Option<Container<Class>> {
        self.lexical_class_stack.pop()
    }

    pub fn push_function(&mut self, function: Container<Function>) {
        self.current_function_stack.push(function);
    }

    pub fn pop_function(&mut self) -> Option<Container<Function>> {
        self.current_function_stack.pop()
    }

    pub fn push_receiver(&mut self, receiver: TreewalkValue) {
        self.current_receiver_stack.push(receiver);
    }

    pub fn pop_receiver(&mut self) -> Option<TreewalkValue> {
        self.current_receiver_stack.pop()
    }

    pub fn push_yield_from_result_frame(&mut self) {
        self.current_yield_from_result_stack.push(None);
    }

    pub fn pop_yield_from_result_frame(&mut self) -> Option<Option<TreewalkValue>> {
        self.current_yield_from_result_stack.pop()
    }

    pub fn set_current_yield_from_result(&mut self, value: TreewalkValue) {
        if let Some(slot) = self.current_yield_from_result_stack.last_mut() {
            *slot = Some(value);
        }
    }

    pub fn take_current_yield_from_result(&mut self) -> Option<TreewalkValue> {
        self.current_yield_from_result_stack
            .last_mut()
            .and_then(Option::take)
    }

    /// Return the currently executing function.
    pub fn read_current_function(&self) -> Option<Container<Function>> {
        self.current_function_stack.last().cloned()
    }

    /// Return the currently executing receiver.
    pub fn read_current_receiver(&self) -> Option<TreewalkValue> {
        self.current_receiver_stack.last().cloned()
    }

    /// Return the current class according to lexical scoping rules.
    pub fn read_class(&self) -> Option<Container<Class>> {
        self.lexical_class_stack.last().cloned()
    }
}
