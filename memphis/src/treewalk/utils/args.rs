use std::collections::HashMap;

use crate::treewalk::TreewalkValue;

/// Represents the evaluated positional and keyword arguments supplied to a call, pre-binding.
#[derive(Default, Debug, Clone)]
pub struct InvokeArgs {
    args: Vec<TreewalkValue>,
    kwargs: HashMap<String, TreewalkValue>,
}

impl InvokeArgs {
    pub fn new(args: Vec<TreewalkValue>, kwargs: HashMap<String, TreewalkValue>) -> Self {
        Self { args, kwargs }
    }

    /// Prepends `cls` for the unbound `__new__` call during object creation
    pub fn with_bound_new(mut self, val: TreewalkValue) -> Self {
        self.args.insert(0, val);
        self
    }

    pub fn into_binding_input(self, receiver: Option<TreewalkValue>) -> BindingInput {
        let mut args = self.args;
        if let Some(receiver) = receiver {
            args.insert(0, receiver);
        };
        BindingInput::new(args, self.kwargs)
    }
}

/// Represents the fully resolved parameter state for positional and keyword arguments, plus any
/// inserted receivers (post-descriptor protocol). This is what args are actually bound against.
pub struct BindingInput {
    args: Vec<TreewalkValue>,
    kwargs: HashMap<String, TreewalkValue>,
}

impl BindingInput {
    pub fn new(args: Vec<TreewalkValue>, kwargs: HashMap<String, TreewalkValue>) -> Self {
        Self { args, kwargs }
    }

    pub fn args(&self) -> &[TreewalkValue] {
        &self.args
    }

    pub fn num_positional(&self) -> usize {
        self.args.len()
    }

    pub fn get_positional(&self, index: usize) -> &TreewalkValue {
        &self.args[index]
    }

    pub fn kwargs(&self) -> &HashMap<String, TreewalkValue> {
        &self.kwargs
    }

    pub fn has_kwarg(&self, key: &str) -> bool {
        self.kwargs.contains_key(key)
    }
}

/// This macro is useful to create `ResolvedArguments` when you only need positional arguments.
/// When kwargs are needed, you can use `ResolvedArguments::new`.
macro_rules! args {
    () => {{
        $crate::treewalk::utils::InvokeArgs::default()
    }};
    // Double curly braces ensure that the entire macro expands into a single expression, which is
    // necessary since we are returning a value from this macro.
    ( $( $arg:expr ),* ) => {{
        $crate::treewalk::utils::InvokeArgs::new(
            vec![$($arg),*],
            std::collections::HashMap::new(),
        )
    }};
}

pub(crate) use args;
