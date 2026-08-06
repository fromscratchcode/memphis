use crate::{
    core::Container,
    treewalk::{
        SymbolTable, TreewalkValue,
        types::{Dict, Exception, Tuple},
        utils::{BindingInput, BoundArgs, InvokeArgs, ParameterDefault, Signature},
    },
};

#[derive(Debug)]
pub enum ArgBindError {
    TooManyPositional { given: usize, expected: usize },
    MissingRequired { names: Vec<String> },
    UnexpectedKeyword { name: String },
}

impl ArgBindError {
    pub fn into_exception(self, callee_name: &str) -> Exception {
        match self {
            Self::TooManyPositional { given, expected } => {
                Exception::type_error(format!("Expected {}, found {} args", expected, given))
            }
            Self::MissingRequired { names } => {
                let num_missing = names.len();
                let noun = if num_missing == 1 {
                    "argument"
                } else {
                    "arguments"
                };
                let arg_names = names
                    .iter()
                    .map(|a| format!("'{a}'"))
                    .collect::<Vec<_>>()
                    .join(" and ");
                Exception::type_error(format!(
                    "{callee_name}() missing {num_missing} required positional {noun}: {arg_names}"
                ))
            }
            Self::UnexpectedKeyword { name } => Exception::type_error(format!(
                "{callee_name}() got an unexpected keyword argument '{name}'"
            )),
        }
    }
}

/// Insert the receiver if necessary, then bind the args.
pub fn bind_args(
    receiver: Option<TreewalkValue>,
    args: InvokeArgs,
    signature: &Signature,
) -> Result<BoundArgs, ArgBindError> {
    let bound_args = args.into_binding_input(receiver);
    bind_input(bound_args, signature)
}

/// Implements Python's binding order:
/// - positional
/// - keyword (overrides or fills)
/// - defaults for missing
/// - leftover get stored in **kwargs
fn bind_input(
    binding_input: BindingInput,
    signature: &Signature,
) -> Result<BoundArgs, ArgBindError> {
    let expected_positional_count = signature
        .args
        .iter()
        .filter(|param| param.accepts_positional())
        .count();

    // Function expects fewer positional args than it was invoked with and there is not an
    // `args_var` in which to store the rest.
    if expected_positional_count < binding_input.num_positional() && signature.args_var.is_none() {
        return Err(ArgBindError::TooManyPositional {
            given: binding_input.num_positional(),
            expected: expected_positional_count,
        });
    }

    let mut table = SymbolTable::default();
    let mut missing_args = vec![];

    for (index, arg_def) in signature.args.iter().enumerate() {
        // Check if already satisfied by a keyword argument
        if binding_input.has_kwarg(&arg_def.name) {
            // We'll bind it later in the keyword override pass
            continue;
        }

        // Check if the argument is provided, otherwise use default
        let value = if index < binding_input.num_positional() && arg_def.accepts_positional() {
            binding_input.get_positional(index).clone()
        } else {
            match &arg_def.default {
                ParameterDefault::Omitted => continue,
                ParameterDefault::Value(default_value) => default_value.clone(),
                ParameterDefault::Required => {
                    missing_args.push(arg_def.name.clone());
                    // We use None here only because if we hit this case, we will return an
                    // error shortly after this loop. We can't do it here because we need to
                    // find all the missing args first.
                    TreewalkValue::None
                }
            }
        };

        table.insert(&arg_def.name, value);
    }

    for (key, value) in binding_input.kwargs().iter() {
        // Is this keyword name something that belongs to the function, either because it was
        // already bound, or because it’s one of the declared parameters?
        if table.has(key) || signature.args.iter().any(|a| &a.name == key) {
            table.insert(key, value.clone());
        } else if signature.kwargs_var.is_none() {
            return Err(ArgBindError::UnexpectedKeyword {
                name: key.to_string(),
            });
        }
    }

    // Function expects more positional args than it was invoked with.
    if !missing_args.is_empty() {
        return Err(ArgBindError::MissingRequired {
            names: missing_args,
        });
    }

    if let Some(ref args_var) = signature.args_var {
        let left_over = binding_input
            .args()
            .iter()
            .skip(expected_positional_count)
            .cloned()
            .collect();
        let args_value = TreewalkValue::Tuple(Tuple::new(left_over));
        table.insert(args_var, args_value);
    }

    if let Some(ref kwargs_var) = signature.kwargs_var {
        let symbol_table = SymbolTable::new(binding_input.kwargs().clone());
        let kwargs = Dict::from_symbol_table(&symbol_table);
        let kwargs_value = TreewalkValue::Dict(Container::new(kwargs));
        table.insert(kwargs_var, kwargs_value);
    }

    Ok(BoundArgs::new(table))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{ArgBindError, bind_args};
    use crate::treewalk::{
        TreewalkValue,
        utils::{InvokeArgs, Parameter, Signature},
    };

    #[test]
    fn keyword_only_parameter_cannot_be_filled_positionally() {
        let signature = Signature::new([
            Parameter::required("first").positional_only(),
            Parameter::required("strict").keyword_only(),
        ])
        .with_varargs("rest");

        let args = InvokeArgs::new(
            vec![TreewalkValue::Int(1), TreewalkValue::Int(2)],
            HashMap::new(),
        );

        let result = bind_args(None, args, &signature);

        assert!(matches!(
            result,
            Err(ArgBindError::MissingRequired { names })
                if names == vec!["strict".to_string()]
        ));
    }

    #[test]
    fn keyword_only_default_is_not_filled_from_varargs() {
        let signature = Signature::new([
            Parameter::required("first").positional_only(),
            Parameter::optional("strict", TreewalkValue::Bool(false)).keyword_only(),
        ])
        .with_varargs("rest");

        let args = InvokeArgs::new(
            vec![TreewalkValue::Int(1), TreewalkValue::Int(2)],
            HashMap::new(),
        );

        let bound = bind_args(None, args, &signature).unwrap();

        assert_eq!(bound.get("first"), &TreewalkValue::Int(1));
        assert_eq!(bound.get("strict"), &TreewalkValue::Bool(false));
        assert_eq!(bound.get_varargs("rest").items(), &[TreewalkValue::Int(2)],);
    }
}
