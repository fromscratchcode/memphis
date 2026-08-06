use std::collections::HashMap;

use crate::{
    parser::types::{AstInvokeArgs, AstParams, KwargsOperation},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        iterator::for_each_mut,
        protocols::TryEvalFrom,
        result::Raise,
        types::{Exception, Tuple},
        utils::{InvokeArgs, Parameter, ParameterDefault, ParameterKind, Signature},
    },
};

impl TreewalkInterpreter {
    /// Evaluate the arguments a function is called with.
    pub fn evaluate_args(&self, args: &AstInvokeArgs) -> TreewalkResult<InvokeArgs> {
        let mut positional = args
            .positional
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<TreewalkResult<Vec<_>>>()?;

        if let Some(ref args_var) = args.args_var {
            let value = self.evaluate_expr(args_var)?;
            let args = Tuple::try_eval_from(value, self)?;
            // Clone each item in place without an intermediate Vec
            positional.extend_from_slice(args.items());
        };

        let mut kwargs = HashMap::default();
        for kwarg in args.kwargs.iter() {
            match kwarg {
                KwargsOperation::Pair(key, value) => {
                    let value = self.evaluate_expr(value)?;
                    insert_kwarg(&mut kwargs, key.as_str(), value).raise(self)?;
                }
                KwargsOperation::Unpacking(expr) => {
                    let unpacked = self.evaluate_expr(expr)?;
                    let iter = unpacked.clone().as_iterator().raise(self)?;
                    for_each_mut(iter, &mut |key_val| {
                        let key = key_val.as_string().raise(self)?;
                        let value = self.load_index(&unpacked, &key_val)?;
                        insert_kwarg(&mut kwargs, &key, value).raise(self)?;
                        Ok(())
                    })?;
                }
            }
        }

        Ok(InvokeArgs::new(positional, kwargs))
    }

    /// Evaluate the parameters a function is defined with, specifically any default values.
    pub fn evaluate_params(&self, params: &AstParams) -> TreewalkResult<Signature> {
        let runtime_params = params
            .positional
            .iter()
            .map(|param| {
                // User code cannot generate ParameterDefault::Omitted, only builtins
                let default = match &param.default {
                    Some(expr) => ParameterDefault::Value(self.evaluate_expr(expr)?),
                    None => ParameterDefault::Required,
                };
                Ok(Parameter {
                    name: param.arg.to_string(),
                    default,
                    kind: ParameterKind::PositionalOrKeyword,
                })
            })
            .collect::<TreewalkResult<Vec<_>>>()?;

        Ok(Signature {
            args: runtime_params,
            args_var: params.args_var.as_ref().map(|c| c.to_string()).clone(),
            kwargs_var: params.kwargs_var.as_ref().map(|c| c.to_string()).clone(),
        })
    }
}

fn insert_kwarg(
    kwargs: &mut HashMap<String, TreewalkValue>,
    key: &str,
    value: TreewalkValue,
) -> DomainResult<()> {
    if kwargs.contains_key(key) {
        Err(Exception::key_error_str(key))
    } else {
        kwargs.insert(key.to_string(), value);
        Ok(())
    }
}
