use crate::{
    core::Container,
    treewalk::{
        SymbolTable, TreewalkValue,
        types::{Dict, Tuple},
    },
};

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterDefault {
    Required,
    Value(TreewalkValue),
    Omitted,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterKind {
    PositionalOnly,
    PositionalOrKeyword,
    KeywordOnly,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub default: ParameterDefault,
    pub kind: ParameterKind,
}

impl Parameter {
    pub fn optional(name: impl Into<String>, default: TreewalkValue) -> Self {
        Parameter {
            name: name.into(),
            default: ParameterDefault::Value(default),
            kind: ParameterKind::PositionalOrKeyword,
        }
    }

    pub fn optional_without_default(name: impl Into<String>) -> Self {
        Parameter {
            name: name.into(),
            default: ParameterDefault::Omitted,
            kind: ParameterKind::PositionalOrKeyword,
        }
    }

    pub fn required(name: impl Into<String>) -> Self {
        Parameter {
            name: name.into(),
            default: ParameterDefault::Required,
            kind: ParameterKind::PositionalOrKeyword,
        }
    }

    pub fn positional_only(mut self) -> Self {
        self.kind = ParameterKind::PositionalOnly;
        self
    }

    pub fn keyword_only(mut self) -> Self {
        self.kind = ParameterKind::KeywordOnly;
        self
    }

    pub fn accepts_positional(&self) -> bool {
        self.kind != ParameterKind::KeywordOnly
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Signature {
    pub args: Vec<Parameter>,
    pub args_var: Option<String>,
    pub kwargs_var: Option<String>,
}

impl Signature {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn new(args: impl IntoIterator<Item = Parameter>) -> Self {
        Signature {
            args: args.into_iter().collect(),
            args_var: None,
            kwargs_var: None,
        }
    }

    pub fn positional_only<I, S>(names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self::new(
            names
                .into_iter()
                .map(|name| Parameter::required(name).positional_only()),
        )
    }

    pub fn with_varargs(mut self, name: impl Into<String>) -> Self {
        self.args_var = Some(name.into());
        self
    }

    pub fn with_varkwargs(mut self, name: impl Into<String>) -> Self {
        self.kwargs_var = Some(name.into());
        self
    }
}

#[derive(Debug)]
pub struct BoundArgs {
    values: SymbolTable,
}

impl BoundArgs {
    pub fn new(values: SymbolTable) -> Self {
        Self { values }
    }

    pub fn into_symbol_table(self) -> SymbolTable {
        self.values
    }

    pub fn get(&self, name: &str) -> &TreewalkValue {
        self.values.get(name).unwrap_or_else(|| {
            panic!("internal error: builtin accessed missing bound argument '{name}'")
        })
    }

    pub fn get_optional(&self, name: &str) -> Option<&TreewalkValue> {
        self.values.get(name)
    }

    pub fn get_varargs(&self, name: &str) -> Tuple {
        self.get(name)
            .as_tuple()
            .expect("internal error: varargs must bind as a tuple")
    }

    pub fn get_varkwargs(&self, name: &str) -> Container<Dict> {
        self.get(name)
            .as_dict()
            .expect("internal error: varkwargs must bind as a dict")
    }
}
