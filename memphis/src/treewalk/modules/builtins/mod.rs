use crate::{
    core::Container,
    domain::{Dunder, MemphisValue, ModuleName},
    runtime::{HostIoError, InputResult},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue, TypeRegistry,
        iterator::{collect, count},
        protocols::{Callable, Iterable, NextResult},
        result::Raise,
        type_system::CloneableCallable,
        types::{Exception, List, Module, Str},
        utils::{BoundArgs, Parameter, Signature, args},
    },
};

fn builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![
        Box::new(CallableBuiltin),
        Box::new(DirBuiltin),
        Box::new(GetattrBuiltin),
        Box::new(SetattrBuiltin),
        Box::new(GlobalsBuiltin),
        Box::new(HashBuiltin),
        Box::new(IsinstanceBuiltin),
        Box::new(IssubclassBuiltin),
        Box::new(IterBuiltin),
        Box::new(SortedBuiltin),
        Box::new(LenBuiltin),
        Box::new(NextBuiltin),
        Box::new(PrintBuiltin),
        Box::new(InputBuiltin),
    ]
}

pub fn init(registry: &TypeRegistry) -> Module {
    let mut mod_ = Module::new_builtin(ModuleName::from_segments(&[Dunder::Builtins]));
    for builtin in builtins() {
        mod_.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
    }

    // This is to insert `list()`, `set()`, etc into the builtin scope. We must do it here instead
    // of in `init_builtin_scope()` because we want to use the singleton instances owned by
    // `TypeRegistry`.
    for builtin_class in registry.builtin_exported_classes() {
        mod_.insert(
            builtin_class.borrow().name(),
            TreewalkValue::Class(builtin_class.clone()),
        );
    }

    mod_
}

#[derive(Clone)]
pub struct CallableBuiltin;
#[derive(Clone)]
pub struct DirBuiltin;
#[derive(Clone)]
pub struct GetattrBuiltin;
#[derive(Clone)]
pub struct SetattrBuiltin;
#[derive(Clone)]
pub struct GlobalsBuiltin;
#[derive(Clone)]
pub struct HashBuiltin;
#[derive(Clone)]
pub struct IsinstanceBuiltin;
#[derive(Clone)]
pub struct IssubclassBuiltin;
#[derive(Clone)]
pub struct IterBuiltin;
#[derive(Clone)]
pub struct SortedBuiltin;
#[derive(Clone)]
pub struct LenBuiltin;
#[derive(Clone)]
pub struct NextBuiltin;
#[derive(Clone)]
pub struct PrintBuiltin;
#[derive(Clone)]
pub struct InputBuiltin;

impl Callable for CallableBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj"])
    }

    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Bool(
            args.get("obj").clone().as_callable().is_ok(),
        ))
    }

    fn name(&self) -> String {
        "callable".into()
    }
}

impl Callable for DirBuiltin {
    fn signature(&self) -> Signature {
        // TODO we don't yet handle the dir() version, which just returns the names in the current
        // local scope
        Signature::positional_only(["obj"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let dir = args
            .get("obj")
            .clone()
            .into_member_reader(interpreter)
            .dir()
            .iter()
            .map(|i| TreewalkValue::Str(Str::new(i)))
            .collect::<Vec<_>>();
        Ok(TreewalkValue::List(Container::new(List::new(dir))))
    }

    fn name(&self) -> String {
        "dir".into()
    }
}

impl Callable for GetattrBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("obj").positional_only(),
            Parameter::required("field").positional_only(),
            Parameter::optional_without_default("default").positional_only(),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("obj");
        let field = args.get("field").as_string().raise(interpreter)?;

        let attr = object
            .clone()
            .into_member_reader(interpreter)
            .get_member(interpreter, field.as_str())?;

        match (attr, args.get_optional("default")) {
            (Some(attr), _) => Ok(attr),
            (None, Some(default)) => Ok(default.clone()),
            (None, None) => Exception::attribute_error(interpreter.state.class_name(object), field)
                .raise(interpreter),
        }
    }

    fn name(&self) -> String {
        "getattr".into()
    }
}

impl Callable for SetattrBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj", "field", "value"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let object = args.get("obj");
        let field = args.get("field").as_string().raise(interpreter)?;
        let value = args.get("value");

        object
            .clone()
            .into_member_writer()
            .ok_or_else(|| {
                Exception::attribute_error(interpreter.state.class_name(object), field.as_str())
            })
            .raise(interpreter)?
            .set_member(interpreter, field.as_str(), value.clone())?;

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "setattr".into()
    }
}

impl Callable for GlobalsBuiltin {
    fn signature(&self) -> Signature {
        Signature::empty()
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        _args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Dict(Container::new(
            interpreter.state.read_globals(),
        )))
    }

    fn name(&self) -> String {
        "globals".into()
    }
}

impl Callable for HashBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let arg = args.get("obj");
        if arg.as_class().is_ok() {
            return Ok(TreewalkValue::Int(arg.hash() as i64));
        }

        let result = interpreter.call_method(arg, Dunder::Hash, args![])?;

        if let TreewalkValue::Int(_) = result {
            Ok(result)
        } else {
            Exception::type_error(format!("{} method should return an integer", Dunder::Hash))
                .raise(interpreter)
        }
    }

    fn name(&self) -> String {
        "hash".into()
    }
}

impl Callable for IsinstanceBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj", "classinfo"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let obj = args.get("obj");
        let message = "isinstance() arg 2 must be a type, a tuple of types, or a union";

        let reference_class = match args.get("classinfo").clone() {
            TreewalkValue::Class(class) => vec![class],
            TreewalkValue::Tuple(tuple) => tuple
                .into_iter()
                .map(|item| item.as_class())
                .collect::<DomainResult<Vec<_>>>()
                .map_err(|_| Exception::type_error(message))
                .raise(interpreter)?,
            _ => return Exception::type_error(message).raise(interpreter),
        };

        let isinstance = reference_class
            .iter()
            .any(|class| interpreter.is_instance_of(obj, class));

        Ok(TreewalkValue::Bool(isinstance))
    }

    fn name(&self) -> String {
        "isinstance".into()
    }
}

impl Callable for IssubclassBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["cls", "classinfo"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let candidate_class = args
            .get("cls")
            .as_class()
            .map_err(|_| Exception::type_error("issubclass() arg 1 must be a class"))
            .raise(interpreter)?;

        let message = "issubclass() arg 2 must be a type, a tuple of types, or a union";

        let reference_class = match args.get("classinfo").clone() {
            TreewalkValue::Class(class) => vec![class],
            TreewalkValue::Tuple(tuple) => tuple
                .into_iter()
                .map(|item| item.as_class())
                .collect::<DomainResult<Vec<_>>>()
                .map_err(|_| Exception::type_error(message))
                .raise(interpreter)?,
            _ => return Exception::type_error(message).raise(interpreter),
        };

        let issubclass = reference_class
            .iter()
            .any(|class| candidate_class.is_subclass_of(class));

        Ok(TreewalkValue::Bool(issubclass))
    }

    fn name(&self) -> String {
        "issubclass".into()
    }
}

impl Callable for PrintBuiltin {
    fn signature(&self) -> Signature {
        Signature::empty().with_varargs("args")
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let value = args
            .get_varargs("args")
            .items()
            .iter()
            .map(|value| MemphisValue::from(value.clone()).to_string())
            .collect::<Vec<_>>()
            .join(" ");
        let result = interpreter.memphis_state.borrow_mut().io.writeln(&value);
        if let Err(HostIoError { message }) = result {
            return Exception::io_error(message).raise(interpreter);
        }
        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "print".into()
    }
}

impl Callable for InputBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([Parameter::optional_without_default("prompt").positional_only()])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let prompt = args
            .get_optional("prompt")
            .map(|value| MemphisValue::from(value.clone()).to_string())
            .unwrap_or_default();

        let input = interpreter.memphis_state.borrow_mut().io.input(&prompt);
        match input {
            Ok(InputResult::Line(input)) => Ok(TreewalkValue::Str(Str::new(&input))),
            Ok(InputResult::Eof) => {
                Exception::eof_error("EOF when reading a line").raise(interpreter)
            }
            Err(HostIoError { message }) => Exception::io_error(message).raise(interpreter),
        }
    }

    fn name(&self) -> String {
        "input".into()
    }
}

impl Callable for LenBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let iter = args.get("obj").as_iterator().raise(interpreter)?;
        let count = count(iter)?;
        Ok(TreewalkValue::Int(count as i64))
    }

    fn name(&self) -> String {
        "len".into()
    }
}

impl Callable for NextBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let mut iterator = args
            .get("obj")
            .clone()
            .as_iterator_strict()
            .raise(interpreter)?;
        match iterator.try_next()? {
            NextResult::Yielded(val) => Ok(val),
            NextResult::Exhausted(None) => Exception::stop_iteration().raise(interpreter),
            NextResult::Exhausted(Some(val)) => {
                Exception::stop_iteration_with(val).raise(interpreter)
            }
        }
    }

    fn name(&self) -> String {
        "next".into()
    }
}

impl Callable for IterBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["obj"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        args.get("obj").clone().as_iterable().raise(interpreter)
    }

    fn name(&self) -> String {
        "iter".into()
    }
}

impl Callable for SortedBuiltin {
    fn signature(&self) -> Signature {
        Signature::positional_only(["iterable"])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let iter = args.get("iterable").as_iterator().raise(interpreter)?;
        let mut items = collect(iter)?;
        interpreter.python_sort(&mut items)?;
        Ok(TreewalkValue::List(Container::new(List::new(items))))
    }

    fn name(&self) -> String {
        "sorted".into()
    }
}
