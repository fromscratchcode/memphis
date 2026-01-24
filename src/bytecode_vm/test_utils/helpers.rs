use crate::{
    bytecode_vm::{VmContext, VmValue},
    domain::{RaisedMemphisError, Source, Text},
};

fn init(text: &str) -> VmContext {
    VmContext::from_text(Text::new(text.trim()))
}

fn init_path(path: &str) -> VmContext {
    VmContext::from_source(Source::from_path(path).expect("Failed to create Source"))
}

pub fn eval(text: &str) -> VmValue {
    init(text)
        .run_inner()
        .expect("Failed to evaluate test string")
}

pub fn eval_expect_error(text: &str) -> RaisedMemphisError {
    let mut context = init(text);
    match context.run_inner() {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e.normalize(context.vm()),
    };
}

pub fn run(text: &str) -> VmContext {
    let mut context = init(text);
    context.run_inner().expect("VM run failed!");
    context
}

pub fn run_path(path: &str) -> VmContext {
    let mut context = init_path(path);
    context.run_inner().expect("VM run failed!");
    context
}

pub fn run_path_expect_error(path: &str) -> RaisedMemphisError {
    let mut context = init_path(path);
    match context.run_inner() {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e.normalize(context.vm()),
    };
}

pub fn read(context: &VmContext, name: &str) -> VmValue {
    context.read_inner(name).expect("Failed to read variable.")
}

pub fn read_attr(context: &VmContext, name: &str, attr: &str) -> VmValue {
    let object = read(context, name);
    let reference = context
        .vm()
        .resolve_raw_attr(&object, attr)
        .expect("Failed to resolve");
    context.vm().deref(reference)
}
