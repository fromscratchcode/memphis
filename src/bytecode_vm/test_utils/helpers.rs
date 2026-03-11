use crate::{
    bytecode_vm::VmContext,
    domain::{MemphisValue, RaisedMemphisError, Source, Text},
};

fn init() -> VmContext {
    VmContext::stdin()
}

fn init_path(path: &str) -> (VmContext, Text) {
    let source = Source::from_path(path).expect("Failed to create Source");
    (VmContext::script(source.clone()), source.text().clone())
}

pub fn eval(text: &str) -> MemphisValue {
    let mut context = init();
    let value = context
        .eval_inner(Text::new(text.trim()))
        .expect("Failed to evaluate test string");
    context.vm().normalize_vm_value(value)
}

pub fn eval_expect_error(text: &str) -> RaisedMemphisError {
    let mut context = init();
    match context.eval_inner(Text::new(text.trim())) {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e.normalize(context.vm()),
    };
}

pub fn run(text: &str) -> VmContext {
    let mut context = init();
    context
        .eval_inner(Text::new(text.trim()))
        .expect("VM run failed!");
    context
}

pub fn run_script(path: &str) -> String {
    let (mut context, text) = init_path(path);
    context.enable_capture();
    context.eval_inner(text).expect("VM run failed!");
    context.take_output().expect("Output not captured")
}

pub fn run_path_expect_error(path: &str) -> RaisedMemphisError {
    let (mut context, text) = init_path(path);
    match context.eval_inner(text) {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e.normalize(context.vm()),
    };
}
