use crate::{
    ModuleOrigin,
    bytecode_vm::VmContext,
    core::Container,
    domain::{MemphisValue, RaisedMemphisError, Source, Text},
    test_utils::{Capture, TestIo, resolve_workspace_path},
};

pub fn init() -> VmContext {
    let (io, _) = TestIo::new();
    VmContext::init(ModuleOrigin::Stdin, io)
}

fn init_path(path: &str) -> (VmContext, Container<Capture>, Text) {
    let (io, capture) = TestIo::new();
    let source = Source::from_path(resolve_workspace_path(path)).expect("Failed to create Source");
    (
        VmContext::init(ModuleOrigin::File(source.path().clone()), io),
        capture,
        source.text().clone(),
    )
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
        Err(e) => e.normalize(context.vm()),
    }
}

pub fn run(text: &str) -> VmContext {
    let mut context = init();
    context
        .eval_inner(Text::new(text.trim()))
        .expect("VM run failed!");
    context
}

pub fn run_script(path: &str) -> String {
    let (mut ctx, capture, text) = init_path(path);
    ctx.eval_inner(text).expect("VM run failed!");
    capture.borrow_mut().take_output()
}

pub fn run_path_expect_error(path: &str) -> RaisedMemphisError {
    let (mut ctx, _, text) = init_path(path);
    match ctx.eval_inner(text) {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => e.normalize(ctx.vm()),
    }
}
