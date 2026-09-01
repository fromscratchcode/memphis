use crate::{
    ModuleOrigin,
    core::Container,
    domain::{Source, Text},
    test_utils::{Capture, TestIo, resolve_workspace_path},
    treewalk::{RaisedException, TreewalkContext, TreewalkValue},
};

fn init() -> TreewalkContext {
    let (io, _) = TestIo::new();
    TreewalkContext::init(ModuleOrigin::Stdin, io)
}

fn init_path(path: &str) -> (TreewalkContext, Container<Capture>, Text) {
    let (io, capture) = TestIo::new();
    let source = Source::from_path(resolve_workspace_path(path)).expect("Failed to create Source");
    (
        TreewalkContext::init(ModuleOrigin::File(source.path().clone()), io),
        capture,
        source.text().clone(),
    )
}

pub fn eval(text: &str) -> TreewalkValue {
    init()
        .eval_inner(Text::new(text))
        .expect("Failed to evaluate test string!")
}

pub fn eval_expect_error(text: &str) -> RaisedException {
    match init().eval_inner(Text::new(text)) {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => e,
    }
}

pub fn run(text: &str) -> TreewalkContext {
    let mut ctx = init();
    ctx.eval_inner(Text::new(text))
        .expect("Treewalk evaluation failed");
    ctx
}

pub fn run_script(path: &str) -> String {
    let (mut ctx, capture, text) = init_path(path);
    ctx.eval_inner(text).expect("Treewalk evaluation failed");
    capture.borrow_mut().take_output()
}

pub fn run_path(path: &str) -> TreewalkContext {
    let (mut ctx, _, text) = init_path(path);
    ctx.eval_inner(text).expect("Treewalk evaluation failed");
    ctx
}

pub fn run_path_expect_error(path: &str) -> RaisedException {
    let (mut ctx, _, text) = init_path(path);
    match ctx.eval_inner(text) {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => e,
    }
}

pub fn read_optional(ctx: &TreewalkContext, name: &str) -> Option<TreewalkValue> {
    ctx.read_inner(name)
}

pub fn read(ctx: &TreewalkContext, name: &str) -> TreewalkValue {
    read_optional(ctx, name).unwrap_or_else(|| panic!("Failed to read var: {}", name))
}
