use crate::{
    domain::{Source, Text},
    treewalk::{RaisedException, TreewalkContext, TreewalkValue},
};

fn init() -> TreewalkContext {
    TreewalkContext::stdin()
}

fn init_path(path: &str) -> (TreewalkContext, Text) {
    let source = Source::from_path(path).expect("Failed to create Source");
    (
        TreewalkContext::script(source.clone()),
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
        Err(e) => return e,
    };
}

pub fn run(text: &str) -> TreewalkContext {
    let mut context = init();
    context
        .eval_inner(Text::new(text))
        .expect("Treewalk evaluation failed");
    context
}

pub fn run_path(path: &str) -> TreewalkContext {
    let (mut context, text) = init_path(path);
    context
        .eval_inner(text)
        .expect("Treewalk evaluation failed");
    context
}

pub fn run_path_expect_error(path: &str) -> RaisedException {
    let (mut context, text) = init_path(path);
    match context.eval_inner(text) {
        Ok(_) => panic!("Expected an error!"),
        Err(e) => return e,
    };
}

pub fn read_optional(ctx: &TreewalkContext, name: &str) -> Option<TreewalkValue> {
    ctx.read_inner(name)
}

pub fn read(ctx: &TreewalkContext, name: &str) -> TreewalkValue {
    read_optional(&ctx, name).expect(&format!("Failed to read var: {}", name))
}
