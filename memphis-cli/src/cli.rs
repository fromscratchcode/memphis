use memphis::{Engine, MemphisContext, Source};
use std::process;

use crate::terminal::TerminalRepl;

pub fn script(filepath: &str, engine: Engine) {
    let source = Source::from_path(filepath)
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        })
        .unwrap();
    let _ = MemphisContext::script(engine, source.clone())
        .eval(source.text().clone())
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        });
}

pub fn repl(engine: Engine) {
    TerminalRepl::new(engine).start();
}
