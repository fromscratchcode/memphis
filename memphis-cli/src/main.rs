use clap::Parser;
use std::path::PathBuf;

use memphis::Engine;

mod repl;
mod script;
mod system_io;

#[derive(Parser)]
struct MemphisCli {
    #[arg(long)]
    engine: Option<Engine>,

    #[arg(value_name = "SCRIPT")]
    script: Option<PathBuf>,
}

fn main() {
    let cli = MemphisCli::parse();
    let engine = cli.engine.unwrap_or(Engine::default());

    match cli.script {
        None => repl::run(engine),
        Some(path) => script::run(path, engine),
    }
}
