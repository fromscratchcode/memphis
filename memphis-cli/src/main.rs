use clap::Parser;
use std::path::PathBuf;

use memphis::Engine;

#[derive(Parser)]
struct Cli {
    #[arg(long)]
    engine: Option<Engine>,

    #[arg(value_name = "SCRIPT")]
    script: Option<PathBuf>,
}

mod cli;
mod io;
mod terminal;

fn main() {
    let cli = Cli::parse();
    let engine = cli.engine.unwrap_or(Engine::default());

    match cli.script {
        None => cli::repl(engine),
        Some(path) => cli::script(path, engine),
    }
}
