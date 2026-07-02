use std::{env, process};

use memphis::Engine;

mod cli;
mod io;
mod terminal;

fn main() {
    let args: Vec<String> = env::args().collect();
    let engine = Engine::from_env();

    match args.len() {
        1 => cli::repl(engine),
        2 => cli::script(&args[1], engine),
        _ => {
            eprintln!("Usage: memphis [<filename>]");
            process::exit(1);
        }
    }
}
