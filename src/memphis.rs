use std::process;

#[cfg(feature = "repl")]
use crate::repl::TerminalRepl;
use crate::{core::memphis_utils, domain::Source, Engine, MemphisContext};

/// The entrypoint to the Memphis executable. Supports script mode or REPL mode.
pub struct Memphis;

impl Memphis {
    pub fn script(filepath: &str, engine: Engine) {
        match engine {
            Engine::Treewalk | Engine::BytecodeVm => {
                let source = Source::from_path(filepath)
                    .map_err(|err| {
                        eprintln!("{err}");
                        process::exit(1);
                    })
                    .unwrap();
                let _ = MemphisContext::script(engine, source.clone())
                    .eval(source.text().clone())
                    .map_err(|err| memphis_utils::exit(err));
            }
        }
    }

    #[cfg(feature = "repl")]
    pub fn repl(engine: Engine) {
        TerminalRepl::new(engine).start();
    }
}
