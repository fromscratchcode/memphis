use memphis::{Engine, MemphisContext, ModuleOrigin, Source};
use std::{path::Path, process};

use crate::system_io::SystemIo;

pub fn run(filepath: impl AsRef<Path>, engine: Engine) {
    let source = Source::from_path(filepath)
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        })
        .unwrap();
    let origin = ModuleOrigin::File(source.path().clone());
    let _ = MemphisContext::new(engine, origin, SystemIo)
        .eval(source.text().clone())
        .map_err(|err| {
            eprintln!("{err}");
            process::exit(1);
        });
}
