use std::fmt::{Display, Formatter};

#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub enum Engine {
    #[default]
    Treewalk,
    BytecodeVm,
}

impl std::str::FromStr for Engine {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "treewalk" => Ok(Engine::Treewalk),
            "bytecode_vm" => Ok(Engine::BytecodeVm),
            _ => Err(format!("Unknown engine: {}", s)),
        }
    }
}

impl Display for Engine {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Engine::Treewalk => write!(f, "treewalk"),
            Engine::BytecodeVm => write!(f, "bytecode_vm"),
        }
    }
}
