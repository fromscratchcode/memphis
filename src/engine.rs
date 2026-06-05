use std::{
    env,
    fmt::{Display, Formatter, Result},
};

#[derive(PartialEq, Clone, Copy)]
pub enum Engine {
    Treewalk,
    BytecodeVm,
    #[cfg(feature = "llvm_backend")]
    LlvmBackend,
}

impl Engine {
    /// I could see the default becoming [`Engine::BytecodeVm`] in the future once it supports more.
    pub const DEFAULT_ENGINE: Engine = Engine::Treewalk;

    pub fn from_env() -> Self {
        if let Ok(mode) = env::var("MEMPHIS_ENGINE") {
            match Engine::try_from(mode.to_lowercase().as_str()) {
                Ok(e) => e,
                Err(_) => panic!("Unsupported engine: {mode}"),
            }
        } else {
            Self::DEFAULT_ENGINE
        }
    }
}

impl TryFrom<&str> for Engine {
    type Error = ();

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        match value {
            "bytecode_vm" => Ok(Engine::BytecodeVm),
            #[cfg(feature = "llvm_backend")]
            "llvm_backend" => Ok(Engine::LlvmBackend),
            "treewalk" => Ok(Engine::Treewalk),
            _ => Err(()),
        }
    }
}

impl Display for Engine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Engine::Treewalk => write!(f, "treewalk"),
            Engine::BytecodeVm => write!(f, "bytecode_vm"),
            #[cfg(feature = "llvm_backend")]
            Engine::LlvmBackend => write!(f, "llvm_backend"),
        }
    }
}
