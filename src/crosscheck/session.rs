use crate::{
    domain::{MemphisResult, MemphisValue, RaisedMemphisError, Text},
    Engine, MemphisContext,
};

pub struct CrosscheckSession {
    treewalk: MemphisContext,
    vm: MemphisContext,
}

impl CrosscheckSession {
    /// Create a new session from a `Source`.
    pub fn new(text: Text) -> Self {
        let treewalk = MemphisContext::from_text(Engine::Treewalk, text.clone());
        let vm = MemphisContext::from_text(Engine::BytecodeVm, text);
        Self { treewalk, vm }
    }

    /// Run both engines; discard the return value and return the session. Useful for later reads.
    pub fn run(mut self) -> MemphisResult<Self> {
        self.treewalk.run()?;
        self.vm.run()?;
        Ok(self)
    }

    /// Read a value from both engines; confirm they return the same value, then return the value.
    pub fn read(&mut self, name: &str) -> (MemphisValue, MemphisValue) {
        self.treewalk.add_text(Text::new(name));
        self.vm.add_text(Text::new(name));
        self.eval()
    }

    /// Run both engines, then return the value. We do not test for equality here, but rather leave
    /// that up to any downstream macros, so the source of the error will be clearer to the user.
    pub fn eval(&mut self) -> (MemphisValue, MemphisValue) {
        let tw_val = self.treewalk.run().expect("Treewalk run failed.");
        let vm_val = self.vm.run().expect("VM run failed.");

        (tw_val, vm_val)
    }

    /// Run both engines, then return the error. We do not test for equality here, but rather leave
    /// that up to any downstream macros, so the source of the error will be clearer to the user.
    pub fn run_expect_error(mut self) -> (RaisedMemphisError, RaisedMemphisError) {
        let treewalk_err = self
            .treewalk
            .run()
            .expect_err("Expected error from treewalk engine");
        let vm_err = self.vm.run().expect_err("Expected error from bytecode VM");

        (treewalk_err, vm_err)
    }
}
