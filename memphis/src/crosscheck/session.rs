use crate::{
    Engine, MemphisContext, ModuleOrigin,
    domain::{MemphisValue, RaisedMemphisError, Text},
    test_utils::TestIo,
};

pub struct CrosscheckSession {
    treewalk: MemphisContext,
    vm: MemphisContext,
}

impl CrosscheckSession {
    /// Create a new session from a `Source`.
    pub fn new() -> Self {
        let (io, _) = TestIo::new();
        let treewalk = MemphisContext::new(Engine::Treewalk, ModuleOrigin::Stdin, io);
        let (io, _) = TestIo::new();
        let vm = MemphisContext::new(Engine::BytecodeVm, ModuleOrigin::Stdin, io);
        Self { treewalk, vm }
    }

    /// Run both engines, then return the value. We do not test for equality here, but rather leave
    /// that up to any downstream macros, so the source of the error will be clearer to the user.
    pub fn eval(&mut self, text: Text) -> (MemphisValue, MemphisValue) {
        let tw_val = self
            .treewalk
            .eval(text.clone())
            .expect("Treewalk run failed");
        let vm_val = self.vm.eval(text).expect("VM run failed");

        (tw_val, vm_val)
    }

    /// Run both engines, then return the error. We do not test for equality here, but rather leave
    /// that up to any downstream macros, so the source of the error will be clearer to the user.
    pub fn run_expect_error(mut self, text: Text) -> (RaisedMemphisError, RaisedMemphisError) {
        let treewalk_err = self
            .treewalk
            .eval(text.clone())
            .expect_err("Expected error from treewalk engine");
        let vm_err = self
            .vm
            .eval(text)
            .expect_err("Expected error from bytecode VM");

        (treewalk_err, vm_err)
    }
}
