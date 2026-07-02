use std::mem::take;

use crate::{
    core::Container,
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        pausable::{Completion, FrameExit, Pausable, Suspension},
        types::Coroutine,
    },
};

/// An event loop which runs `Coroutine` objects.
pub struct Executor {
    current_coroutine: Option<Container<Coroutine>>,
    running: Vec<Container<Coroutine>>,
    spawned: Vec<Container<Coroutine>>,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    /// Create an `Executor`.
    pub fn new() -> Self {
        Self {
            current_coroutine: None,
            running: vec![],
            spawned: vec![],
        }
    }

    pub fn current_coroutine(&self) -> &Option<Container<Coroutine>> {
        &self.current_coroutine
    }

    /// The main interface to the `Executor` event loop. An `TreewalkValue` will be returned once
    /// the coroutine has resolved.
    pub fn run(
        &mut self,
        interpreter: &TreewalkInterpreter,
        coroutine: Container<Coroutine>,
    ) -> TreewalkResult<TreewalkValue> {
        self.running.push(coroutine.clone());

        loop {
            // Take the current queue of running coroutines
            let to_run = take(&mut self.running);
            for c in &to_run {
                if c.borrow().has_work() {
                    self.step_coroutine(interpreter, c.clone())?;
                }
            }
            // Push them back in for the next round
            self.running.extend(to_run);

            // Same pattern for spawned, which we also don't need to push back
            let new_spawns = take(&mut self.spawned);
            for c in &new_spawns {
                self.step_coroutine(interpreter, c.clone())?;
                self.running.push(c.clone());
            }

            // The event loop exits when its original coroutine has completed all its work. Other
            // spawned coroutines may or may not be finished by this time.
            if let Some(result) = coroutine.borrow().is_finished_with() {
                return Ok(result);
            }
        }
    }

    /// Launch a new `Coroutine`. This will be consumed at the end of the current iteration of the
    /// event loop.
    pub fn spawn(&mut self, coroutine: Container<Coroutine>) -> TreewalkResult<TreewalkValue> {
        coroutine.borrow_mut().context_mut().start();
        self.spawned.push(coroutine.clone());
        Ok(TreewalkValue::Coroutine(coroutine))
    }

    /// Do the next piece of work on a given `Coroutine`. After its work is done, check to
    /// see if it was put to sleep and handle it accordingly.
    fn step_coroutine(
        &mut self,
        interpreter: &TreewalkInterpreter,
        coroutine: Container<Coroutine>,
    ) -> TreewalkResult<()> {
        self.current_coroutine = Some(coroutine.clone());

        // Run the coroutine in a separate block to avoid lock contention
        let exit = { coroutine.borrow_mut().run_until_pause(interpreter)? };

        match exit {
            FrameExit::Completed(Completion::Return(val)) => {
                coroutine.borrow_mut().set_return_val(val);
            }
            FrameExit::Completed(Completion::Finished) => {
                coroutine.borrow_mut().set_return_val(TreewalkValue::None);
            }
            FrameExit::Suspended(Suspension::Yield(_)) => unimplemented!(
                "Async generators are currently not supported in the treewalk engine"
            ),
            FrameExit::Suspended(Suspension::Sleep(f)) => {
                coroutine.borrow_mut().sleep(f);
            }
            FrameExit::Suspended(Suspension::Await(c)) => {
                coroutine.borrow_mut().wait_on(c.clone());
                if !c.borrow().has_started() {
                    self.spawn(c.clone())?;
                }
            }
        }

        self.current_coroutine = None;
        Ok(())
    }
}
