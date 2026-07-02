use crate::{
    bytecode_vm::runtime::Frame,
    core::{log, Container, LogLevel},
    runtime::MemphisState,
};

/// All code which is executed lives inside a [`Frame`] on this call stack.
pub struct CallStack {
    stack: Vec<Frame>,
    state: Container<MemphisState>,
}

impl CallStack {
    pub fn new(state: Container<MemphisState>) -> Self {
        Self {
            stack: vec![],
            state,
        }
    }

    pub fn push(&mut self, frame: Frame) {
        log(LogLevel::Trace, || {
            format!("Pushing frame: {}", frame.function.code_object.name())
        });
        // If we don't save the current line number, we won't properly record where in the current
        // file we called the next function from. We do something similar in the treewalk
        // interpreter.
        if !self.is_empty() {
            self.state.save_line_number();
        }
        self.state.push_stack_frame(&frame);
        self.stack.push(frame);
    }

    pub fn pop(&mut self) -> Option<Frame> {
        self.state.pop_stack_frame();

        if let Some(frame) = self.stack.pop() {
            log(LogLevel::Trace, || {
                format!("Popping frame: {}", frame.function.code_object.name())
            });
            Some(frame)
        } else {
            None
        }
    }

    pub fn top(&self) -> &Frame {
        self.stack.last().expect("Empty call stack")
    }

    pub fn top_mut(&mut self) -> &mut Frame {
        self.stack.last_mut().expect("Empty call stack")
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn advance_pc(&mut self) {
        let frame = self.top_mut();
        log(LogLevel::Trace, || {
            format!(
                "Advancing PC in module: {}",
                frame.function.code_object.name()
            )
        });
        frame.pc = frame.next_pc();
    }

    pub fn jump_to_offset(&mut self, offset: isize) {
        let new_offset = self.top().pc_plus_offset(offset);
        self.top_mut().pc = new_offset;
    }
}
