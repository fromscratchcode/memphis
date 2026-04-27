use crate::parser::types::{Ast, Statement};

/// An association between an [`Ast`] of code and the current statement.
#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    pc: usize,
    ast: Ast,
}

impl Frame {
    /// Initialize a [`Frame`].
    pub fn new(ast: Ast) -> Self {
        Self { ast, pc: 0 }
    }

    /// Initialize a [`Frame`] in a completed state so the caller can use the normal loop
    /// restart path to begin execution.
    pub fn new_finished(ast: Ast) -> Self {
        let pc = ast.len();
        Self { ast, pc }
    }

    /// Return a boolean indicating whether we have instructions left in the block to evaluate.
    pub fn is_finished(&self) -> bool {
        self.len() <= self.pc
    }

    pub fn current_statement(&self) -> &Statement {
        self.ast.get(self.pc).expect("No statement!")
    }

    pub fn advance_pc(&mut self) {
        self.pc += 1;
    }

    /// Reset the program counter to the start of the block. This is useful to simulate loops.
    pub fn restart(&mut self) {
        self.pc = 0;
    }

    /// Return the length of the block held by this frame.
    fn len(&self) -> usize {
        self.ast.len()
    }
}
