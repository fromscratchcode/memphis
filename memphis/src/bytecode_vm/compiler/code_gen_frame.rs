use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::bytecode_vm::compiler::{
    opcode::{SignedOffset, UnsignedOffset},
    CodeObject, ExceptionRange, Opcode,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LabelId(usize);

impl Display for LabelId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}

#[derive(Debug)]
struct PendingJump {
    at: UnsignedOffset,
    kind: JumpKind,
}

#[derive(Debug, Copy, Clone)]
pub enum JumpKind {
    Jump,
    JumpIfFalse,
    JumpIfTrue,
    PopJumpIfFalse,
    ForIter,
}

impl JumpKind {
    fn name(&self) -> &'static str {
        match self {
            JumpKind::Jump => "JUMP",
            JumpKind::JumpIfFalse => "JUMP_IF_FALSE",
            JumpKind::JumpIfTrue => "JUMP_IF_TRUE",
            JumpKind::PopJumpIfFalse => "POP_JUMP_IF_FALSE",
            JumpKind::ForIter => "FOR_ITER",
        }
    }

    fn to_opcode(self, offset: SignedOffset) -> Opcode {
        match self {
            JumpKind::Jump => Opcode::Jump(offset),
            JumpKind::JumpIfFalse => Opcode::JumpIfFalse(offset),
            JumpKind::JumpIfTrue => Opcode::JumpIfTrue(offset),
            JumpKind::PopJumpIfFalse => Opcode::PopJumpIfFalse(offset),
            JumpKind::ForIter => Opcode::ForIter(offset),
        }
    }
}

#[derive(Debug)]
pub struct CodeGenFrame {
    code: CodeObject,

    next_label: usize,
    labels: HashMap<LabelId, UnsignedOffset>,
    pending_jumps: Vec<(LabelId, PendingJump)>,
    pending_ranges: Vec<(LabelId, LabelId, LabelId)>,
}

impl CodeGenFrame {
    pub fn new(code: CodeObject) -> Self {
        Self {
            code,
            next_label: 0,
            labels: HashMap::new(),
            pending_jumps: Vec::new(),
            pending_ranges: Vec::new(),
        }
    }

    pub fn code(&self) -> &CodeObject {
        &self.code
    }

    pub fn code_mut(&mut self) -> &mut CodeObject {
        &mut self.code
    }

    pub fn finalize(mut self) -> CodeObject {
        for (label, jump) in self.pending_jumps.drain(..) {
            let target = *self.labels.get(&label).expect("Unbound label");
            let src = jump.at;
            let offset = target as SignedOffset - src as SignedOffset - 1;
            self.code.bytecode[src] = jump.kind.to_opcode(offset);
        }

        for (start, end, target) in self.pending_ranges.drain(..) {
            let start_pc = *self.labels.get(&start).expect("Unbound label");
            let end_pc = *self.labels.get(&end).expect("Unbound label");
            let target_pc = *self.labels.get(&target).expect("Unbound label");
            let range = ExceptionRange::new(start_pc, end_pc, target_pc);
            self.code.exception_table.push(range);
        }

        self.code
    }

    pub fn new_label(&mut self) -> LabelId {
        let id = LabelId(self.next_label);
        self.next_label += 1;
        id
    }

    pub fn bind_label(&mut self, label: LabelId) {
        let offset = self.code.bytecode.len();
        self.labels.insert(label, offset);
    }

    pub fn emit_jump_to(&mut self, label: LabelId, kind: JumpKind) {
        let at = self.code.bytecode.len();
        self.code.bytecode.push(Opcode::Placeholder);
        self.pending_jumps.push((label, PendingJump { at, kind }));
    }

    pub fn register_range(&mut self, start: LabelId, end: LabelId, target: LabelId) {
        self.pending_ranges.push((start, end, target));
    }

    pub fn debug_disasm_with_labels(&self) -> String {
        use std::fmt::Write;

        let mut out = String::new();

        // Invert label map: offset -> labels
        let mut labels_at: Vec<Vec<LabelId>> = vec![Vec::new(); self.code.bytecode.len() + 1];
        for (label, &offset) in &self.labels {
            labels_at[offset].push(*label);
        }

        let mut pending_at: HashMap<UnsignedOffset, (&LabelId, &JumpKind)> = HashMap::new();
        for (label, pending) in &self.pending_jumps {
            pending_at.insert(pending.at, (label, &pending.kind));
        }

        writeln!(out, "Disassembly for {}", self.code.name()).unwrap();

        for (i, opcode) in self.code.bytecode.iter().enumerate() {
            for label in &labels_at[i] {
                writeln!(out, "{}:", label).unwrap();
            }

            writeln!(
                out,
                "  {:>4}: {}",
                i,
                self.format_opcode(opcode, i, &pending_at)
            )
            .unwrap();
        }

        // Labels bound at end-of-code
        for label in &labels_at[self.code.bytecode.len()] {
            writeln!(out, "{}:", label).unwrap();
        }

        if !self.pending_ranges.is_empty() {
            writeln!(out, "Exception Table:").unwrap();
            for (start, end, target) in self.pending_ranges.iter() {
                writeln!(out, "  {} to {} -> {}", start, end, target).unwrap();
            }
        }

        out
    }

    fn format_opcode(
        &self,
        opcode: &Opcode,
        at: UnsignedOffset,
        pending_at: &HashMap<UnsignedOffset, (&LabelId, &JumpKind)>,
    ) -> String {
        match opcode {
            Opcode::Placeholder => {
                let (label, kind) = pending_at.get(&at).expect("Unlabeled placeholder!");
                format!("{} -> {}", kind.name(), label)
            }
            Opcode::ForIter(_)
            | Opcode::Jump(_)
            | Opcode::JumpIfFalse(_)
            | Opcode::PopJumpIfFalse(_)
            | Opcode::JumpIfTrue(_) => panic!("Jumps should not yet exist in the code!"),
            _ => opcode.display_annotated(&self.code),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode_vm::compiler::{CodeObject, Constant, Opcode};
    use crate::bytecode_vm::indices::Index;
    use crate::domain::ModuleName;

    #[test]
    fn debug_disasm_shows_labels_and_annotated_operands() {
        // Build a minimal code object
        let mut code = CodeObject::new_root(ModuleName::from_segments(&["<module>"]), "<test>");

        // Constants
        code.constants.push(Constant::Int(1));
        code.constants.push(Constant::Int(2));
        code.constants.push(Constant::Int(-1));

        // Globals
        code.names.push("i".to_string());
        code.names.push("a".to_string());

        let mut frame = CodeGenFrame::new(code);

        frame.code_mut().bytecode.extend([
            Opcode::LoadConst(Index::new(0)),
            Opcode::LoadConst(Index::new(1)),
            Opcode::BuildList(2),
            Opcode::GetIter,
        ]);

        let loop_start = frame.new_label(); // L0
        let loop_end = frame.new_label(); // L1

        frame.bind_label(loop_start);
        frame.emit_jump_to(loop_end, JumpKind::ForIter);

        frame.code_mut().bytecode.extend([
            Opcode::StoreGlobal(Index::new(0)),
            Opcode::LoadConst(Index::new(2)),
            Opcode::StoreGlobal(Index::new(1)),
        ]);

        frame.emit_jump_to(loop_start, JumpKind::Jump);
        frame.bind_label(loop_end);

        let output = frame.debug_disasm_with_labels();

        let expected = r#"
Disassembly for <module>
     0: LOAD_CONST 0 (1)
     1: LOAD_CONST 1 (2)
     2: BUILD_LIST 2
     3: GET_ITER
L0:
     4: FOR_ITER -> L1
     5: STORE_GLOBAL 0 (i)
     6: LOAD_CONST 2 (-1)
     7: STORE_GLOBAL 1 (a)
     8: JUMP -> L0
L1:
"#
        .trim_start();

        assert_eq!(output, expected);
    }
}
