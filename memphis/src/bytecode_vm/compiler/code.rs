use std::fmt::{Debug, Display, Error, Formatter};

use crate::{
    bytecode_vm::{
        compiler::{Bytecode, Constant, Opcode, opcode::OpcodeAnnotations},
        indices::{ConstantIndex, NonlocalIndex},
    },
    domain::{FunctionType, ModuleName},
};

/// Represent a range for handled exceptions to redirect to a given target PC. This is embedded in
/// the code object at the end of compilation. All labels must be resolved before this point.
#[derive(Clone, PartialEq)]
pub struct ExceptionRange {
    start: usize,
    end: usize,
    target: usize,
}

impl ExceptionRange {
    pub fn new(start: usize, end: usize, target: usize) -> Self {
        Self { start, end, target }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeSpec {
    module_name: ModuleName,
    name: String,
    filename: String,
    parameters: Vec<String>,
    function_type: FunctionType,
}

impl CodeSpec {
    pub fn new_root(module_name: ModuleName, filename: &str) -> Self {
        Self::new(
            "<module>",
            module_name,
            filename,
            &[],
            FunctionType::Regular,
        )
    }

    pub fn new(
        name: &str,
        module_name: ModuleName,
        filename: &str,
        parameters: &[&str],
        function_type: FunctionType,
    ) -> Self {
        Self {
            module_name,
            name: name.to_string(),
            filename: filename.to_string(),
            parameters: parameters.iter().map(|i| i.to_string()).collect(),
            function_type,
        }
    }

    pub fn with_function_type(mut self, function_type: FunctionType) -> Self {
        self.function_type = function_type;
        self
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arg_count(&self) -> usize {
        self.parameters.len()
    }

    pub fn parameters(&self) -> &[String] {
        &self.parameters
    }

    // This is a helper for debug output, this does _not_ have semantic meaning.
    pub fn dbg_context(&self) -> String {
        format!("{}.{}", self.module_name, self.name)
    }
}

/// Represents the bytecode and associated metadata for a block of Python code. It's a compiled
/// version of the source code, containing instructions that the VM can execute. This is immutable
/// and does not know about the context in which it is executed, meaning it doesn't hold references
/// to the global or local variables it operates on.
#[derive(Clone, PartialEq)]
pub struct CodeObject {
    spec: CodeSpec,
    bytecode: Bytecode,
    /// Local variable names (parameters + compiler-discovered locals)
    local_names: Vec<String>,
    /// Free variable names
    free_names: Vec<String>,
    /// Names addressed by nonlocal opcodes, not `nonlocal` declared names
    nonlocal_names: Vec<String>,
    constants: Vec<Constant>,
    line_map: Vec<(usize, usize)>,
    exception_table: Vec<ExceptionRange>,
}

impl CodeObject {
    pub fn empty_module(spec: CodeSpec) -> Self {
        Self {
            spec,
            bytecode: Vec::new(),
            free_names: Vec::new(),
            local_names: Vec::new(),
            nonlocal_names: Vec::new(),
            constants: Vec::new(),
            line_map: Vec::new(),
            exception_table: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn from_compiler(
        spec: CodeSpec,
        bytecode: Bytecode,
        local_names: Vec<String>,
        free_names: Vec<String>,
        nonlocal_names: Vec<String>,
        constants: Vec<Constant>,
        line_map: Vec<(usize, usize)>,
        exception_table: Vec<ExceptionRange>,
    ) -> Self {
        Self {
            spec,
            bytecode,
            local_names,
            free_names,
            nonlocal_names,
            constants,
            line_map,
            exception_table,
        }
    }

    pub fn name(&self) -> &str {
        &self.spec.name
    }

    // This is a helper for debug output, this does _not_ have semantic meaning.
    pub fn dbg_context(&self) -> String {
        self.spec.dbg_context()
    }

    pub fn path(&self) -> &str {
        &self.spec.filename
    }

    pub fn module_name(&self) -> &ModuleName {
        &self.spec.module_name
    }

    pub fn function_type(&self) -> &FunctionType {
        &self.spec.function_type
    }

    pub fn arg_count(&self) -> usize {
        self.spec.arg_count()
    }

    pub fn num_inst(&self) -> usize {
        self.bytecode.len()
    }

    pub fn inst_at(&self, pc: usize) -> Opcode {
        self.bytecode[pc]
    }

    pub fn nonlocal_name(&self, index: NonlocalIndex) -> &str {
        &self.nonlocal_names[*index]
    }

    pub fn constant(&self, index: ConstantIndex) -> &Constant {
        &self.constants[*index]
    }

    pub fn bytecode(&self) -> &[Opcode] {
        &self.bytecode
    }

    pub fn local_names(&self) -> &[String] {
        &self.local_names
    }

    pub fn free_names(&self) -> &[String] {
        &self.free_names
    }

    pub fn nonlocal_names(&self) -> &[String] {
        &self.nonlocal_names
    }

    pub fn constants(&self) -> &[Constant] {
        &self.constants
    }

    pub fn opcode_annotations(&self) -> OpcodeAnnotations<'_> {
        OpcodeAnnotations {
            local_names: &self.local_names,
            free_names: &self.free_names,
            nonlocal_names: &self.nonlocal_names,
            constants: &self.constants,
        }
    }

    pub fn get_line_number(&self, pc: usize) -> usize {
        match self
            .line_map
            .binary_search_by_key(&pc, |(offset, _)| *offset)
        {
            Ok(index) => self.line_map[index].1, // Exact match
            Err(index) => {
                if index == 0 {
                    0 // Default to first line if before first instruction
                } else {
                    self.line_map[index - 1].1 // Use the last known line number
                }
            }
        }
    }

    pub fn target_pc_for(&self, pc: usize) -> Option<usize> {
        self.exception_table
            .iter()
            .find(|entry| pc >= entry.start && pc < entry.end)
            .map(|i| i.target)
    }
}

impl Display for CodeObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<code {}>", self.name())
    }
}

impl Debug for CodeObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "CodeObject: {}", self.dbg_context())?;
        writeln!(f, "names:")?;
        for (index, name) in self.nonlocal_names.iter().enumerate() {
            writeln!(f, "[{index:?}]: {name}")?;
        }

        writeln!(f, "\nconstants:")?;
        for (index, constant) in self.constants.iter().enumerate() {
            writeln!(f, "[{index}]: {constant}")?;
        }

        for constant in self.constants.iter() {
            if let Constant::Code(code) = constant {
                writeln!(f, "\n{}:", code.name())?;
                for (index, opcode) in code.bytecode.iter().enumerate() {
                    writeln!(
                        f,
                        "{index}: {}",
                        opcode.display_annotated(&code.opcode_annotations())
                    )?;
                }
            }
        }

        writeln!(f, "\n{}:", self.name())?;
        for (index, opcode) in self.bytecode.iter().enumerate() {
            writeln!(
                f,
                "{index}: {}",
                opcode.display_annotated(&self.opcode_annotations())
            )?;
        }

        Ok(())
    }
}
