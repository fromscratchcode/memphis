use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Opcode},
        indices::{ConstantIndex, FreeIndex, LocalIndex, NonlocalIndex},
        result::Raise,
        runtime::{
            modules::builtins,
            types::{Coroutine, Exception, FunctionObject, Generator, Method, Module},
            CallStack, Frame, Reference, StepResult, VmExecutor,
        },
        DomainResult, RaisedException, Runtime, VmContext, VmResult, VmValue,
    },
    core::{log, log_impure, Container, LogLevel},
    domain::{Dunder, MemphisValue, ModuleName, ModuleOrigin},
    runtime::MemphisState,
};

mod opcodes;

pub struct VirtualMachine {
    runtime: Container<Runtime>,

    state: Container<MemphisState>,

    pub executor: VmExecutor,

    call_stack: CallStack,
}

impl VirtualMachine {
    pub fn new(state: Container<MemphisState>, runtime: Container<Runtime>) -> Self {
        let call_stack = CallStack::new(state.clone());

        Self {
            state,
            runtime,
            executor: VmExecutor::default(),
            call_stack,
        }
    }

    pub fn raise(&self, exception: Exception) -> RaisedException {
        self.state.save_line_number();
        RaisedException::new(self.state.debug_call_stack(), exception)
    }

    pub fn execute(&mut self, code: CodeObject) -> VmResult<VmValue> {
        self.load(code).raise(self)?;
        self.run_loop()
    }

    pub fn read_global(&self, name: &str) -> DomainResult<VmValue> {
        let reference = self
            .load_global_by_name(name)
            .ok_or_else(Exception::runtime_error)?;
        self.deref(reference)
    }

    /// Read a global variable from the `__main__` module.
    // TODO this should really only be available in test/repl mode, but we currently call this in
    // the Interpreter trait. The other option is splitting Interpreter into two traits and putting
    // the read one behind a test/repl flag.
    //
    // We use unwrap here because:
    // 1) the main module should always exist, and
    // 2) constructing a real error with a heapified string would require this to take a mutable
    //    reference to the VM, which would ripple through the tests.
    fn load_global_by_name(&self, name: &str) -> Option<Reference> {
        let module = self
            .runtime
            .borrow()
            .read_module(&ModuleName::main())
            .unwrap();

        let module_binding = module.borrow();
        module_binding.read(name)
    }

    fn current_module(&self) -> DomainResult<Container<Module>> {
        Ok(self.current_frame()?.module.clone())
    }

    pub fn read_module(&mut self, name: &ModuleName) -> DomainResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(name) {
            return Ok(module);
        }

        let msg = VmValue::String(format!("Failed to read module: {}", name));
        Err(Exception::runtime_error_with(self.heapify(msg)))
    }

    /// Check if the module is already present (e.g. Rust-backed or previously imported).
    pub fn read_or_load_module(&mut self, module_name: &ModuleName) -> VmResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(module_name) {
            return Ok(module);
        }

        self.import_from_source(module_name)
    }

    fn import_from_source(&mut self, module_name: &ModuleName) -> VmResult<Container<Module>> {
        let (_resolved, source) = self
            .state
            .load_source(module_name)
            .map_err(|err| {
                let msg = VmValue::String(err.message);
                Exception::import_error(self.heapify(msg))
            })
            .raise(self)?;

        let module = self.runtime.borrow_mut().create_module(module_name);

        let mut context = VmContext::from_state(
            module_name.clone(),
            source.text().clone(),
            ModuleOrigin::File(source.path().to_path_buf()),
            self.state.clone(),
            self.runtime.clone(),
        );

        context.run_inner()?;

        Ok(module)
    }

    fn current_frame(&self) -> DomainResult<&Frame> {
        self.call_stack.top_frame()
    }

    fn current_frame_mut(&mut self) -> DomainResult<&mut Frame> {
        self.call_stack.top_frame_mut()
    }

    fn read_constant(&self, index: ConstantIndex) -> DomainResult<VmValue> {
        self.current_frame()?
            .function
            .code_object
            .constants
            .get(*index)
            .map(|c| c.into())
            .ok_or_else(Exception::runtime_error)
    }

    fn update_fn<F>(&mut self, obj_ref: Reference, function: F)
    where
        F: FnOnce(&mut VmValue),
    {
        if let Some(object_value) = self.runtime.borrow_mut().heap.get_mut(obj_ref) {
            function(object_value)
        }
    }

    fn store_global(&mut self, index: NonlocalIndex, value: Reference) -> DomainResult<()> {
        let name = self.resolve_name(index)?;
        self.current_module()?.borrow_mut().write(name, value);
        Ok(())
    }

    fn store_local(&mut self, index: LocalIndex, value: Reference) -> DomainResult<()> {
        let current_frame = self.current_frame_mut()?;
        if current_frame.locals.len() == *index {
            current_frame.locals.push(value);
        } else {
            current_frame.locals[*index] = value;
        }
        Ok(())
    }

    fn load_local(&self, index: LocalIndex) -> DomainResult<Reference> {
        Ok(self.current_frame()?.locals[*index])
    }

    fn load_free(&self, index: FreeIndex) -> DomainResult<Reference> {
        Ok(self.current_frame()?.function.freevars[*index])
    }

    fn load_global(&mut self, index: NonlocalIndex) -> DomainResult<Reference> {
        let name = self.resolve_name(index)?;

        if let Some(val) = self.current_module()?.borrow().read(name) {
            return Ok(val);
        }

        if let Some(builtins) = self
            .runtime
            .borrow()
            .read_module(&ModuleName::from_segments(&[Dunder::Builtins]))
        {
            if let Some(val) = builtins.borrow().read(name) {
                return Ok(val);
            }
        }

        let name = VmValue::String(name.to_string());
        Err(Exception::name_error(self.heapify(name)))
    }

    fn resolve_name(&self, index: NonlocalIndex) -> DomainResult<&str> {
        Ok(&self.current_frame()?.function.code_object.names[*index])
    }

    fn peek(&mut self) -> DomainResult<Reference> {
        let frame = self.current_frame_mut()?;

        if let Some(value) = frame.stack.last() {
            return Ok(*value);
        }

        Err(Exception::runtime_error())
    }

    fn pop(&mut self) -> DomainResult<Reference> {
        let frame = self.current_frame_mut()?;

        if let Some(value) = frame.stack.pop() {
            log_impure(LogLevel::Trace, || {
                println!("After pop:");
                self.dump_frame();
            });
            return Ok(value);
        }

        Err(Exception::runtime_error())
    }

    fn push(&mut self, value: Reference) -> DomainResult<()> {
        let frame = self.current_frame_mut()?;
        frame.stack.push(value);

        log_impure(LogLevel::Trace, || {
            println!("After push:");
            self.dump_frame();
        });

        Ok(())
    }

    fn dump_frame(&self) {
        let frame = self.current_frame().expect("No frame!");

        for (index, stack_var) in frame.stack.iter().rev().enumerate() {
            println!(
                "stack[{index}] = {}",
                stack_var.display_annotated(&self.runtime.borrow().heap)
            );
        }

        for (index, local) in frame.locals.iter().rev().enumerate() {
            println!(
                "local[{index}] = {}",
                local.display_annotated(&self.runtime.borrow().heap)
            );
        }
    }

    /// Extract primitives and resolve any references to a [`VmValue`]. All modifications should
    /// occur through VM instructions.
    pub fn deref(&self, reference: Reference) -> DomainResult<VmValue> {
        let val = match reference {
            Reference::ObjectRef(_) => self
                .runtime
                .borrow()
                .heap
                .get(reference)
                .cloned()
                .ok_or_else(Exception::runtime_error)?,
            Reference::Int(i) => VmValue::Int(i),
            Reference::Float(f) => VmValue::Float(f),
        };

        Ok(val)
    }

    pub fn normalize_vm_ref(&self, r: Reference) -> DomainResult<MemphisValue> {
        let value = self.deref(r)?;
        self.normalize_vm_value(value)
    }

    pub fn normalize_vm_value(&self, value: VmValue) -> DomainResult<MemphisValue> {
        let val = match value {
            VmValue::None => MemphisValue::None,
            VmValue::Int(i) => MemphisValue::Integer(i),
            VmValue::Float(f) => MemphisValue::Float(f),
            VmValue::String(s) => MemphisValue::Str(s),
            VmValue::Bool(b) => MemphisValue::Boolean(b),
            VmValue::List(i) => {
                let items = i
                    .items
                    .iter()
                    .map(|r| self.normalize_vm_ref(*r))
                    .collect::<DomainResult<Vec<_>>>()?;
                MemphisValue::List(items)
            }
            VmValue::Tuple(i) => {
                let items = i
                    .items
                    .iter()
                    .map(|r| self.normalize_vm_ref(*r))
                    .collect::<DomainResult<Vec<_>>>()?;
                MemphisValue::Tuple(items)
            }
            VmValue::Dict(i) => {
                let items = i
                    .items
                    .iter()
                    .map(|(k, v)| Ok((self.normalize_vm_ref(*k)?, self.normalize_vm_ref(*v)?)))
                    .collect::<DomainResult<Vec<(_, _)>>>()?;
                MemphisValue::Dict(items)
            }
            VmValue::Range(r) => MemphisValue::Range(r.start, r.stop, r.step),
            VmValue::Method(m) => MemphisValue::Method(m.name()),
            VmValue::Function(f) => MemphisValue::Function(f.name().to_string()),
            VmValue::Module(m) => MemphisValue::Module(m.borrow().name().to_string()),
            VmValue::Coroutine(_) => MemphisValue::Coroutine,
            VmValue::Generator(_) => MemphisValue::Generator,
            VmValue::Class(c) => MemphisValue::Class(c.name().to_string()),
            VmValue::Object(o) => {
                let class_name = self.normalize_vm_ref(o.class)?;
                MemphisValue::Object(class_name.to_string())
            }
            VmValue::Code(_) => MemphisValue::Code,
            VmValue::ListIter(_) => MemphisValue::ListIter,
            VmValue::RangeIter(_) => MemphisValue::RangeIter,
            VmValue::TupleIter(_) => MemphisValue::TupleIter,
            VmValue::BuiltinFunction(f) => MemphisValue::BuiltinFunction(f.name().to_string()),
            VmValue::SleepFuture(_) => todo!(),
        };

        Ok(val)
    }

    /// Resolves an attribute without applying method binding (used in tests or low-level access).
    pub fn resolve_raw_attr(&self, value: &VmValue, name: &str) -> DomainResult<Reference> {
        if let Some(object) = value.as_object() {
            object
                .read(name, self)?
                .ok_or_else(|| Exception::attribute_error(&value.get_type(), name))
        } else if let Some(module) = value.as_module() {
            module
                .borrow()
                .read(name)
                .ok_or_else(|| Exception::attribute_error(&value.get_type(), name))
        } else {
            unimplemented!()
        }
    }

    /// Resolves an attribute and applies method binding if it is a function.
    pub fn resolve_attr(&mut self, object_ref: Reference, name: &str) -> DomainResult<Reference> {
        let object = self.deref(object_ref)?;

        let attr_ref = self.resolve_raw_attr(&object, name)?;
        let attr_val = self.deref(attr_ref)?;

        let bound = match attr_val {
            VmValue::Function(f) if object.should_bind() => {
                self.heapify(VmValue::Method(Method::new(object_ref, f.clone())))
            }
            _ => attr_ref,
        };

        Ok(bound)
    }

    pub fn coerce_to_int(&mut self, value: &VmValue) -> DomainResult<i64> {
        match value {
            VmValue::Int(i) => Ok(*i),
            VmValue::String(s) => s.parse::<i64>().map_err(|_| {
                let msg = VmValue::String("Invalid int literal".to_string());
                Exception::value_error(self.heapify(msg))
            }),
            _ => {
                let msg = VmValue::String("Cannot coerce to an int".to_string());
                Err(Exception::type_error(self.heapify(msg)))
            }
        }
    }

    /// Primitives are stored inline on the stack, we create a reference to the global store for
    /// all other types.
    pub fn heapify(&mut self, value: VmValue) -> Reference {
        match value {
            // This case is only needed when we receive a generic `VmValue`, i.e. loading a
            // constant. For cases where we know we have a boolean, it is preferred to use
            // `to_heapified_bool` directly.
            VmValue::Bool(bool_val) => self.to_heapified_bool(bool_val),
            VmValue::Int(_) | VmValue::Float(_) => value.into_ref(),
            _ => self.runtime.borrow_mut().heap.allocate(value),
        }
    }

    pub fn none(&self) -> Reference {
        self.runtime.borrow().heap.none()
    }

    pub fn true_(&self) -> Reference {
        self.runtime.borrow().heap.true_()
    }

    pub fn false_(&self) -> Reference {
        self.runtime.borrow().heap.false_()
    }

    pub fn to_heapified_bool(&self, value: bool) -> Reference {
        if value {
            self.true_()
        } else {
            self.false_()
        }
    }

    /// Dereferences the top value on the stack.
    fn peek_value(&mut self) -> DomainResult<VmValue> {
        let reference = self.peek()?;
        self.deref(reference)
    }

    /// Pops and dereferences a value.
    fn pop_value(&mut self) -> DomainResult<VmValue> {
        let reference = self.pop()?;
        self.deref(reference)
    }

    fn push_value(&mut self, value: VmValue) -> DomainResult<()> {
        let reference = self.heapify(value);
        self.push(reference)
    }

    fn collect_n(&mut self, n: usize) -> DomainResult<Vec<Reference>> {
        let mut items = Vec::with_capacity(n);
        for _ in 0..n {
            items.push(self.pop()?);
        }
        // Reverse the items since we pop them off in reverse order
        items.reverse();
        Ok(items)
    }

    fn try_string_multiplication(a: &VmValue, b: &VmValue) -> Option<VmValue> {
        match (a, b) {
            (VmValue::String(s), VmValue::Int(n)) | (VmValue::Int(n), VmValue::String(s)) => {
                let result = if *n < 0 {
                    "".to_string()
                } else {
                    s.repeat(*n as usize)
                };
                Some(VmValue::String(result))
            }
            _ => None,
        }
    }

    fn binary_op<F>(&mut self, opcode: Opcode, op: F, force_float: bool) -> DomainResult<()>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        let result = if opcode == Opcode::Mul {
            if let Some(result) = Self::try_string_multiplication(&a, &b) {
                result
            } else {
                self.binary_numeric_op(op, &a, &b, force_float)?
            }
        } else {
            self.binary_numeric_op(op, &a, &b, force_float)?
        };

        self.push_value(result)?;
        Ok(())
    }

    fn cmp_op<F>(&mut self, op: F) -> DomainResult<()>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let b = self.pop_value()?;
        let a = self.pop_value()?;
        let result_ref = self.dynamic_cmp(&a, &b, op)?;
        self.push(result_ref)?;
        Ok(())
    }

    fn binary_numeric_op<F>(
        &mut self,
        op: F,
        a: &VmValue,
        b: &VmValue,
        force_float: bool,
    ) -> DomainResult<VmValue>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let result = match (a, b) {
            (VmValue::Int(x), VmValue::Int(y)) => {
                let res = op(*x as f64, *y as f64);
                if force_float {
                    VmValue::Float(res)
                } else {
                    VmValue::Int(res as i64)
                }
            }
            (VmValue::Float(x), VmValue::Float(y)) => VmValue::Float(op(*x, *y)),
            (VmValue::Int(x), VmValue::Float(y)) => VmValue::Float(op(*x as f64, *y)),
            (VmValue::Float(x), VmValue::Int(y)) => VmValue::Float(op(*x, *y as f64)),
            _ => {
                let msg =
                    VmValue::String("Unsupported operand types for binary operation".to_string());
                return Err(Exception::type_error(self.heapify(msg)));
            }
        };

        Ok(result)
    }

    fn dynamic_cmp<F>(&mut self, a: &VmValue, b: &VmValue, op: F) -> DomainResult<Reference>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let result = match (a, b) {
            (VmValue::Int(x), VmValue::Int(y)) => op(*x as f64, *y as f64),
            (VmValue::Float(x), VmValue::Float(y)) => op(*x, *y),
            (VmValue::Int(x), VmValue::Float(y)) => op(*x as f64, *y),
            (VmValue::Float(x), VmValue::Int(y)) => op(*x, *y as f64),
            _ => {
                let msg = VmValue::String("Unsupported operand types for comparison".to_string());
                return Err(Exception::type_error(self.heapify(msg)));
            }
        };

        Ok(self.to_heapified_bool(result))
    }

    fn dynamic_negate(&mut self, value: &VmValue) -> DomainResult<VmValue> {
        let result = match value {
            VmValue::Int(x) => VmValue::Int(-x),
            VmValue::Float(x) => VmValue::Float(-x),
            _ => {
                let msg = VmValue::String("Unsupported operand type for unary '-'".to_string());
                return Err(Exception::type_error(self.heapify(msg)));
            }
        };

        Ok(result)
    }

    fn value_in_iter(&mut self, needle: VmValue, haystack: VmValue) -> VmResult<bool> {
        let iter = builtins::iter_internal(self, haystack)?;
        loop {
            match builtins::next_internal(self, iter)? {
                Some(item_ref) => {
                    if needle == self.deref(item_ref).raise(self)? {
                        return Ok(true);
                    }
                }
                None => return Ok(false),
            }
        }
    }

    // === Declarative VM Call Interface ===

    /// Push a new `Frame` to the call stack and immediately execute it to completion, returning
    /// its return value.
    pub fn call(&mut self, frame: Frame) -> VmResult<Reference> {
        let (step_result, _frame) = self.run_frame(frame)?;
        Ok(unwrap_return_value(step_result))
    }

    /// Push a new `Frame` to the call stack and immediately execute it to completion, returning
    /// the frame. Useful for class definitions.
    pub fn call_and_return_frame(&mut self, frame: Frame) -> VmResult<Frame> {
        let (step_result, frame) = self.run_frame(frame)?;
        match step_result {
            StepResult::Return(val) => assert_eq!(
                val,
                self.none(),
                "`call_and_return_frame` expects the frame to return None."
            ),
            other => panic!("Unexpected step result in `call_and_return_frame`: {other:?}"),
        }
        Ok(frame)
    }

    pub fn resume_generator(
        &mut self,
        generator: Container<Generator>,
    ) -> VmResult<Option<Reference>> {
        let frame = generator.borrow_mut().frame.clone();
        let (step_result, new_frame) = self.run_frame(frame)?;
        let return_val = match step_result {
            StepResult::Yield(val) => Some(val),
            StepResult::Return(_) => None,
            StepResult::Sleep(_) | StepResult::Await(_) => {
                panic!("Async generators are not currently supported.")
            }
            other => panic!("Unexpected step result in `resume_generator`: {other:?}"),
        };
        generator.borrow_mut().frame = new_frame;

        Ok(return_val)
    }

    pub fn step_coroutine(&mut self, coroutine: Container<Coroutine>) -> VmResult<StepResult> {
        let (step_result, new_frame) = self.run_frame(coroutine.borrow().frame.clone())?;
        coroutine.borrow_mut().frame = new_frame;
        Ok(step_result)
    }

    /// Push a frame and run it, capturing the result and returning the frame.
    /// We need to capture the frame when it is finished for creating new Classes and for saving
    /// the state of a Coroutine.
    fn run_frame(&mut self, frame: Frame) -> VmResult<(StepResult, Frame)> {
        self.call_stack.push(frame);
        self.run_top_frame()
    }

    // === Internal Execution Flow ===

    /// Executes all frames in the call stack to completion.
    /// This is the main loop of the VM.
    fn run_loop(&mut self) -> VmResult<VmValue> {
        let mut result = self.none();
        while !self.call_stack.is_finished() {
            let (step_result, _frame) = self.run_top_frame()?;
            result = match step_result {
                StepResult::Return(val) => val,
                StepResult::Halt => self.none(),
                other => panic!("Unexpected step result in `run_loop`: {other:?}"),
            };
        }

        self.deref(result).raise(self)
    }

    /// Run the top frame in the call stack to completion and then return.
    fn run_top_frame(&mut self) -> VmResult<(StepResult, Frame)> {
        while self.call_stack.top().is_some_and(|f| !f.is_finished()) {
            let result = self.step_frame()?;
            match result {
                StepResult::Continue => continue,
                _ => {
                    let frame = self
                        .call_stack
                        .pop()
                        .ok_or_else(Exception::runtime_error)
                        .raise(self)?;
                    return Ok((result, frame));
                }
            }
        }

        // If we fell out of the loop: frame is finished with no explicit return
        let frame = self
            .call_stack
            .pop()
            .ok_or_else(Exception::runtime_error)
            .raise(self)?;
        Ok((StepResult::Return(self.none()), frame))
    }

    /// Run the next instruction on the top frame in the call stack.
    fn step_frame(&mut self) -> VmResult<StepResult> {
        let frame = self.current_frame().raise(self)?;

        // Save this in case we encounter a runtime exception and need to record this info in
        // the stack trace
        self.state.set_line_number(frame.current_line());
        let opcode = frame.current_inst();

        log_impure(LogLevel::Debug, || self.dump_frame());
        log(LogLevel::Debug, || frame.current_inst_annotated());

        let result = self.execute_opcode(opcode)?;

        if matches!(result, StepResult::Continue) {
            self.call_stack.advance_pc().raise(self)?;
        }

        Ok(result)
    }

    fn load(&mut self, code: CodeObject) -> DomainResult<()> {
        log(LogLevel::Debug, || format!("{code:?}"));

        let function = FunctionObject::new(code);
        let module = self.read_module(&function.code_object.module_name)?;
        let frame = Frame::new(function, vec![], module);

        self.call_stack.push(frame);
        Ok(())
    }
}

/// Expects the frame to return a value. Panics if the result was not `StepResult::Return`.
fn unwrap_return_value(step_result: StepResult) -> Reference {
    match step_result {
        StepResult::Return(val) => val,
        other => panic!("Expected StepResult::Return, got {other:?}"),
    }
}

#[cfg(test)]
/// These test conditions related to the VM's internal state.
mod tests {
    use super::*;

    use crate::bytecode_vm::test_utils::*;

    #[test]
    /// We're testing for basic memory-efficiency here. The original implementation created
    /// unnecessary copies of the object.
    fn object_store_duplicates() {
        let text = r#"
class Foo:
    def __init__(self):
        self.x = 44

f = Foo()
"#;
        let ctx = run(text);
        let runtime = ctx.vm().runtime.borrow();
        let objects: Vec<&VmValue> = runtime
            .heap
            .iter()
            .filter(|object| matches!(object, VmValue::Object(_)))
            .collect();
        assert_eq!(objects.len(), 1);
    }
}
