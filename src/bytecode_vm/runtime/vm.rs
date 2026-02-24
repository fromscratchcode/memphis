use crate::{
    bytecode_vm::{
        compiler::{CodeObject, Opcode},
        indices::{ConstantIndex, FreeIndex, LocalIndex, NonlocalIndex},
        result::Raise,
        runtime::{
            modules::builtins,
            types::{Coroutine, Exception, FunctionObject, Generator, Method, Module},
            CallStack, Completion, Frame, FrameExit, Reference, StepResult, Suspension, VmExecutor,
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

    exception_stack: Vec<Exception>,
}

impl VirtualMachine {
    pub fn new(state: Container<MemphisState>, runtime: Container<Runtime>) -> Self {
        let call_stack = CallStack::new(state.clone());

        Self {
            state,
            runtime,
            executor: VmExecutor::default(),
            call_stack,
            exception_stack: vec![],
        }
    }

    pub fn raise(&self, exception: Exception) -> RaisedException {
        self.state.save_line_number();
        RaisedException::new(self.state.debug_call_stack(), exception)
    }

    pub fn raise_step(&mut self, exc: Exception) -> StepResult {
        StepResult::Exit(FrameExit::Completed(Completion::Exception(self.raise(exc))))
    }

    /// This sets up a minimal call stack, just enough to throw an exception encountered upstream.
    pub fn init_and_raise(
        &mut self,
        exception: Exception,
        module_name: ModuleName,
        path_str: &str,
    ) -> RaisedException {
        let code = CodeObject::new_root(module_name, path_str);
        let frame = self.frame_for_code(code);
        self.call_stack.push(frame);
        self.raise(exception)
    }

    pub fn execute(&mut self, code: CodeObject) -> VmResult<VmValue> {
        assert!(
            self.call_stack.is_empty(),
            "VM execute called with non-empty call stack"
        );

        log(LogLevel::Debug, || format!("{:?}", code));
        let frame = self.frame_for_code(code);

        let result = self.call(frame);
        assert!(
            self.call_stack.is_empty(),
            "VM execute returned with non-empty call stack"
        );
        result.map(|reference| self.deref(reference))
    }

    pub fn intern_string(&mut self, val: &str) -> Reference {
        let s = VmValue::Str(val.to_string());
        self.heapify(s)
    }

    pub fn read_global(&self, name: &str) -> Option<VmValue> {
        let reference = self.load_global_by_name(name)?;
        Some(self.deref(reference))
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
        let module = self.read_module(&ModuleName::main());
        let module_binding = module.borrow();
        module_binding.read(name)
    }

    fn current_module(&self) -> Container<Module> {
        self.current_frame().module.clone()
    }

    pub fn read_module(&self, name: &ModuleName) -> Container<Module> {
        if let Some(module) = self.runtime.borrow().read_module(name) {
            module
        } else {
            panic!("VM consistency error: Module not found!");
        }
    }

    /// Check if the module is already present (e.g. Rust-backed or previously imported).
    pub fn read_or_load_module(&mut self, module_name: &ModuleName) -> VmResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(module_name) {
            return Ok(module);
        }

        self.import_from_source(module_name)
    }

    fn import_from_source(&mut self, module_name: &ModuleName) -> VmResult<Container<Module>> {
        let (resolved, source) = self
            .state
            .load_source(module_name)
            .map_err(|err| {
                let msg = self.intern_string(&err.message);
                Exception::import_error(msg)
            })
            .raise(self)?;

        let module = self.runtime.borrow_mut().create_module(module_name);

        let mut context = VmContext::from_state(
            resolved.name.clone(),
            resolved.package.clone(),
            source.text().clone(),
            ModuleOrigin::File(source.path().to_path_buf()),
            self.state.clone(),
            self.runtime.clone(),
        );

        context.run_inner()?;

        Ok(module)
    }

    fn current_frame(&self) -> &Frame {
        self.call_stack.top()
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.call_stack.top_mut()
    }

    fn read_constant(&self, index: ConstantIndex) -> VmValue {
        self.current_frame()
            .function
            .code_object
            .constants
            .get(*index)
            .map(|c| c.into())
            .expect("Invalid constant index")
    }

    fn update_fn<F>(&mut self, obj_ref: Reference, function: F)
    where
        F: FnOnce(&mut VmValue),
    {
        if let Some(object_value) = self.runtime.borrow_mut().heap.get_mut(obj_ref) {
            function(object_value)
        }
    }

    fn store_global(&mut self, index: NonlocalIndex, value: Reference) {
        let name = self.resolve_name(index);
        self.current_module().borrow_mut().write(name, value);
    }

    fn store_local(&mut self, index: LocalIndex, value: Reference) {
        let current_frame = self.current_frame_mut();
        if current_frame.locals.len() == *index {
            current_frame.locals.push(value);
        } else {
            current_frame.locals[*index] = value;
        }
    }

    fn load_local(&self, index: LocalIndex) -> Reference {
        self.current_frame().locals[*index]
    }

    fn load_free(&self, index: FreeIndex) -> Reference {
        self.current_frame().function.freevars[*index]
    }

    fn load_global(&mut self, index: NonlocalIndex) -> DomainResult<Reference> {
        let name = self.resolve_name(index).to_owned();

        if let Some(val) = self.current_module().borrow().read(&name) {
            return Ok(val);
        }

        let builtins = self.read_module(&ModuleName::from_segments(&[Dunder::Builtins]));
        if let Some(val) = builtins.borrow().read(&name) {
            return Ok(val);
        }

        let name_ref = self.intern_string(&name);
        Err(Exception::name_error(name_ref))
    }

    fn resolve_name(&self, index: NonlocalIndex) -> &str {
        &self.current_frame().function.code_object.names[*index]
    }

    fn peek(&mut self) -> Reference {
        let frame = self.current_frame_mut();

        if let Some(value) = frame.stack.last() {
            return *value;
        }

        panic!("Empty VM value stack!");
    }

    fn pop(&mut self) -> Reference {
        let frame = self.current_frame_mut();

        if let Some(value) = frame.stack.pop() {
            log_impure(LogLevel::Trace, || {
                println!("After pop:");
                self.dump_frame();
            });
            return value;
        }

        panic!("Empty VM value stack!");
    }

    fn push(&mut self, value: Reference) {
        let frame = self.current_frame_mut();
        frame.stack.push(value);

        log_impure(LogLevel::Trace, || {
            println!("After push:");
            self.dump_frame();
        });
    }

    fn dump_frame(&self) {
        let frame = self.current_frame();

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
    pub fn deref(&self, reference: Reference) -> VmValue {
        match reference {
            Reference::ObjectRef(_) => self
                .runtime
                .borrow()
                .heap
                .get(reference)
                .cloned()
                .expect("Invalid object reference in heap"),
            Reference::Int(i) => VmValue::Int(i),
            Reference::Float(f) => VmValue::Float(f),
        }
    }

    pub fn type_name(&self, obj: &VmValue) -> String {
        match obj {
            VmValue::Object(o) => {
                let class = self.deref(o.class);
                class
                    .as_class()
                    .expect("Invalid class reference")
                    .name()
                    .to_string()
            }
            _ => obj.get_type().to_string(),
        }
    }

    pub fn normalize_vm_ref(&self, r: Reference) -> MemphisValue {
        let value = self.deref(r);
        self.normalize_vm_value(value)
    }

    pub fn normalize_vm_value(&self, value: VmValue) -> MemphisValue {
        match value {
            VmValue::None => MemphisValue::None,
            VmValue::Int(i) => MemphisValue::Int(i),
            VmValue::Float(f) => MemphisValue::Float(f),
            VmValue::Str(s) => MemphisValue::Str(s),
            VmValue::Bool(b) => MemphisValue::Bool(b),
            VmValue::List(i) => {
                let items = i.into_iter().map(|r| self.normalize_vm_ref(r)).collect();
                MemphisValue::List(items)
            }
            VmValue::Tuple(i) => {
                let items = i.items.iter().map(|r| self.normalize_vm_ref(*r)).collect();
                MemphisValue::Tuple(items)
            }
            VmValue::Dict(i) => {
                let items = i
                    .items
                    .iter()
                    .map(|(_, (k, v))| (self.normalize_vm_ref(*k), self.normalize_vm_ref(*v)))
                    .collect();
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
                let class_name = self.normalize_vm_ref(o.class);
                MemphisValue::Object(class_name.to_string())
            }
            VmValue::Code(_) => MemphisValue::Code,
            VmValue::ListIter(_) => MemphisValue::ListIter,
            VmValue::RangeIter(_) => MemphisValue::RangeIter,
            VmValue::TupleIter(_) => MemphisValue::TupleIter,
            VmValue::BuiltinFunction(f) => MemphisValue::BuiltinFunction(f.name().to_string()),
            VmValue::SleepFuture(_) => todo!(),
            VmValue::Exception(e) => {
                let me = e.normalize(self);
                MemphisValue::Exception(me)
            }
        }
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
        } else if let Some(class) = value.as_class() {
            class
                .read(name)
                .ok_or_else(|| Exception::attribute_error(&value.get_type(), name))
        } else {
            unimplemented!()
        }
    }

    /// Resolves an attribute and applies method binding if it is a function.
    pub fn resolve_attr(&mut self, object_ref: Reference, name: &str) -> DomainResult<Reference> {
        let object = self.deref(object_ref);

        let attr_ref = self.resolve_raw_attr(&object, name)?;
        let attr_val = self.deref(attr_ref);

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
            VmValue::Str(s) => s.parse::<i64>().map_err(|_| {
                let msg = self.intern_string("Invalid int literal");
                Exception::value_error(msg)
            }),
            _ => {
                let msg = self.intern_string("Cannot coerce to an int");
                Err(Exception::type_error(msg))
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
    fn peek_value(&mut self) -> VmValue {
        let reference = self.peek();
        self.deref(reference)
    }

    /// Pops and dereferences a value.
    fn pop_value(&mut self) -> VmValue {
        let reference = self.pop();
        self.deref(reference)
    }

    fn push_value(&mut self, value: VmValue) {
        let reference = self.heapify(value);
        self.push(reference);
    }

    fn collect_n(&mut self, n: usize) -> Vec<Reference> {
        let mut items = Vec::with_capacity(n);
        for _ in 0..n {
            items.push(self.pop());
        }
        // Reverse the items since we pop them off in reverse order
        items.reverse();
        items
    }

    fn try_string_multiplication(a: &VmValue, b: &VmValue) -> Option<VmValue> {
        match (a, b) {
            (VmValue::Str(s), VmValue::Int(n)) | (VmValue::Int(n), VmValue::Str(s)) => {
                let result = if *n < 0 {
                    "".to_string()
                } else {
                    s.repeat(*n as usize)
                };
                Some(VmValue::Str(result))
            }
            _ => None,
        }
    }

    fn binary_op<F>(&mut self, opcode: Opcode, op: F, force_float: bool) -> DomainResult<()>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let b = self.pop_value();
        let a = self.pop_value();
        let result = if opcode == Opcode::Mul {
            if let Some(result) = Self::try_string_multiplication(&a, &b) {
                result
            } else {
                self.binary_numeric_op(op, &a, &b, force_float)?
            }
        } else if opcode == Opcode::Div {
            match b {
                VmValue::Int(0) => {
                    let msg = self.intern_string("integer division or modulo by zero");
                    return Err(Exception::div_by_zero_error(msg));
                }
                VmValue::Float(0.0) => {
                    let msg = self.intern_string("float division by zero");
                    return Err(Exception::div_by_zero_error(msg));
                }
                _ => self.binary_numeric_op(op, &a, &b, force_float)?,
            }
        } else {
            self.binary_numeric_op(op, &a, &b, force_float)?
        };

        self.push_value(result);
        Ok(())
    }

    fn cmp_op<F>(&mut self, op: F) -> DomainResult<()>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        let b = self.pop_value();
        let a = self.pop_value();
        let result_ref = self.dynamic_cmp(&a, &b, op)?;
        self.push(result_ref);
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
                let msg = self.intern_string("Unsupported operand types for binary operation");
                return Err(Exception::type_error(msg));
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
                let msg = self.intern_string("Unsupported operand types for comparison");
                return Err(Exception::type_error(msg));
            }
        };

        Ok(self.to_heapified_bool(result))
    }

    fn dynamic_negate(&mut self, value: &VmValue) -> DomainResult<VmValue> {
        let result = match value {
            VmValue::Int(x) => VmValue::Int(-x),
            VmValue::Float(x) => VmValue::Float(-x),
            _ => {
                let msg = self.intern_string("Unsupported operand type for unary '-'");
                return Err(Exception::type_error(msg));
            }
        };

        Ok(result)
    }

    fn value_in_iter(&mut self, needle: VmValue, haystack: VmValue) -> VmResult<bool> {
        let iter = builtins::iter_internal(self, haystack)?;
        loop {
            match builtins::next_internal(self, iter)? {
                Some(item_ref) => {
                    if needle == self.deref(item_ref) {
                        return Ok(true);
                    }
                }
                None => return Ok(false),
            }
        }
    }

    /// Push a new `Frame` to the call stack and immediately execute it to completion, returning
    /// its return value.
    pub fn call(&mut self, frame: Frame) -> VmResult<Reference> {
        let (step_result, _frame) = self.run_frame_to_completion(frame);
        match step_result {
            Completion::Return(val) => Ok(val),
            Completion::Exception(e) => Err(e),
        }
    }

    /// Push a new `Frame` to the call stack and immediately execute it to completion, returning
    /// the frame. Useful for class definitions.
    pub fn call_and_return_frame(&mut self, frame: Frame) -> Frame {
        let (step_result, frame) = self.run_frame_to_completion(frame);
        match step_result {
            Completion::Return(val) => assert_eq!(
                val,
                self.none(),
                "`call_and_return_frame` expects the frame to return None."
            ),
            Completion::Exception(_) => {
                panic!("Unexpected exception during class body");
            }
        }
        frame
    }

    pub fn resume_generator(&mut self, generator: Container<Generator>) -> Option<Reference> {
        let frame = generator.borrow_mut().frame.clone();
        let (step_result, new_frame) = self.run_frame(frame);
        let return_val = match step_result {
            FrameExit::Suspended(Suspension::Yield(val)) => Some(val),
            FrameExit::Suspended(Suspension::Sleep(_) | Suspension::Await(_)) => {
                unimplemented!("Async generators are not currently supported.")
            }
            FrameExit::Completed(Completion::Return(_)) => None,
            FrameExit::Completed(Completion::Exception(_)) => {
                unimplemented!("Exceptions inside generators are not currently supported.")
            }
        };
        generator.borrow_mut().frame = new_frame;

        return_val
    }

    pub fn resume_coroutine(&mut self, coroutine: Container<Coroutine>) -> FrameExit {
        let (step_result, new_frame) = self.run_frame(coroutine.borrow().frame.clone());
        coroutine.borrow_mut().frame = new_frame;
        step_result
    }

    /// Push a frame and run it, capturing the result and returning the frame.
    /// We need to capture the frame when it is finished for creating new Classes and for saving
    /// the state of a Coroutine.
    fn run_frame(&mut self, frame: Frame) -> (FrameExit, Frame) {
        self.call_stack.push(frame);
        while !self.call_stack.top().is_finished() {
            let result = self.step_frame();
            match result {
                StepResult::Continue => continue,
                StepResult::Exit(FrameExit::Completed(Completion::Exception(e))) => {
                    let frame = self.current_frame();
                    if let Some(target_pc) = frame.function.code_object.target_pc_for(frame.pc) {
                        // Jump directly to the handler. This is in place of advancing the PC,
                        // which only happens in execute_opcode for a StepResult::Continue.
                        self.current_frame_mut().pc = target_pc;
                        self.exception_stack.push(e.exception);

                        // Swallow the exception and keep running now that we are in a handler
                        continue;
                    } else {
                        // No handler: propagate
                        let frame = self.call_stack.pop().expect("Empty call stack!");
                        return (FrameExit::Completed(Completion::Exception(e)), frame);
                    }
                }
                StepResult::Exit(e) => {
                    // Frame was suspended
                    let frame = self.call_stack.pop().expect("Empty call stack!");
                    return (e, frame);
                }
            }
        }

        // If we fell out of the loop: frame is finished with no explicit return
        let frame = self.call_stack.pop().expect("Empty call stack!");
        (FrameExit::Completed(Completion::Return(self.none())), frame)
    }

    fn run_frame_to_completion(&mut self, frame: Frame) -> (Completion, Frame) {
        let (reason, frame) = self.run_frame(frame);
        match reason {
            FrameExit::Completed(c) => (c, frame),
            FrameExit::Suspended(_) => panic!("Frame ended in a suspended state."),
        }
    }

    /// Run the next instruction on the top frame in the call stack.
    fn step_frame(&mut self) -> StepResult {
        let frame = self.current_frame();

        // Save this in case we encounter a runtime exception and need to record this info in
        // the stack trace
        self.state.set_line_number(frame.current_line());
        let opcode = frame.current_inst();

        log_impure(LogLevel::Debug, || self.dump_frame());
        log(LogLevel::Debug, || frame.current_inst_annotated());

        self.execute_opcode(opcode)
    }

    pub fn frame_for_function(&self, function: FunctionObject, args: Vec<Reference>) -> Frame {
        let module = self.read_module(&function.code_object.module_name);
        Frame::new(function, args, module)
    }

    pub fn frame_for_method(&self, method: Method, args: Vec<Reference>) -> Frame {
        let mut bound_args = vec![method.receiver];
        bound_args.extend(args);

        let module = self.read_module(&method.function.code_object.module_name);
        Frame::new(method.function, bound_args, module)
    }

    pub fn frame_for_code(&self, code: CodeObject) -> Frame {
        let function = FunctionObject::new(code);
        self.frame_for_function(function, vec![])
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
