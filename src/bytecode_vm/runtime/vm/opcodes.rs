use crate::{
    bytecode_vm::{
        compiler::Opcode,
        runtime::{
            import_utils::build_module_chain,
            modules::builtins,
            types::{
                Coroutine, Dict, Exception, FunctionObject, Generator, List, Method, Object, Tuple,
            },
            BuiltinFunction, Frame, Reference, StepResult,
        },
        VirtualMachine, VmValue,
    },
    core::Container,
    domain::{Dunder, FunctionType, ModuleName},
};

macro_rules! step {
    ($vm:expr, $e:expr) => {
        match $e {
            Ok(t) => t,
            Err(e) => return $vm.raise_step(e),
        }
    };
}

macro_rules! step_raised {
    ($e:expr) => {
        match $e {
            Ok(t) => t,
            Err(e) => return StepResult::Exception(e),
        }
    };
}

impl VirtualMachine {
    pub fn execute_opcode(&mut self, opcode: Opcode) -> StepResult {
        match opcode {
            Opcode::Add => {
                let result = self.binary_op(opcode, |a, b| a + b, false);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for +".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::Sub => {
                let result = self.binary_op(opcode, |a, b| a - b, false);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for -".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::Mul => {
                let result = self.binary_op(opcode, |a, b| a * b, false);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for *".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::Div => {
                let result = self.binary_op(opcode, |a, b| a / b, true);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for /".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::Eq => {
                let right = self.pop_value();
                let left = self.pop_value();
                self.push(self.to_heapified_bool(left == right));
            }
            Opcode::Ne => {
                let right = self.pop_value();
                let left = self.pop_value();
                self.push(self.to_heapified_bool(left != right));
            }
            Opcode::Is => {
                // For referential identity, we compare the Reference objects directly.
                let right = self.pop();
                let left = self.pop();
                self.push(self.to_heapified_bool(left == right));
            }
            Opcode::IsNot => {
                // For referential identity, we compare the Reference objects directly.
                let right = self.pop();
                let left = self.pop();
                self.push(self.to_heapified_bool(left != right));
            }
            Opcode::LessThan => {
                let result = self.cmp_op(|a, b| a < b);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for <".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::LessThanOrEq => {
                let result = self.cmp_op(|a, b| a <= b);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for <=".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::GreaterThan => {
                let result = self.cmp_op(|a, b| a > b);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for >".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::GreaterThanOrEq => {
                let result = self.cmp_op(|a, b| a >= b);
                if result.is_err() {
                    let msg = VmValue::String("Unsupported operand types for >=".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::In => {
                let haystack = self.pop_value();
                let needle = self.pop_value();

                let in_result = step_raised!(self.value_in_iter(needle, haystack));
                self.push(self.to_heapified_bool(in_result));
            }
            Opcode::NotIn => {
                let haystack = self.pop_value();
                let needle = self.pop_value();

                let in_result = !step_raised!(self.value_in_iter(needle, haystack));
                self.push(self.to_heapified_bool(in_result));
            }
            Opcode::UnaryNegative => {
                let value = self.pop_value();
                let result = step!(self, self.dynamic_negate(&value));
                self.push_value(result);
            }
            Opcode::UnaryNot => {
                let right = self.pop_value().to_boolean();
                self.push(self.to_heapified_bool(!right));
            }
            Opcode::UnaryInvert => {
                let right = self.pop_value().as_integer();

                if let Some(right) = right {
                    self.push(Reference::Int(!right));
                } else {
                    let msg = VmValue::String("Unsupported operand type for '~'".to_string());
                    let exp = Exception::type_error(self.heapify(msg));
                    return self.raise_step(exp);
                }
            }
            Opcode::LoadConst(index) => {
                // After loading a constant for the first time, it becomes an object managed by
                // the heap like any other object.
                let value = self.read_constant(index);
                self.push_value(value);
            }
            Opcode::StoreFast(index) => {
                let reference = self.pop();
                self.store_local(index, reference);
            }
            Opcode::StoreGlobal(index) => {
                let reference = self.pop();
                self.store_global(index, reference);
            }
            Opcode::LoadFast(index) => {
                let reference = self.load_local(index);
                self.push(reference);
            }
            Opcode::LoadFree(index) => {
                let reference = self.load_free(index);
                self.push(reference);
            }
            Opcode::LoadGlobal(index) => {
                let reference = step!(self, self.load_global(index));
                self.push(reference);
            }
            Opcode::LoadAttr(index) => {
                let attr_name = self.resolve_name(index).to_owned();
                let object_ref = self.pop();

                let bound_attr = step!(self, self.resolve_attr(object_ref, &attr_name));
                self.push(bound_attr);
            }
            Opcode::SetAttr(index) => {
                let value = self.pop();
                let obj_ref = self.pop();

                let name = self.resolve_name(index).to_owned();
                self.update_fn(obj_ref, |object_value| {
                    let VmValue::Object(object) = object_value else {
                        todo!()
                    };
                    object.write(&name, value);
                });
            }
            Opcode::LoadBuildClass => {
                self.push_value(VmValue::BuiltinFunction(BuiltinFunction::new(
                    "load_build_class",
                    builtins::build_class,
                )));
            }
            Opcode::BuildList(n) => {
                let items = self.collect_n(n);
                self.push_value(VmValue::List(List::new(items)));
            }
            Opcode::BuildTuple(n) => {
                let items = self.collect_n(n);
                self.push_value(VmValue::Tuple(Tuple::new(items)));
            }
            Opcode::BuildMap(n) => {
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    let value = self.pop();
                    let key = self.pop();
                    items.push((key, value));
                }
                items.reverse(); // to preserve left-to-right source order
                self.push_value(VmValue::Dict(Dict::new(items)));
            }
            Opcode::GetIter => {
                let obj = self.pop_value();
                let iterator_ref = step_raised!(builtins::iter_internal(self, obj));
                self.push(iterator_ref);
            }
            Opcode::ForIter(offset) => {
                // Don’t pop, we need the iterator on the stack for the next iteration
                let iter_ref = self.peek();
                let next_ref = step_raised!(builtins::next_internal(self, iter_ref));

                if let Some(next_ref) = next_ref {
                    // Iterator stays, value now lives above it
                    self.push(next_ref);
                } else {
                    // Pop the iterator only if exhausted
                    let _ = self.pop();
                    self.call_stack.jump_to_offset(offset);
                }
            }
            Opcode::Jump(offset) => {
                self.call_stack.jump_to_offset(offset);
            }
            Opcode::JumpIfFalse(offset) => {
                if !self.peek_value().to_boolean() {
                    self.call_stack.jump_to_offset(offset);
                }
            }
            Opcode::JumpIfTrue(offset) => {
                if self.peek_value().to_boolean() {
                    self.call_stack.jump_to_offset(offset);
                }
            }
            Opcode::PopTop => {
                let _ = self.pop();
            }
            Opcode::DupTop => {
                let x = self.peek();
                self.push(x);
            }
            Opcode::RotThree => {
                // Before:
                // c <- TOS
                // b
                // a
                //
                // After:
                // b <- TOS
                // a
                // c
                let c = self.pop();
                let b = self.pop();
                let a = self.pop();
                self.push(c);
                self.push(a);
                self.push(b);
            }
            Opcode::MakeFunction => {
                let code_value = self.pop_value();
                let code = code_value
                    .as_code()
                    .expect("MAKE_FUNCTION expected a code object on the stack");
                let function = FunctionObject::new(code.clone());
                self.push_value(VmValue::Function(function));
            }
            Opcode::MakeClosure(num_free) => {
                let freevars = (0..num_free).map(|_| self.pop()).collect::<Vec<_>>();
                let code_value = self.pop_value();
                let code = code_value
                    .as_code()
                    .expect("MAKE_CLOSURE expected a code object on the stack");
                let function = FunctionObject::new_with_free(code.clone(), freevars);
                self.push_value(VmValue::Function(function));
            }
            Opcode::Call(argc) => {
                let args = (0..argc).map(|_| self.pop()).collect::<Vec<_>>();
                let callable_ref = self.pop();
                let callable = self.deref(callable_ref);

                match callable {
                    VmValue::BuiltinFunction(builtin) => {
                        let reference = step_raised!(builtin.call(self, args));
                        self.push(reference);
                    }
                    VmValue::Function(ref function) => {
                        let module =
                            step!(self, self.read_module(&function.code_object.module_name));
                        let frame = Frame::new(function.clone(), args, module);
                        match function.function_type() {
                            FunctionType::Regular => {
                                let return_val_ref = step_raised!(self.call(frame));
                                self.push(return_val_ref);
                            }
                            FunctionType::Generator => {
                                let generator = Container::new(Generator::new(frame));
                                self.push_value(VmValue::Generator(generator));
                            }
                            FunctionType::Async => {
                                let coroutine = Container::new(Coroutine::new(frame));
                                self.push_value(VmValue::Coroutine(coroutine));
                            }
                        }
                    }
                    VmValue::Method(ref method) => {
                        let module = step!(
                            self,
                            self.read_module(&method.function.code_object.module_name)
                        );
                        let frame = Frame::from_method(method.clone(), args, module);
                        let return_val_ref = step_raised!(self.call(frame));
                        self.push(return_val_ref);
                    }
                    VmValue::Class(ref class) => {
                        let object = VmValue::Object(Object::new(callable_ref));
                        let reference = self.heapify(object);

                        if let Some(init_method) = class.read(Dunder::Init) {
                            let init_value = self.deref(init_method);
                            let init_fn = init_value.as_function();

                            let init_fn = if let Some(init_fn) = init_fn {
                                init_fn
                            } else {
                                let msg = VmValue::String("Expected a function".to_string());
                                let exp = Exception::type_error(self.heapify(msg));
                                return self.raise_step(exp);
                            };
                            let method = Method::new(reference, init_fn.clone());

                            // The object reference must be on the stack for
                            // after the constructor executes.
                            self.push(reference);

                            let module = step!(
                                self,
                                self.read_module(&method.function.code_object.module_name)
                            );
                            let frame = Frame::from_method(method, args, module);
                            let _ = step_raised!(self.call(frame));
                        } else {
                            self.push(reference);
                        }
                    }
                    _ => unimplemented!(),
                };
            }
            Opcode::ReturnValue => {
                let return_val_ref = self.pop();
                return StepResult::Return(return_val_ref);
            }
            Opcode::YieldValue => {
                let yield_val_ref = self.pop();
                self.call_stack.advance_pc();
                return StepResult::Yield(yield_val_ref);
            }
            Opcode::YieldFrom => {
                if !self.current_frame().has_subgenerator() {
                    // First time hitting this instruction: pop the iterable and store it
                    let iterable = self.pop_value();
                    let iterator_ref = step_raised!(builtins::iter_internal(self, iterable));
                    let frame = self.current_frame_mut();
                    frame.set_subgenerator(iterator_ref);
                }

                // Extract iterator_ref in a separate scope to avoid borrow overlap
                let iterator_ref = match self.current_frame().subgenerator_ref() {
                    Some(r) => r,
                    None => unreachable!("YieldFrom without a sub-generator"),
                };

                // Actually try the next() call
                let next_result = step_raised!(builtins::next_internal(self, iterator_ref));
                match next_result {
                    Some(val) => {
                        return StepResult::Yield(val); // yield and don't advance PC
                    }
                    None => {
                        // Sub-generator is done, clean up and continue
                        let frame = self.current_frame_mut();
                        frame.clear_subgenerator();
                        self.call_stack.advance_pc(); // advance past YieldFrom
                        return StepResult::Continue;
                    }
                }
            }
            Opcode::Await => {
                let value = self.pop_value();

                self.call_stack.advance_pc();
                match value {
                    VmValue::SleepFuture(duration) => {
                        return StepResult::Sleep(duration);
                    }
                    VmValue::Coroutine(co) => {
                        return StepResult::Await(co.clone());
                    }
                    _ => {
                        let msg = VmValue::String("Expected awaitable".to_string());
                        let exp = Exception::type_error(self.heapify(msg));
                        return self.raise_step(exp);
                    }
                }
            }
            Opcode::ImportAll => todo!(),
            Opcode::ImportName(index) => {
                let name = self.resolve_name(index).to_owned();
                let module_name = ModuleName::from_dotted(&name);
                let inner_module = step_raised!(self.read_or_load_module(&module_name));
                let inner_module_ref = self.heapify(VmValue::Module(inner_module));

                let outer_module_ref = step!(
                    self,
                    build_module_chain(self, &module_name, inner_module_ref)
                );
                self.push(outer_module_ref);
            }
            Opcode::ImportFrom(index) => {
                let name = self.resolve_name(index).to_owned();
                let module_name = ModuleName::from_dotted(&name);
                let inner_module = step_raised!(self.read_or_load_module(&module_name));
                let inner_module_ref = self.heapify(VmValue::Module(inner_module));
                self.push(inner_module_ref);
            }
            // This is in an internal error that indicates a jump offset was not properly set
            // by the compiler. This opcode should not leak into the VM.
            Opcode::Placeholder => panic!("Placeholder emitted in bytecode"),
        }

        StepResult::Continue
    }
}
