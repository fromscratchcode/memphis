use crate::{
    bytecode_vm::{
        compiler::Opcode,
        result::Raise,
        runtime::{
            import_utils::build_module_chain,
            modules::builtins,
            types::{
                Coroutine, Dict, Exception, FunctionObject, Generator, List, Method, Object, Tuple,
            },
            BuiltinFunction, Frame, Reference, StepResult,
        },
        VirtualMachine, VmResult, VmValue,
    },
    core::Container,
    domain::{Dunder, FunctionType, ModuleName},
};

impl VirtualMachine {
    pub fn execute_opcode(&mut self, opcode: Opcode) -> VmResult<StepResult> {
        match opcode {
            Opcode::Add => {
                self.binary_op(opcode, |a, b| a + b, false)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for +".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::Sub => {
                self.binary_op(opcode, |a, b| a - b, false)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for -".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::Mul => {
                self.binary_op(opcode, |a, b| a * b, false)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for *".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::Div => {
                self.binary_op(opcode, |a, b| a / b, true)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for /".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::Eq => {
                let right = self.pop_value().raise(self)?;
                let left = self.pop_value().raise(self)?;
                self.push(self.to_heapified_bool(left == right))
                    .raise(self)?;
            }
            Opcode::Ne => {
                let right = self.pop_value().raise(self)?;
                let left = self.pop_value().raise(self)?;
                self.push(self.to_heapified_bool(left != right))
                    .raise(self)?;
            }
            Opcode::Is => {
                // For referential identity, we compare the Reference objects directly.
                let right = self.pop().raise(self)?;
                let left = self.pop().raise(self)?;
                self.push(self.to_heapified_bool(left == right))
                    .raise(self)?;
            }
            Opcode::IsNot => {
                // For referential identity, we compare the Reference objects directly.
                let right = self.pop().raise(self)?;
                let left = self.pop().raise(self)?;
                self.push(self.to_heapified_bool(left != right))
                    .raise(self)?;
            }
            Opcode::LessThan => {
                self.cmp_op(|a, b| a < b)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for <".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::LessThanOrEq => {
                self.cmp_op(|a, b| a <= b)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for <=".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::GreaterThan => {
                self.cmp_op(|a, b| a > b)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for >".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::GreaterThanOrEq => {
                self.cmp_op(|a, b| a >= b)
                    .map_err(|_| {
                        let msg = VmValue::String("Unsupported operand types for >=".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
            }
            Opcode::In => {
                let haystack = self.pop_value().raise(self)?;
                let needle = self.pop_value().raise(self)?;

                let in_result = self.value_in_iter(needle, haystack)?;
                self.push(self.to_heapified_bool(in_result)).raise(self)?;
            }
            Opcode::NotIn => {
                let haystack = self.pop_value().raise(self)?;
                let needle = self.pop_value().raise(self)?;

                let in_result = !self.value_in_iter(needle, haystack)?;
                self.push(self.to_heapified_bool(in_result)).raise(self)?;
            }
            Opcode::UnaryNegative => {
                let value = self.pop_value().raise(self)?;
                let result = self.dynamic_negate(&value).raise(self)?;
                self.push_value(result).raise(self)?;
            }
            Opcode::UnaryNot => {
                let right = self.pop_value().raise(self)?.to_boolean();
                self.push(self.to_heapified_bool(!right)).raise(self)?;
            }
            Opcode::UnaryInvert => {
                let right = self
                    .pop_value()
                    .raise(self)?
                    .as_integer()
                    .ok_or_else(|| {
                        let msg = VmValue::String("Unsupported operand type for '~'".to_string());
                        Exception::type_error(self.heapify(msg))
                    })
                    .raise(self)?;
                self.push(Reference::Int(!right)).raise(self)?;
            }
            Opcode::LoadConst(index) => {
                // After loading a constant for the first time, it becomes an object managed by
                // the heap like any other object.
                let value = self.read_constant(index).raise(self)?;
                self.push_value(value).raise(self)?;
            }
            Opcode::StoreFast(index) => {
                let reference = self.pop().raise(self)?;
                self.store_local(index, reference).raise(self)?;
            }
            Opcode::StoreGlobal(index) => {
                let reference = self.pop().raise(self)?;
                self.store_global(index, reference).raise(self)?;
            }
            Opcode::LoadFast(index) => {
                let reference = self.load_local(index).raise(self)?;
                self.push(reference).raise(self)?;
            }
            Opcode::LoadFree(index) => {
                let reference = self.load_free(index).raise(self)?;
                self.push(reference).raise(self)?;
            }
            Opcode::LoadGlobal(index) => {
                let reference = self.load_global(index).raise(self)?;
                self.push(reference).raise(self)?;
            }
            Opcode::LoadAttr(index) => {
                let attr_name = self.resolve_name(index).raise(self)?.to_owned();
                let object_ref = self.pop().raise(self)?;

                let bound_attr = self.resolve_attr(object_ref, &attr_name).raise(self)?;
                self.push(bound_attr).raise(self)?;
            }
            Opcode::SetAttr(index) => {
                let value = self.pop().raise(self)?;
                let obj_ref = self.pop().raise(self)?;

                let name = self.resolve_name(index).raise(self)?.to_owned();
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
                )))
                .raise(self)?;
            }
            Opcode::BuildList(n) => {
                let items = self.collect_n(n).raise(self)?;
                self.push_value(VmValue::List(List::new(items)))
                    .raise(self)?;
            }
            Opcode::BuildTuple(n) => {
                let items = self.collect_n(n).raise(self)?;
                self.push_value(VmValue::Tuple(Tuple::new(items)))
                    .raise(self)?;
            }
            Opcode::BuildMap(n) => {
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    let value = self.pop().raise(self)?;
                    let key = self.pop().raise(self)?;
                    items.push((key, value));
                }
                items.reverse(); // to preserve left-to-right source order
                self.push_value(VmValue::Dict(Dict::new(items)))
                    .raise(self)?;
            }
            Opcode::GetIter => {
                let obj = self.pop_value().raise(self)?;
                let iterator_ref = builtins::iter_internal(self, obj)?;
                self.push(iterator_ref).raise(self)?;
            }
            Opcode::ForIter(offset) => {
                // Don’t pop, we need the iterator on the stack for the next iteration
                let iter_ref = self.peek().raise(self)?;
                let next_ref = builtins::next_internal(self, iter_ref)?;

                if let Some(next_ref) = next_ref {
                    // Iterator stays, value now lives above it
                    self.push(next_ref).raise(self)?;
                } else {
                    // Pop the iterator only if exhausted
                    let _ = self.pop().raise(self)?;
                    self.call_stack.jump_to_offset(offset).raise(self)?;
                }
            }
            Opcode::Jump(offset) => {
                self.call_stack.jump_to_offset(offset).raise(self)?;
            }
            Opcode::JumpIfFalse(offset) => {
                if !self.peek_value().raise(self)?.to_boolean() {
                    self.call_stack.jump_to_offset(offset).raise(self)?;
                }
            }
            Opcode::JumpIfTrue(offset) => {
                if self.peek_value().raise(self)?.to_boolean() {
                    self.call_stack.jump_to_offset(offset).raise(self)?;
                }
            }
            Opcode::PopTop => {
                let _ = self.pop().raise(self)?;
            }
            Opcode::DupTop => {
                let x = self.peek().raise(self)?;
                self.push(x).raise(self)?;
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
                let c = self.pop().raise(self)?;
                let b = self.pop().raise(self)?;
                let a = self.pop().raise(self)?;
                self.push(c).raise(self)?;
                self.push(a).raise(self)?;
                self.push(b).raise(self)?;
            }
            Opcode::MakeFunction => {
                let code_value = self.pop_value().raise(self)?;
                let code = code_value
                    .as_code()
                    .ok_or_else(Exception::runtime_error)
                    .raise(self)?;
                let function = FunctionObject::new(code.clone());
                self.push_value(VmValue::Function(function)).raise(self)?;
            }
            Opcode::MakeClosure(num_free) => {
                let freevars = (0..num_free)
                    .map(|_| self.pop())
                    .collect::<Result<Vec<_>, _>>()
                    .raise(self)?;
                let code_value = self.pop_value().raise(self)?;
                let code = code_value
                    .as_code()
                    .ok_or_else(Exception::runtime_error)
                    .raise(self)?;
                let function = FunctionObject::new_with_free(code.clone(), freevars);
                self.push_value(VmValue::Function(function)).raise(self)?;
            }
            Opcode::Call(argc) => {
                let args = (0..argc)
                    .map(|_| self.pop())
                    .collect::<Result<Vec<_>, _>>()
                    .raise(self)?;
                let callable_ref = self.pop().raise(self)?;
                let callable = self.deref(callable_ref).raise(self)?;

                match callable {
                    VmValue::BuiltinFunction(builtin) => {
                        let reference = builtin.call(self, args)?;
                        self.push(reference).raise(self)?;
                    }
                    VmValue::Function(ref function) => {
                        let module = self
                            .read_module(&function.code_object.module_name)
                            .raise(self)?;
                        let frame = Frame::new(function.clone(), args, module);
                        match function.function_type() {
                            FunctionType::Regular => {
                                let return_val_ref = self.call(frame)?;
                                self.push(return_val_ref).raise(self)?;
                            }
                            FunctionType::Generator => {
                                let generator = Container::new(Generator::new(frame));
                                self.push_value(VmValue::Generator(generator)).raise(self)?
                            }
                            FunctionType::Async => {
                                let coroutine = Container::new(Coroutine::new(frame));
                                self.push_value(VmValue::Coroutine(coroutine)).raise(self)?;
                            }
                        }
                    }
                    VmValue::Method(ref method) => {
                        let module = self
                            .read_module(&method.function.code_object.module_name)
                            .raise(self)?;
                        let frame = Frame::from_method(method.clone(), args, module);
                        let return_val_ref = self.call(frame)?;
                        self.push(return_val_ref).raise(self)?;
                    }
                    VmValue::Class(ref class) => {
                        let object = VmValue::Object(Object::new(callable_ref));
                        let reference = self.heapify(object);

                        if let Some(init_method) = class.read(Dunder::Init) {
                            let init_value = self.deref(init_method).raise(self)?;
                            let init_fn = init_value
                                .as_function()
                                .ok_or_else(|| {
                                    let msg = VmValue::String("Expected a function".to_string());
                                    Exception::type_error(self.heapify(msg))
                                })
                                .raise(self)?;
                            let method = Method::new(reference, init_fn.clone());

                            // The object reference must be on the stack for
                            // after the constructor executes.
                            self.push(reference).raise(self)?;

                            let module = self
                                .read_module(&method.function.code_object.module_name)
                                .raise(self)?;
                            let frame = Frame::from_method(method, args, module);
                            let _ = self.call(frame)?;
                        } else {
                            self.push(reference).raise(self)?;
                        }
                    }
                    _ => unimplemented!(),
                };
            }
            Opcode::ReturnValue => {
                let return_val_ref = self.pop().raise(self)?;
                return Ok(StepResult::Return(return_val_ref));
            }
            Opcode::YieldValue => {
                let yield_val_ref = self.pop().raise(self)?;
                self.call_stack.advance_pc().raise(self)?;
                return Ok(StepResult::Yield(yield_val_ref));
            }
            Opcode::YieldFrom => {
                if !self.current_frame().raise(self)?.has_subgenerator() {
                    // First time hitting this instruction: pop the iterable and store it
                    let iterable = self.pop_value().raise(self)?;
                    let iterator_ref = builtins::iter_internal(self, iterable)?;
                    let frame = match self.current_frame_mut() {
                        Ok(f) => f,
                        Err(e) => e.raise(self)?,
                    };
                    frame.set_subgenerator(iterator_ref);
                }

                // Extract iterator_ref in a separate scope to avoid borrow overlap
                let iterator_ref = match self.current_frame().raise(self)?.subgenerator_ref() {
                    Some(r) => r,
                    None => unreachable!("YieldFrom without a sub-generator"),
                };

                // Actually try the next() call
                match builtins::next_internal(self, iterator_ref)? {
                    Some(val) => {
                        return Ok(StepResult::Yield(val)); // yield and don't advance PC
                    }
                    None => {
                        // Sub-generator is done, clean up and continue
                        let frame = match self.current_frame_mut() {
                            Ok(f) => f,
                            Err(e) => e.raise(self)?,
                        };
                        frame.clear_subgenerator();
                        self.call_stack.advance_pc().raise(self)?; // advance past YieldFrom
                        return Ok(StepResult::Continue);
                    }
                }
            }
            Opcode::Await => {
                let value = self.pop_value().raise(self)?;

                self.call_stack.advance_pc().raise(self)?;
                match value {
                    VmValue::SleepFuture(duration) => {
                        return Ok(StepResult::Sleep(duration));
                    }
                    VmValue::Coroutine(co) => {
                        return Ok(StepResult::Await(co.clone()));
                    }
                    _ => {
                        let msg = VmValue::String("Expected awaitable".to_string());
                        return Exception::type_error(self.heapify(msg)).raise(self);
                    }
                }
            }
            Opcode::ImportAll => todo!(),
            Opcode::ImportName(index) => {
                let name = self.resolve_name(index).raise(self)?.to_owned();
                let module_name = ModuleName::from_dotted(&name);
                let inner_module = self.read_or_load_module(&module_name)?;
                let inner_module_ref = self.heapify(VmValue::Module(inner_module));

                let outer_module_ref =
                    build_module_chain(self, &module_name, inner_module_ref).raise(self)?;
                self.push(outer_module_ref).raise(self)?;
            }
            Opcode::ImportFrom(index) => {
                let name = self.resolve_name(index).raise(self)?.to_owned();
                let module_name = ModuleName::from_dotted(&name);
                let inner_module = self.read_or_load_module(&module_name)?;
                let inner_module_ref = self.heapify(VmValue::Module(inner_module));
                self.push(inner_module_ref).raise(self)?;
            }
            Opcode::Halt => {
                return Ok(StepResult::Halt);
            }
            // This is in an internal error that indicates a jump offset was not properly set
            // by the compiler. This opcode should not leak into the VM.
            Opcode::Placeholder => return Exception::runtime_error().raise(self),
        }

        Ok(StepResult::Continue)
    }
}
