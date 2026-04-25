use crate::{
    parser::types::{Statement, StatementKind},
    treewalk::{
        protocols::Iterable, result::Raise, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

use super::{Frame, Pausable, PausableFrame, PausableState, PausableStepResult};

pub struct PausableRunner;

impl PausableRunner {
    /// Run this [`Pausable`] until it reaches a pause event.
    pub fn run_until_pause<P: Pausable>(
        pausable: &mut P,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<TreewalkValue> {
        Self::on_entry(pausable, interpreter);

        let mut result = TreewalkValue::None;
        loop {
            match pausable.context().state() {
                PausableState::Created => {
                    pausable.context_mut().start();
                    continue;
                }
                PausableState::Running => {
                    if pausable.context().frame().is_finished() {
                        Self::on_exit(interpreter);
                        return pausable.finish(result).raise(interpreter);
                    }
                }
                PausableState::InForLoop { index, iterable } => {
                    if pausable.context().frame().is_finished() {
                        let item = iterable
                            .clone()
                            .as_iterator_strict()
                            .raise(interpreter)?
                            .try_next()?;
                        if let Some(item) = item {
                            interpreter.execute_loop_index_assignment(index, item)?;
                            pausable.context_mut().frame_mut().restart();
                        } else {
                            pausable.context_mut().pop();
                            continue;
                        }
                    }
                }
                PausableState::InBlock => {
                    if pausable.context().frame().is_finished() {
                        pausable.context_mut().pop();
                        continue;
                    }
                }
                PausableState::InWhileLoop(condition) => {
                    if pausable.context().frame().is_finished() {
                        if interpreter.evaluate_expr(condition)?.coerce_to_bool() {
                            pausable.context_mut().frame_mut().restart();
                        } else {
                            pausable.context_mut().pop();
                            continue;
                        }
                    }
                }
            }

            match Self::step(pausable, interpreter)? {
                PausableStepResult::NoOp => {}
                PausableStepResult::YieldValue(val) => {
                    Self::on_exit(interpreter);
                    break Ok(val);
                }
                PausableStepResult::Return(val) => {
                    result = val;
                }
                PausableStepResult::Suspend => {
                    Self::on_exit(interpreter);
                    break Ok(TreewalkValue::None);
                }
            };
        }
    }

    /// The default behavior which selects the next [`Statement`] and manually evaluates any
    /// control flow statements. This then calls [`Pausable::handle_step`] to set up any return
    /// values based on whether a control flow structure was encountered.
    fn step<P: Pausable>(
        pausable: &mut P,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<PausableStepResult> {
        let statement = pausable.context_mut().frame().current_statement().clone();

        // Delegate to the common function for control flow
        let encountered_control_flow =
            Self::execute_control_flow_statement(pausable, &statement, interpreter)?;

        if encountered_control_flow {
            return Ok(PausableStepResult::NoOp);
        }

        pausable.execute_statement(interpreter, &statement)
    }

    /// This function manually executes any control flow statements. Any changes are reflected by
    /// invoking [`Container<PausableContext>::push_context`] with the new [`Frame`] and
    /// [`PausableState`].
    ///
    /// This implementation uses a stack-based control flow to remember the next instruction
    /// whenever this coroutine is awaited.
    ///
    /// A boolean is returned indicated whether a control flow statement was encountered.
    fn execute_control_flow_statement<P: Pausable>(
        pausable: &mut P,
        stmt: &Statement,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<bool> {
        match &stmt.kind {
            StatementKind::WhileLoop(cond_ast) => {
                pausable.context_mut().frame_mut().advance_pc();
                pausable.context_mut().push(PausableFrame::new(
                    Frame::new_finished(cond_ast.ast.clone()),
                    PausableState::InWhileLoop(cond_ast.condition.clone()),
                ));

                Ok(true)
            }
            StatementKind::IfElse {
                if_part,
                elif_parts,
                else_part,
            } => {
                pausable.context_mut().frame_mut().advance_pc();
                if let Some(selected_block) =
                    interpreter.select_if_branch(if_part, elif_parts, else_part)?
                {
                    pausable.context_mut().push(PausableFrame::new(
                        Frame::new(selected_block),
                        PausableState::InBlock,
                    ));
                }

                Ok(true)
            }
            StatementKind::ForInLoop {
                index,
                iterable,
                body,
                ..
            } => {
                pausable.context_mut().frame_mut().advance_pc();
                // This now stores the iterator value directly. That depends on clone-stable
                // iterator state because `PausableState` is cloned through `current_state()`.
                // List/Tuple/Range/Generator are safe; remaining iterator variants still need the
                // same shared-state treatment for pausable `for` loops to be correct.
                let iterator = interpreter
                    .evaluate_expr(iterable)?
                    .as_iterable()
                    .raise(interpreter)?;
                pausable.context_mut().push(PausableFrame::new(
                    Frame::new_finished(body.clone()),
                    PausableState::InForLoop {
                        index: index.clone(),
                        iterable: iterator,
                    },
                ));

                Ok(true)
            }
            _ => Ok(false), // only control flow statements are handled here
        }
    }

    /// The default behavior required to perform the necessary context switching when entering a
    /// pausable function.
    fn on_entry<P: Pausable>(pausable: &P, interpreter: &TreewalkInterpreter) {
        interpreter.state.push_local(pausable.scope());
    }

    /// The default behavior required to perform the necessary context switching when exiting a
    /// pausable function.
    fn on_exit(interpreter: &TreewalkInterpreter) {
        interpreter.state.pop_local();
    }
}
