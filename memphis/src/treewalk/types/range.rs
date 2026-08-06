use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
        macros::*,
        protocols::Callable,
        result::Raise,
        utils::{BoundArgs, Parameter, Signature},
    },
};

#[derive(Clone, PartialEq)]
pub struct Range {
    pub start: i64,
    pub stop: i64,
    pub step: i64,
}

impl_typed!(Range, Type::Range);
impl_method_provider!(Range, [NewBuiltin]);
impl_iterable!(RangeIter);

impl Range {
    const DEFAULT_START: i64 = 0;
    const DEFAULT_STOP: i64 = 0;
    const DEFAULT_STEP: i64 = 1;

    fn new(start: i64, stop: i64, step: i64) -> Self {
        Self { start, stop, step }
    }

    fn with_stop(stop: i64) -> Self {
        Self::new(Self::DEFAULT_START, stop, Self::DEFAULT_STEP)
    }
}

impl Default for Range {
    fn default() -> Self {
        Self {
            start: Self::DEFAULT_START,
            stop: Self::DEFAULT_STOP,
            step: Self::DEFAULT_STEP,
        }
    }
}

impl IntoIterator for Range {
    type Item = TreewalkValue;
    type IntoIter = RangeIter;

    fn into_iter(self) -> Self::IntoIter {
        RangeIter::new(self)
    }
}

#[derive(Clone)]
pub struct RangeIter(Container<Range>);

impl RangeIter {
    fn new(range: Range) -> Self {
        RangeIter(Container::new(range))
    }
}

impl Iterator for RangeIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        let mut range = self.0.borrow_mut();
        if range.start < range.stop {
            let result = range.start;
            // Modify the start value in the range itself to prep the state for the next time
            // `next` is called.
            range.start += range.step;
            Some(TreewalkValue::Int(result))
        } else {
            None
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::required("start_or_stop").positional_only(),
            Parameter::optional_without_default("stop").positional_only(),
            Parameter::optional("step", TreewalkValue::Int(1)).positional_only(),
        ])
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        let range = if let Some(stop) = args.get_optional("stop") {
            let start = args.get("start_or_stop").as_int().raise(interpreter)?;
            let stop = stop.as_int().raise(interpreter)?;
            let step = args.get("step").as_int().raise(interpreter)?;
            Range::new(start, stop, step)
        } else {
            let stop = args.get("start_or_stop").as_int().raise(interpreter)?;
            Range::with_stop(stop)
        };

        Ok(TreewalkValue::Range(range))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
