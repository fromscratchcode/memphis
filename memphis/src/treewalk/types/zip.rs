use crate::{
    domain::{Dunder, Type},
    treewalk::{
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
        iterator::count,
        macros::*,
        protocols::{Callable, Iterable, NextResult},
        result::Raise,
        type_system::CloneableIterable,
        types::{Exception, Tuple},
        utils::{BoundArgs, Parameter, Signature},
    },
};

pub struct ZipIterator(Vec<Box<dyn CloneableIterable>>);

impl Clone for ZipIterator {
    /// This works similar to the [dyn-clone](https://github.com/dtolnay/dyn-clone) crate.
    fn clone(&self) -> Self {
        Self(self.0.iter().map(safe_clone).collect())
    }
}

impl_typed!(ZipIterator, Type::Zip);
impl_method_provider!(ZipIterator, [NewBuiltin]);

impl ZipIterator {
    pub fn new(items: Vec<Box<dyn CloneableIterable>>) -> Self {
        Self(items)
    }

    fn lengths(&self) -> TreewalkResult<Vec<usize>> {
        self.0
            .iter()
            .map(|i| count(safe_clone(i)))
            .collect::<TreewalkResult<Vec<usize>>>()
    }
}

impl Iterable for ZipIterator {
    // We cannot use the boilerplate impl_iterable! here because some of the composite iterators
    // may fail, not swallow them the way Iterator::next does.
    fn try_next(&mut self) -> TreewalkResult<NextResult> {
        // Python advances child iterators left-to-right for each next() call.
        // If one iterator is exhausted, earlier iterators may already have advanced,
        // but later iterators should not be touched. This is true regardless how many times next
        // is called.
        let mut results = vec![];
        for i in self.0.iter_mut() {
            let next = i.try_next()?;
            match next {
                NextResult::Exhausted(val) => return Ok(NextResult::Exhausted(val)),
                NextResult::Yielded(val) => results.push(val),
            }
        }

        Ok(NextResult::Yielded(TreewalkValue::Tuple(Tuple::new(
            results,
        ))))
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn signature(&self) -> Signature {
        Signature::new([
            Parameter::required("cls").positional_only(),
            Parameter::optional("strict", TreewalkValue::Bool(false)).keyword_only(),
        ])
        .with_varargs("iterables")
    }

    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: BoundArgs,
    ) -> TreewalkResult<TreewalkValue> {
        // The default behavior will stop zipping when the shortest iterator is exhausted,
        // which matches default behavior from Python. Using strict=True causes this to throw an
        // exception instead.
        let iters = args
            .get_varargs("iterables")
            .items()
            .iter()
            .map(|a| a.as_iterator())
            .collect::<DomainResult<Vec<Box<dyn CloneableIterable>>>>()
            .raise(interpreter)?;

        let zip = ZipIterator::new(iters);

        if args.get("strict") == &TreewalkValue::Bool(true) {
            let lengths = zip.lengths()?;
            let all_equal = lengths.is_empty() || lengths.iter().all(|&x| x == lengths[0]);

            if !all_equal {
                return Exception::runtime_error().raise(interpreter);
            }
        }

        Ok(TreewalkValue::Zip(zip))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

#[allow(clippy::borrowed_box)]
fn safe_clone(i: &Box<dyn CloneableIterable>) -> Box<dyn CloneableIterable> {
    (**i).clone_box()
}
