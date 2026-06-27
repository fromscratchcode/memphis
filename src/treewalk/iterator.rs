use crate::treewalk::{
    protocols::{Iterable, NextResult},
    type_system::CloneableIterable,
    TreewalkResult, TreewalkValue,
};

impl Iterable for Box<dyn CloneableIterable> {
    /// This should surface any `StopIteration` errors. Use `Iterator` to swallow them.
    fn try_next(&mut self) -> TreewalkResult<NextResult> {
        self.as_mut().try_next()
    }
}

pub fn collect(iter: Box<dyn CloneableIterable>) -> TreewalkResult<Vec<TreewalkValue>> {
    let mut l = vec![];
    for_each_mut(iter, &mut |val| {
        l.push(val);
        Ok(())
    })?;
    Ok(l)
}

pub fn count(iter: Box<dyn CloneableIterable>) -> TreewalkResult<usize> {
    let mut c = 0;
    for_each_mut(iter, &mut |_| {
        c += 1;
        Ok(())
    })?;
    Ok(c)
}

pub fn any<F>(mut iter: Box<dyn CloneableIterable>, f: F) -> TreewalkResult<bool>
where
    F: Fn(TreewalkValue) -> bool,
{
    while let NextResult::Yielded(val) = iter.try_next()? {
        if f(val) {
            return Ok(true);
        }
    }

    Ok(false)
}

pub enum LoopControl {
    Continue,
    Break,
}

pub fn try_for_each_mut<F>(mut iter: Box<dyn CloneableIterable>, f: &mut F) -> TreewalkResult<()>
where
    F: FnMut(TreewalkValue) -> TreewalkResult<LoopControl>,
{
    while let NextResult::Yielded(val) = iter.try_next()? {
        match f(val)? {
            LoopControl::Continue => {}
            LoopControl::Break => break,
        }
    }
    Ok(())
}

pub fn for_each_mut<F>(iter: Box<dyn CloneableIterable>, f: &mut F) -> TreewalkResult<()>
where
    F: FnMut(TreewalkValue) -> TreewalkResult<()>,
{
    try_for_each_mut(iter, &mut |v| {
        f(v)?;
        Ok(LoopControl::Continue)
    })
}
