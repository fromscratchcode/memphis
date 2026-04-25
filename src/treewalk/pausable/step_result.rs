use crate::treewalk::TreewalkValue;

pub enum StepResult {
    Continue,
    Exit(FrameExit),
}

#[derive(Debug)]
pub enum FrameExit {
    Completed(Completion),
    Suspended(Suspension),
}

#[derive(Debug)]
// We are chosing not to model Exceptions as part of this, because those use the TreewalkResult<_>
// system.
pub enum Completion {
    Return(TreewalkValue),
}

#[derive(Debug)]
pub enum Suspension {
    Yield(TreewalkValue),
    Await,
    Sleep,
}
