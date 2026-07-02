macro_rules! none {
    () => {
        crate::domain::MemphisValue::None
    };
}

macro_rules! int {
    ($val:expr) => {
        $crate::domain::MemphisValue::Int($val)
    };
}

macro_rules! float {
    ($val:expr) => {
        $crate::domain::MemphisValue::Float($val)
    };
}

macro_rules! str {
    ($val:expr) => {
        $crate::domain::MemphisValue::Str($val.to_string())
    };
}

macro_rules! bool {
    ($val:expr) => {
        $crate::domain::MemphisValue::Bool($val)
    };
}

macro_rules! list {
    ($($expr:expr),* $(,)?) => {
        crate::domain::MemphisValue::List(vec![
            $($expr),*
        ])
    };
}

macro_rules! tuple {
    ($($expr:expr),* $(,)?) => {
        crate::domain::MemphisValue::Tuple(vec![
            $($expr),*
        ])
    };
}

macro_rules! range {
    ($stop:expr) => {
        $crate::domain::MemphisValue::Range(0, $stop, 1)
    };
    ($start:expr, $stop:expr) => {
        $crate::domain::MemphisValue::Range($start, $stop, 1)
    };
    ($start:expr, $stop:expr, $step:expr) => {
        $crate::domain::MemphisValue::Range($start, $stop, $step)
    };
}

macro_rules! dict {
    ({ $($key:expr => $value:expr),* $(,)? }) => {
        crate::domain::MemphisValue::Dict(
            vec![
                $(($key, $value)),*
            ]
        )
    };
}

pub(crate) use bool;
pub(crate) use dict;
pub(crate) use float;
pub(crate) use int;
pub(crate) use list;
pub(crate) use none;
pub(crate) use range;
pub(crate) use str;
pub(crate) use tuple;
