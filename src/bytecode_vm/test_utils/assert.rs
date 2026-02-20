macro_rules! assert_eval_eq {
    ($input:expr, $expected:expr) => {
        assert_eq!(eval($input), $expected);
    };
}

macro_rules! assert_read_eq {
    ($ctx:expr, $input:expr, $expected:expr) => {
        assert_eq!(read(&$ctx, $input), $expected);
    };
}

pub(crate) use assert_eval_eq;
pub(crate) use assert_read_eq;
