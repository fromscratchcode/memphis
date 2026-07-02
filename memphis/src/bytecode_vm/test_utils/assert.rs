macro_rules! assert_eval_eq {
    ($input:expr, $expected:expr) => {
        assert_eq!(eval($input), $expected);
    };
}

pub(crate) use assert_eval_eq;
