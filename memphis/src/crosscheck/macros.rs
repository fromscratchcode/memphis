macro_rules! crosscheck_expect_error {
    ($src:expr) => {{
        let (tw_err, vm_err) = $crate::crosscheck::CrosscheckSession::new()
            .run_expect_error($crate::domain::Text::new($src));
        assert_eq!(
            tw_err, vm_err,
            "Engines did not return the same error (trewalk, VM)"
        );
        tw_err
    }};
}

macro_rules! assert_crosscheck_return {
    ($src:expr, $expected:expr) => {{
        let mut session = $crate::crosscheck::CrosscheckSession::new();
        let (tw_val, vm_val) = session.eval($crate::domain::Text::new($src));
        assert_eq!(tw_val, vm_val, "Engines did not return the same value");
        assert_eq!(
            tw_val, $expected,
            "Treewalk return value did not match expected"
        );
        assert_eq!(
            vm_val, $expected,
            "Bytecode VM return value did not match expected"
        );
    }};
}

pub(crate) use assert_crosscheck_return;
pub(crate) use crosscheck_expect_error;
