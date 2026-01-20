macro_rules! crosscheck_eval {
    ($src:expr) => {{
        $crate::crosscheck::CrosscheckSession::new($crate::domain::Text::new($src))
            .run()
            .expect("Crosscheck session failed")
    }};
}

macro_rules! crosscheck_expect_error {
    ($src:expr) => {{
        let (tw_err, vm_err) =
            $crate::crosscheck::CrosscheckSession::new($crate::domain::Text::new($src))
                .run_expect_error();
        assert_eq!(tw_err, vm_err, "Engines did not return the same error");
        tw_err
    }};
}

macro_rules! assert_crosscheck_eq {
    ($session:expr, $name:expr, $expected:expr) => {{
        let (tw_val, vm_val) = $session.read($name);
        assert_eq!(tw_val, vm_val, "Engines did not return the same value.");
        assert_eq!(tw_val, $expected, "Treewalk value did not match expected.");
        assert_eq!(
            vm_val, $expected,
            "Bytecode VM value did not match expected."
        );
    }};
}

macro_rules! assert_crosscheck_return {
    ($src:expr, $expected:expr) => {{
        let mut session =
            $crate::crosscheck::CrosscheckSession::new($crate::domain::Text::new($src));
        let (tw_val, vm_val) = session.eval();
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

pub(crate) use assert_crosscheck_eq;
pub(crate) use assert_crosscheck_return;
pub(crate) use crosscheck_eval;
pub(crate) use crosscheck_expect_error;
