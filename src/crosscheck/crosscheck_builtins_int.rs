use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn builtin_int() {
    let input = r#"int()"#;
    assert_crosscheck_return!(input, int!(0));

    let input = r#"int(5)"#;
    assert_crosscheck_return!(input, int!(5));

    let input = r#"int('6')"#;
    assert_crosscheck_return!(input, int!(6));

    let input = r#"int(9.9)"#;
    assert_crosscheck_return!(input, int!(9));
}
