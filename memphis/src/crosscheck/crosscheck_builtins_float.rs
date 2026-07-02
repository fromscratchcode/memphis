use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn builtin_float() {
    let input = r#"float()"#;
    assert_crosscheck_return!(input, float!(0.0));

    let input = r#"float(3.99)"#;
    assert_crosscheck_return!(input, float!(3.99));

    let input = r#"float(3)"#;
    assert_crosscheck_return!(input, float!(3.0));

    let input = r#"float('3')"#;
    assert_crosscheck_return!(input, float!(3.0));

    let input = r#"float('3.99')"#;
    assert_crosscheck_return!(input, float!(3.99));
}
