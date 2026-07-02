use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn unpacking() {
    let input = r#"
a, b = 2, 3
a, b
"#;
    assert_crosscheck_return!(input, tuple![int!(2), int!(3)]);

    let input = r#"a, b = 2, 3, 4"#;
    let e = crosscheck_expect_error!(input);
    assert_value_error!(e.exception, "too many values to unpack (expected 2)");

    let input = r#"a, b, c, d = 2, 3, 4"#;
    let e = crosscheck_expect_error!(input);
    assert_value_error!(
        e.exception,
        "not enough values to unpack (expected 4, got 3)"
    );
}

#[test]
fn unpacking_list() {
    let input = r#"
a, b = [2, 3]
a, b
"#;
    assert_crosscheck_return!(input, tuple![int!(2), int!(3)]);
}

#[test]
fn unpacking_range() {
    let input = r#"
a, b = range(1,3)
a, b
"#;
    assert_crosscheck_return!(input, tuple![int!(1), int!(2)]);
}

#[test]
#[ignore]
fn unpacking_set() {
    let input = r#"
f, g = {1, 2}
{ f, g } == { 1, 2 }
"#;
    assert_crosscheck_return!(input, bool!(true));
}

#[test]
#[ignore]
fn unpacking_unary_op() {
    let input = r#"
l = [1,2]
(*l,)
"#;
    assert_crosscheck_return!(input, tuple![int!(1), int!(2)]);
}
