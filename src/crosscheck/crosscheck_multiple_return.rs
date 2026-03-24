use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn multiple_return() {
    let input = r#"
def foo():
    return
foo()
"#;
    assert_crosscheck_return!(input, none!());

    let input = r#"
def foo():
    return 1
foo()
"#;
    assert_crosscheck_return!(input, int!(1));

    let input = r#"
def foo():
    return 1, 2
foo()
"#;
    assert_crosscheck_return!(input, tuple![int!(1), int!(2)]);
}

#[test]
fn multiple_return_crossed_with_unpaacking() {
    let input = r#"
def foo():
    return 2, 4
a, b = foo()
a, b
"#;
    assert_crosscheck_return!(input, tuple![int!(2), int!(4)]);
}
