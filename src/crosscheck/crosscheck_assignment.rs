use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn assignment_int() {
    let input = r#"
a = 5 - 3
a
"#;
    assert_crosscheck_return!(input, int!(2));
}

#[test]
fn assignment_str() {
    let input = r#"
a = "Hello World"
a
"#;
    assert_crosscheck_return!(input, str!("Hello World"));
}

#[test]
fn assignment_multiple() {
    let input = r#"
a = 5 - 3
b = 10
c = None
a,b,c
"#;
    assert_crosscheck_return!(input, tuple![int!(2), int!(10), none!()]);
}

#[test]
fn assignment_var() {
    let input = r#"
a = 5 - 3
b = 10 + a
b
"#;
    assert_crosscheck_return!(input, int!(12));
}
