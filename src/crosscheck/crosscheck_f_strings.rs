use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn f_strings() {
    let input = r#"
name = "John"
f"Hello {name}"
"#;
    assert_crosscheck_return!(input, str!("Hello John"));

    let input = r#"
x = 10
y = 20
f"{x} + {y} = {x + y}"
"#;
    assert_crosscheck_return!(input, str!("10 + 20 = 30"));
}
