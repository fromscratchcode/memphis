use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn control_flow() {
    let input = r#"
i = 0
n = 4
while i < n:
    i = i + 1
i
"#;
    assert_crosscheck_return!(input, int!(4));

    let input = r#"
i = 0
if i < 10:
    a = -1
a
"#;
    assert_crosscheck_return!(input, int!(-1));

    let input = r#"
i = 0
if i > 10:
    a = -1
else:
    a = 3
a
"#;
    assert_crosscheck_return!(input, int!(3));

    let input = r#"
i = 0
if i > 10:
    a = -1
elif i > -5:
    a = -2
else:
    a = 3
a
"#;
    assert_crosscheck_return!(input, int!(-2));

    let input = r#"
x = 7
result = "none"
if x < 5:
    result = "low"
elif x < 6:
    result = "mid"
result
"#;
    assert_crosscheck_return!(input, str!("none"));

    let input = r#"
x = 2
if x < 0:
    a = "zero"
elif x < 1:
    a = "one"
elif x < 2:
    a = "two"
elif x < 3:
    a = "three"
else:
    a = "other"
a
"#;
    assert_crosscheck_return!(input, str!("three"));
}

#[test]
fn semicolons() {
    let input = r#"
b = 2; c = 3
b, c
"#;
    assert_crosscheck_return!(input, tuple![int!(2), int!(3)]);

    let input = "a = 10; 4 + a";
    assert_crosscheck_return!(input, int!(14));
}
