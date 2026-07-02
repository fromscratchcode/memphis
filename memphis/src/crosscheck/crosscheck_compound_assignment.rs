use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn compound_assignment() {
    let input = r#"
a = 5
a += 1
a
"#;
    assert_crosscheck_return!(input, int!(6));

    let input = r#"
a = 5
a -= 1
a
"#;
    assert_crosscheck_return!(input, int!(4));

    let input = r#"
a = 5
a *= 2
a
"#;
    assert_crosscheck_return!(input, int!(10));

    let input = r#"
a = 5
a /= 2
a
"#;
    assert_crosscheck_return!(input, float!(2.5));
}
