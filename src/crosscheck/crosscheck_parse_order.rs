use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn syntax_error_should_occur_before_runtime_error() {
    let input = r#"
print("This line has a runtime error", 1/0)
if True print("This line has a syntax error")
"#;
    let e = crosscheck_expect_error!(input);
    assert_syntax_error!(e.exception, "invalid syntax");
}
