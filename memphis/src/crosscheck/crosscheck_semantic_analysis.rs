use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn semantic_error_should_occur_before_runtime_error() {
    let input = r#"
print("This line has a runtime error", 1/0)
return True
"#;
    let e = crosscheck_expect_error!(input);
    assert_syntax_error!(e.exception, "'return' outside function");

    let input = r#"
1/0
nonlocal a
"#;
    let e = crosscheck_expect_error!(input);
    assert_syntax_error!(e.exception, "'nonlocal' cannot be used at module level");
}
