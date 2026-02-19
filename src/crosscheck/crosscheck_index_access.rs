use crate::domain::{test_utils::*, MemphisValue};

use super::macros::*;

#[test]
fn index_access_read() {
    let input = r#"[1,2,3][1]"#;
    assert_crosscheck_return!(input, MemphisValue::Integer(2));

    let input = r#"[1,2,3][-1]"#;
    assert_crosscheck_return!(input, MemphisValue::Integer(3));

    let e = crosscheck_expect_error!(r#"[1,2,3][3]"#);
    assert_index_error!(e.exception, "list index out of range");

    let input = r#"(1,2,3)[2]"#;
    assert_crosscheck_return!(input, MemphisValue::Integer(3));

    let input = r#"(1,2,3)[-1]"#;
    assert_crosscheck_return!(input, MemphisValue::Integer(3));

    let input = r#"(1,2,3)[3]"#;
    let e = crosscheck_expect_error!(input);
    assert_index_error!(e.exception, "tuple index out of range");
}
