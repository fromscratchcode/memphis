use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn index_access_read() {
    let input = r#"[1,2,3][1]"#;
    assert_crosscheck_return!(input, int!(2));

    let input = r#"[1,2,3][-1]"#;
    assert_crosscheck_return!(input, int!(3));

    let e = crosscheck_expect_error!(r#"[1,2,3][3]"#);
    assert_index_error!(e.exception, "list index out of range");

    let input = r#"[1,2,3]["a"]"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(
        e.exception,
        "list indices must be integers or slices, not str"
    );

    let input = r#"
class Foo: pass
[1,2,3][Foo()]
"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(
        e.exception,
        "list indices must be integers or slices, not Foo"
    );

    let input = r#"
class Foo: pass
[1,2,3][Foo]
"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(
        e.exception,
        "list indices must be integers or slices, not type"
    );

    let input = r#"(1,2,3)[2]"#;
    assert_crosscheck_return!(input, int!(3));

    let input = r#"(1,2,3)[-1]"#;
    assert_crosscheck_return!(input, int!(3));

    let input = r#"(1,2,3)[3]"#;
    let e = crosscheck_expect_error!(input);
    assert_index_error!(e.exception, "tuple index out of range");

    let input = r#"(1,2,3)["a"]"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(
        e.exception,
        "tuple indices must be integers or slices, not str"
    );

    let input = r#"{"a": 4}["a"]"#;
    assert_crosscheck_return!(input, int!(4));

    let input = r#"{"a": 4}["b"]"#;
    let e = crosscheck_expect_error!(input);
    assert_key_error!(e.exception, "b");

    let input = r#""abcdef"[2]"#;
    assert_crosscheck_return!(input, str!("c"));

    let input = r#""abcdef"[-1]"#;
    assert_crosscheck_return!(input, str!("f"));

    let input = r#""abcdef"[8]"#;
    let e = crosscheck_expect_error!(input);
    assert_index_error!(e.exception, "string index out of range");

    let input = r#""abcdef"["b"]"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(e.exception, "string indices must be integers, not 'str'");

    let input = r#"4[1]"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(e.exception, "'int' object is not subscriptable");
}

#[test]
fn index_access_write() {
    let input = r#"
a = [1,2,3]
a[0] = 10
a
"#;
    assert_crosscheck_return!(input, list![int!(10), int!(2), int!(3),]);

    let input = r#"
a = [1,2,3]
a[-1] = 10
a
"#;
    assert_crosscheck_return!(input, list![int!(1), int!(2), int!(10),]);

    let input = r#"
a = [1,2,3]
a[10] = 10
"#;
    let e = crosscheck_expect_error!(input);
    assert_index_error!(e.exception, "list assignment index out of range");

    let input = r#"
a = [1,2,3]
a["10"] = 10
"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(
        e.exception,
        "list indices must be integers or slices, not str"
    );

    let input = r#"
(1,2,3)[1] = 10
"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(
        e.exception,
        "'tuple' object does not support item assignment"
    );

    let input = r#"
4[1] = 10
"#;
    let e = crosscheck_expect_error!(input);
    assert_type_error!(e.exception, "'int' object does not support item assignment");
}
