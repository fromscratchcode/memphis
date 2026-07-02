use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn next_builtin_list_iterator() {
    let input = r#"
it = iter([1, 2, 3])
a = next(it)
b = next(it)
c = next(it)
a, b, c
"#;

    assert_crosscheck_return!(input, tuple![int!(1), int!(2), int!(3)]);
}

#[test]
fn next_builtin_tuple_iterator() {
    let input = r#"
it = iter((4, 5, 6))
a = next(it)
b = next(it)
c = next(it)
a, b, c
"#;

    assert_crosscheck_return!(input, tuple![int!(4), int!(5), int!(6)]);
}

#[test]
fn next_builtin_range_iterator() {
    let input = r#"
it = iter(range(7, 10))
a = next(it)
b = next(it)
c = next(it)
a, b, c
"#;

    assert_crosscheck_return!(input, tuple![int!(7), int!(8), int!(9)]);
}
