use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn list_comprehension() {
    let input = r#"
[ i * 2 for i in [1,2] ]
"#;
    assert_crosscheck_return!(input, list![int!(2), int!(4)]);

    let input = r#"
[ i * 2 for i in range(1,4) ]
"#;
    assert_crosscheck_return!(input, list![int!(2), int!(4), int!(6),]);
}

#[test]
fn list_comprehension_conditional() {
    let input = r#"
[ i * 2 for i in range(1,4) if False ]
"#;
    assert_crosscheck_return!(input, list![]);

    let input = r#"
[ j * 2 for j in range(1,4) if j > 2 ]
"#;
    assert_crosscheck_return!(input, list![int!(6),]);
}

#[test]
fn list_comprehension_multiple_clauses() {
    let input = r#"
[x * y for x in range(1,3) for y in range(1,3)]
"#;
    assert_crosscheck_return!(input, list![int!(1), int!(2), int!(2), int!(4),]);
}

#[test]
fn list_comprehension_tuple_unpacking() {
    let input = r#"
[x + y for (x, y) in [(1, 2), (3, 4)]]
"#;

    assert_crosscheck_return!(input, list![int!(3), int!(7)]);
}

#[test]
fn list_comprehension_preserves_existing_loop_variable() {
    let input = r#"
x = "outside"
result = [x for x in [1, 2]]
result, x
"#;
    assert_crosscheck_return!(input, tuple![list![int!(1), int!(2)], str!("outside")]);
}

#[test]
fn list_comprehension_does_not_introduce_loop_variable() {
    let input = r#"
result = [y for y in [1]]
y
"#;
    let e = crosscheck_expect_error!(input);
    assert_name_error!(e.exception, "y");
}

#[test]
fn list_comprehension_evaluates_first_iterable_in_outer_scope() {
    let input = r#"
x = [1, 2]
result = [x for x in x]
result, x
"#;
    assert_crosscheck_return!(
        input,
        tuple![list![int!(1), int!(2)], list![int!(1), int!(2)]]
    );
}

#[test]
fn list_comprehension_nested_clauses_and_filters_are_isolated() {
    let input = r#"
x = "outer x"
y = "outer y"
result = [(x, y) for x in [1, 2] for y in [x, x + 1] if y > x]

result, x, y
"#;
    assert_crosscheck_return!(
        input,
        tuple![
            list![tuple![int!(1), int!(2)], tuple![int!(2), int!(3)]],
            str!("outer x"),
            str!("outer y")
        ]
    );
}
