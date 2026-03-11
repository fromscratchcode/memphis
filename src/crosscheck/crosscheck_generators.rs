use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn generator_next_builtin() {
    let input = r#"
def gen():
    yield 1
    yield 2

g = gen()
a = next(g)
b = next(g)
a,b
"#;
    assert_crosscheck_return!(input, tuple![int!(1), int!(2)]);
}

#[test]
fn generator_as_iterator() {
    let input = r#"
def gen():
    yield 1
    yield 2

s = 11
for i in gen():
    s = s + i
i,s
"#;
    assert_crosscheck_return!(input, tuple![int!(2), int!(14)]);
}

#[test]
fn generator_stop_iteration() {
    let e = crosscheck_expect_error!(
        r#"
def gen():
    yield 1

g = gen()
a = next(g)
b = next(g)
"#
    );

    assert_stop_iteration!(e.exception);
}

#[test]
fn yield_from_list_builtin_list_iterable() {
    let input = r#"
def gen():
    yield from [1, 2, 3]

list(gen())
"#;

    assert_crosscheck_return!(input, list![int!(1), int!(2), int!(3)]);
}

#[test]
fn yield_from_list_builtin_generator_iterable() {
    let input = r#"
def subgen():
    yield 1
    yield 2
    yield 4

def gen():
    yield from subgen()

list(gen())
"#;
    assert_crosscheck_return!(input, list![int!(1), int!(2), int!(4)]);
}
