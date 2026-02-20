use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn generator_next_builtin() {
    let mut session = crosscheck_eval!(
        r#"
def gen():
    yield 1
    yield 2

g = gen()
a = next(g)
b = next(g)
"#
    );
    assert_crosscheck_eq!(session, "a", int!(1));
    assert_crosscheck_eq!(session, "b", int!(2));
}

#[test]
fn generator_as_iterator() {
    let mut session = crosscheck_eval!(
        r#"
def gen():
    yield 1
    yield 2

s = 11
for i in gen():
    s = s + i
"#
    );
    assert_crosscheck_eq!(session, "i", int!(2));
    assert_crosscheck_eq!(session, "s", int!(14));
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
    let mut session = crosscheck_eval!(
        r#"
def gen():
    yield from [1, 2, 3]

a = list(gen())
"#
    );

    assert_crosscheck_eq!(session, "a", list![int!(1), int!(2), int!(3)]);
}

#[test]
fn yield_from_list_builtin_generator_iterable() {
    let mut session = crosscheck_eval!(
        r#"
def subgen():
    yield 1
    yield 2
    yield 4

def gen():
    yield from subgen()

a = list(gen())
"#
    );

    assert_crosscheck_eq!(session, "a", list![int!(1), int!(2), int!(4)]);
}
