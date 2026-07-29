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
fn generator_chain() {
    let input = r#"
def numbers():
    yield 1
    yield 2
    yield 3

def doubled(values):
    for value in values:
        yield value * 2

def plus_one(values):
    for value in values:
        yield value + 1

step1 = numbers()
step2 = doubled(step1)
step3 = plus_one(step2)

list(step3)
"#;
    assert_crosscheck_return!(input, list![int!(3), int!(5), int!(7)]);
}
