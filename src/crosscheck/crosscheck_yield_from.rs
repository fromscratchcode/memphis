use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn yield_from_list_builtin_list_iterable() {
    let input = r#"
def gen():
    yield from [1, 2, 3]

list(gen())
"#;

    assert_crosscheck_return!(input, list![int!(1), int!(2), int!(3)]);

    let input = r#"
def gen():
    yield from []

list(gen())
"#;
    assert_crosscheck_return!(input, list![]);
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

#[test]
fn yield_from_for_loop() {
    let input = r#"
def countdown(n):
    while n > 0:
        yield n
        n = n-1

def countdown_from(x, y):
    yield from countdown(x)
    yield from countdown(y)

sum = 0
for number in countdown_from(3, 2):
    sum = sum+number

sum
"#;
    assert_crosscheck_return!(input, int!(9));
}

#[test]
fn yield_from_with_return() {
    let input = r#"
def g1():
    yield 1
    yield 2
    return 42

def g2():
    x = yield from g1()
    yield x

list(g2())
"#;
    assert_crosscheck_return!(input, list![int!(1), int!(2), int!(42)]);
}
