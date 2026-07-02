use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn closure() {
    let input = r#"
def make_adder(x):
    def inner_adder(y):
        return x + y
    return inner_adder

adder = make_adder(10)
adder(5)
"#;
    assert_crosscheck_return!(input, int!(15));
}

#[test]
fn function_call_with_callee() {
    let text = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2.5
    return wrapper

def get_val_undecorated():
    return 2

test_decorator(get_val_undecorated)()
"#;
    assert_crosscheck_return!(text, float!(5.0));
}

#[test]
fn function_call_with_decorator() {
    let text = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2
    return wrapper

@test_decorator
def once_decorated():
    return 2

once_decorated()
"#;
    assert_crosscheck_return!(text, int!(4));
}

#[test]
fn function_call_with_multiple_decorators() {
    let text = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2
    return wrapper

@test_decorator
@test_decorator
def twice_decorated():
    return 2

twice_decorated()
"#;
    assert_crosscheck_return!(text, int!(8));
}

#[test]
fn function_call_with_two_stage_decorators() {
    let text = r#"
def multiply(factor):
    def decorator(func):
        def wrapper():
            return func() * factor
        return wrapper
    return decorator

@multiply(3)
def get_val():
    return 2

@multiply(4)
def get_larger_val():
    return 2

a = get_val()
b = get_larger_val()
a, b
"#;
    assert_crosscheck_return!(text, tuple![int!(6), int!(8)]);
}
