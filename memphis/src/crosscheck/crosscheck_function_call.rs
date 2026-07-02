use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn function_call() {
    let input = r#"
def foo(a, b):
    return a + b

foo(2, 9)
"#;
    assert_crosscheck_return!(input, int!(11));

    let input = r#"
def foo(a, b):
    c = 9
    return a + b + c

foo(2, 9)
"#;
    assert_crosscheck_return!(input, int!(20));
}

#[test]
fn function_call_with_no_return() {
    let text = r#"
def hello():
    print("Hello")

def world():
    print("World")

hello()
world()
"#;
    assert_crosscheck_return!(text, none!());
}

#[test]
fn function_call_with_nested_function() {
    let text = r#"
def foo(a, b):
    def inner(c, d):
        return c * d
    return a + b + inner(a, b)

foo(2, 9)
"#;
    assert_crosscheck_return!(text, int!(29));
}

#[test]
fn stack_trace() {
    let e = crosscheck_expect_error!(
        r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#
    );

    assert_name_error!(e.exception, "unknown");
    assert_eq!(e.debug_call_stack.len(), 3);
    assert_eq!(e.debug_call_stack.get(0).name(), "<module>");
    assert_eq!(e.debug_call_stack.get(0).file_path_str(), "<stdin>");
    assert_eq!(e.debug_call_stack.get(0).line_number(), 8);
    assert_eq!(e.debug_call_stack.get(1).name(), "middle_call");
    assert_eq!(e.debug_call_stack.get(1).file_path_str(), "<stdin>");
    assert_eq!(e.debug_call_stack.get(1).line_number(), 3);
    assert_eq!(e.debug_call_stack.get(2).name(), "last_call");
    assert_eq!(e.debug_call_stack.get(2).file_path_str(), "<stdin>");
    assert_eq!(e.debug_call_stack.get(2).line_number(), 6);
}
