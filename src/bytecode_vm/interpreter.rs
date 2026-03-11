#[cfg(test)]
mod tests_vm_interpreter {
    use crate::{bytecode_vm::test_utils::*, domain::test_utils::*};

    #[test]
    fn expression() {
        let text = "4 * (2 + 3)";
        assert_eval_eq!(text, int!(20));

        let text = "4 > x";
        let e = eval_expect_error(text);
        assert_name_error!(e.exception, "x");

        let text = "y()";
        let e = eval_expect_error(text);
        assert_name_error!(e.exception, "y");
    }

    #[test]
    fn binary_multiplication_str() {
        let text = "4 * 'a'";
        assert_eval_eq!(text, str!("aaaa"));

        let text = "'b' * 3";
        assert_eval_eq!(text, str!("bbb"));

        let text = "-4 * 'a'";
        assert_eval_eq!(text, str!(""));

        let text = "'b' * -3";
        assert_eval_eq!(text, str!(""));

        let text = "4.1 * 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "unsupported operand type(s) for *");
    }

    #[test]
    fn list_literal() {
        let text = "[2,3]";
        assert_eval_eq!(text, list![int!(2), int!(3)]);

        let text = r#"[2,"Hello"]"#;
        assert_eval_eq!(text, list![int!(2), str!("Hello")]);
    }

    #[test]
    fn list_builtin() {
        let text = "list()";
        assert_eval_eq!(text, list![]);

        let text = "list([])";
        assert_eval_eq!(text, list![]);

        let text = "list([2,3])";
        assert_eval_eq!(text, list![int!(2), int!(3)]);

        let text = "list((2,3))";
        assert_eval_eq!(text, list![int!(2), int!(3)]);

        let text = "list(range(2))";
        assert_eval_eq!(text, list![int!(0), int!(1)]);

        let text = "list(1,2)";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "list expected at most 1 argument, got 2");
    }

    #[test]
    fn tuple_literal() {
        let text = "(2,3)";
        assert_eval_eq!(text, tuple![int!(2), int!(3)]);

        let text = r#"(2,"Hello")"#;
        assert_eval_eq!(text, tuple![int!(2), str!("Hello")]);
    }

    #[test]
    fn dict_literal() {
        let text = r#"{ 2: 1 }"#;
        assert_eval_eq!(text, dict!({ int!(2) => int!(1) }));

        let text = r#"{"a": 22}"#;
        assert_eval_eq!(text, dict!({ str!("a") => int!(22) }));
    }

    #[test]
    fn tuple_builtin() {
        let text = "tuple()";
        assert_eval_eq!(text, tuple![]);

        let text = "tuple([])";
        assert_eval_eq!(text, tuple![]);

        let text = "tuple([2,3])";
        assert_eval_eq!(text, tuple![int!(2), int!(3)]);

        let text = "tuple((2,3))";
        assert_eval_eq!(text, tuple![int!(2), int!(3)]);

        let text = "tuple(range(2))";
        assert_eval_eq!(text, tuple![int!(0), int!(1)]);

        let text = "tuple(1,2)";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "tuple expected at most 1 argument, got 2");
    }

    #[test]
    fn range_builtin() {
        let text = "range(5)";
        assert_eval_eq!(text, range!(5));

        let text = "range(2, 5)";
        assert_eval_eq!(text, range!(2, 5));

        let text = "range(2, 5, 3)";
        assert_eval_eq!(text, range!(2, 5, 3));

        let text = "range()";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "range expected at least 1 argument, got 0");

        let text = "range(1,1,1,1)";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "range expected at most 3 arguments, got 4");
    }

    #[test]
    fn bool_builtin() {
        let text = r#"bool()"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#"bool(True)"#;
        assert_eval_eq!(text, bool!(true));

        let text = r#"bool(False)"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#"bool([])"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#"bool([1])"#;
        assert_eval_eq!(text, bool!(true));

        let text = r#"bool('')"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#"bool('hello')"#;
        assert_eval_eq!(text, bool!(true));

        let text = r#"bool(0)"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#"bool(5)"#;
        assert_eval_eq!(text, bool!(true));

        let text = r#"bool(())"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#"bool((1))"#;
        assert_eval_eq!(text, bool!(true));
    }

    #[test]
    fn for_in_loop_list() {
        let text = r#"
s = 0
for i in [2,3,11]:
    s = s + i

i, s
"#;
        assert_eval_eq!(text, tuple![int!(11), int!(16)]);
    }

    #[test]
    fn next_builtin_list() {
        let text = r#"
it = iter([1, 2, 3])
a = next(it)
b = next(it)
c = next(it)

a, b, c
"#;
        assert_eval_eq!(text, tuple![int!(1), int!(2), int!(3)]);

        let text = "next([1])";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "'list' object is not an iterator");
    }

    #[test]
    fn for_in_loop_tuple() {
        let text = r#"
s = 0
for i in (2,3,11):
    s = s + i

i,s
"#;
        assert_eval_eq!(text, tuple![int!(11), int!(16)]);
    }

    #[test]
    fn next_builtin_tuple() {
        let text = r#"
it = iter((1, 2, 3))
a = next(it)
b = next(it)
c = next(it)
a,b,c
"#;
        assert_eval_eq!(text, tuple![int!(1), int!(2), int!(3)]);

        let text = "next((1,))";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "'tuple' object is not an iterator");
    }

    #[test]
    fn for_in_loop_range() {
        let text = r#"
s = 0
for i in range(2,10,2):
    s = s + i

i, s
"#;
        assert_eval_eq!(text, tuple![int!(8), int!(20)]);
    }

    #[test]
    fn for_in_loop_range_with_inner_function() {
        let text = r#"
def test():
    x = 10
    for i in range(3):
        print(i, x)

test()
"#;
        // No output to check here. This test previously failed before we emitted a PopTop after a
        // function call, and before we properly split stack vs local handling inside a Frame.
        assert_eval_eq!(text, none!());
    }

    #[test]
    fn next_builtin_range() {
        let text = "next(iter(range(5)))";
        assert_eval_eq!(text, int!(0));

        let text = "next(range(5))";
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "'range' object is not an iterator");
    }

    #[test]
    fn list_builtin_generator() {
        let text = r#"
def gen():
    yield 1
    yield 2

list(gen())
"#;
        assert_eval_eq!(text, list![int!(1), int!(2)]);
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar():
        return 4
"#;
        assert_eval_eq!(text, none!());
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
class Foo:
    def bar():
        return 4

f = Foo()
"#;
        assert_eval_eq!(text, none!());
    }

    #[test]
    fn class_with_method_call() {
        let text = r#"
class Foo:
    def bar(self, val):
        return 4 + val

f = Foo()
f.bar(11)
"#;
        assert_eval_eq!(text, int!(15));
    }

    #[test]
    fn index_access() {
        let text = r#"[1,2,3][1]"#;
        assert_eval_eq!(text, int!(2));

        let text = r#"[1,2,3][3]"#;
        let e = eval_expect_error(text);
        assert_index_error!(e.exception, "list index out of range");
    }

    #[test]
    fn class_with_member_access() {
        let text = r#"
class Foo: pass
f = Foo()
f.x = 4
f.x
"#;
        assert_eval_eq!(text, int!(4));
    }

    #[test]
    fn class_with_bound_method() {
        let text = r#"
class Foo:
    def bar(self):
        self.x = 4

f = Foo()
f.bar()
f.x
"#;
        assert_eval_eq!(text, int!(4));
    }

    #[test]
    fn class_instantiation_with_constructor() {
        let text = r#"
class Foo:
    def __init__(self):
        self.x = 44

f = Foo()
f.x
"#;
        assert_eval_eq!(text, int!(44));
    }

    #[test]
    fn class_instantiation_with_constructor_and_args() {
        let text = r#"
class Foo:
    def __init__(self, val):
        self.x = val

f = Foo(33)
f.x
"#;
        assert_eval_eq!(text, int!(33));
    }

    #[test]
    fn regular_import_same_file() {
        let output = run_script("src/bytecode_vm/fixtures/imports/one/main.py");
        assert_eq!(output, "5\n");
    }

    #[test]
    fn regular_import_function_in_other_file() {
        let output = run_script("src/bytecode_vm/fixtures/imports/two/main.py");
        assert_eq!(output, "33\n");
    }

    #[test]
    fn selective_import_relative() {
        let output = run_script("src/fixtures/imports/relative/main_a.py");
        assert_eq!(output, "2\n");
    }

    #[test]
    fn regular_import_relative_parent_package() {
        let output = run_script("src/fixtures/imports/relative/main_b.py");
        assert_eq!(output, "2\n");
    }

    #[test]
    fn regular_import_relative_alias() {
        let output = run_script("src/fixtures/imports/relative/main_c.py");
        assert_eq!(output, "2\n");
    }

    #[test]
    fn relative_import_from_package() {
        let output = run_script("src/fixtures/imports/pkg_test/test_app.py");
        assert_eq!(output, "111\n");
    }

    #[test]
    fn regular_import_error() {
        let text = r#"
import not_found
"#;
        let e = eval_expect_error(text);
        assert_import_error!(e.exception, "No module named not_found");
    }

    #[test]
    fn stack_trace() {
        let text = r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#;
        let e = eval_expect_error(text);
        assert_name_error!(e.exception, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert_eq!(call_stack.get(0).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert_eq!(call_stack.get(1).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert_eq!(call_stack.get(2).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn stack_trace_from_file() {
        let e = run_path_expect_error("src/fixtures/call_stack/call_stack_one_file.py");
        assert_name_error!(e.exception, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn stack_trace_multiple_files() {
        let e = run_path_expect_error("src/fixtures/call_stack/call_stack.py");
        assert_name_error!(e.exception, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(0).line_number(), 2);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn exception_classes() {
        let text = r#"ZeroDivisionError.__name__"#;
        assert_eval_eq!(text, str!("ZeroDivisionError"));
    }

    #[test]
    fn try_except_catch_all_caught() {
        let text = r#"
try:
    a = 1 / 0
except:
    a = 42
a
"#;
        assert_eval_eq!(text, int!(42));
    }

    #[test]
    fn try_except_catch_all_no_exception() {
        let text = r#"
try:
    a = 1 / 2
except:
    a = 42
a
"#;
        assert_eval_eq!(text, float!(0.5));
    }

    #[test]
    fn try_except_one_typed_handler_caught() {
        let text = r#"
try:
    a = 1 / 0
except ZeroDivisionError:
    a = 42
a
"#;
        assert_eval_eq!(text, int!(42));
    }

    #[test]
    fn try_except_one_typed_handler_uncaught() {
        let text = r#"
try:
    a = 4 + "4"
except ZeroDivisionError:
    a = 42
a
"#;
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "unsupported operand type(s) for +");
    }

    #[test]
    fn try_except_one_typed_one_bare() {
        let text = r#"
try:
    a = 4 + "4"
except ZeroDivisionError:
    a = 42
except:
    a = 99
a
"#;
        assert_eval_eq!(text, int!(99));
    }

    #[test]
    fn try_except_two_typed_caught() {
        let text = r#"
try:
    a = 4 + "4"
except ZeroDivisionError:
    a = 42
except TypeError:
    a = 52
a
"#;
        assert_eval_eq!(text, int!(52));
    }

    #[test]
    fn try_except_two_typed_uncaught() {
        let text = r#"
try:
    a = 4 + "4"
except ZeroDivisionError:
    a = 42
except NameError:
    a = 52
a
"#;
        let e = eval_expect_error(text);
        assert_type_error!(e.exception, "unsupported operand type(s) for +");
    }

    #[test]
    fn try_except_typed_as_binds_exception() {
        let text = r#"
try:
    a = 1 / 0
except ZeroDivisionError as e:
    a = e
a
"#;
        let v = eval(text);
        let e = v.as_exception().expect("Expected exception");
        assert_div_by_zero_error!(e, "integer division or modulo by zero");
    }

    #[test]
    fn try_except_two_default_handlers() {
        // This also used to fail because we didn't properly initialize the VM before raising
        // syntax errors.
        let text = r#"
try:
    a = 4 + "4"
except:
    a = 42
except:
    a = 52
a
"#;
        let e = eval_expect_error(text);
        assert_syntax_error!(e.exception, "default 'except:' must be last");
    }

    #[test]
    fn try_except_typed_as_reraise() {
        let text = r#"
try:
    a = 1 / 0
except ZeroDivisionError as e:
    raise
"#;
        let e = eval_expect_error(text);
        assert_div_by_zero_error!(e.exception, "integer division or modulo by zero");
    }

    #[test]
    fn raise_without_active_exception() {
        let input = r#"
raise
"#;
        let e = eval_expect_error(input);
        assert_runtime_error!(e.exception, "No active exception to reraise");
    }

    #[test]
    fn semicolons() {
        let text = r#"
b = 2; c = 3
b, c
"#;
        assert_eval_eq!(text, tuple![int!(2), int!(3)]);

        assert_eval_eq!("a = 10; 4 + a", int!(14));
    }

    #[test]
    fn type_builtin() {
        let text = r#"type([1]).__name__"#;
        assert_eval_eq!(text, str!("list"));

        let text = r#"type(type([1]).__name__).__name__"#;
        assert_eval_eq!(text, str!("str"));
    }
}
