use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn method_call() {
    let input = r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
f.bar()
"#;
    assert_crosscheck_return!(input, int!(4));

    let input = r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
f.bar()
"#;
    assert_crosscheck_return!(input, int!(10));
}
