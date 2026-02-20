use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn method_call() {
    let mut session = crosscheck_eval!(
        r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
b = f.bar()
"#
    );
    assert_crosscheck_eq!(session, "b", int!(4));

    let mut session = crosscheck_eval!(
        r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
b = f.bar()
"#
    );
    assert_crosscheck_eq!(session, "b", int!(10));
}
