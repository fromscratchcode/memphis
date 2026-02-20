use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn assignment() {
    let mut session = crosscheck_eval!(
        r#"
a = 5 - 3
"#
    );
    assert_crosscheck_eq!(session, "a", int!(2));

    let mut session = crosscheck_eval!(
        r#"
a = "Hello World"
"#
    );
    assert_crosscheck_eq!(session, "a", str!("Hello World"));

    let mut session = crosscheck_eval!(
        r#"
a = 5 - 3
b = 10
c = None
"#
    );
    assert_crosscheck_eq!(session, "a", int!(2));
    assert_crosscheck_eq!(session, "b", int!(10));
    assert_crosscheck_eq!(session, "c", none!());

    let mut session = crosscheck_eval!(
        r#"
a = 5 - 3
b = 10 + a
"#
    );
    assert_crosscheck_eq!(session, "a", int!(2));
    assert_crosscheck_eq!(session, "b", int!(12));
}
