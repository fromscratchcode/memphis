use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn logical_operators() {
    let input = "True and False";
    assert_crosscheck_return!(input, bool!(false));

    let input = "True or False";
    assert_crosscheck_return!(input, bool!(true));

    let input = "not (True or False)";
    assert_crosscheck_return!(input, bool!(false));

    let input = "True and not False";
    assert_crosscheck_return!(input, bool!(true));

    let input = "not False";
    assert_crosscheck_return!(input, bool!(true));

    let input = "not True";
    assert_crosscheck_return!(input, bool!(false));

    let input = "None or []";
    assert_crosscheck_return!(input, list![]);

    let input = "None and []";
    assert_crosscheck_return!(input, none!());

    let input = "True or 1 / 0";
    assert_crosscheck_return!(input, bool!(true));

    let input = "False and 1 / 0";
    assert_crosscheck_return!(input, bool!(false));

    let input = "False or 7";
    assert_crosscheck_return!(input, int!(7));

    let input = "True and 7";
    assert_crosscheck_return!(input, int!(7));
}
