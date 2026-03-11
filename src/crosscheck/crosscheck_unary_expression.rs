use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn unary_expression_negative() {
    let input = "-2";
    assert_crosscheck_return!(input, int!(-2));

    let input = "-2.5";
    assert_crosscheck_return!(input, float!(-2.5));

    let input = "-(-2)";
    assert_crosscheck_return!(input, int!(2));

    let input = "-(-2.5)";
    assert_crosscheck_return!(input, float!(2.5));
}

#[test]
fn unary_expression_positive() {
    let input = "+5";
    assert_crosscheck_return!(input, int!(5));

    let input = "+(-5)";
    assert_crosscheck_return!(input, int!(-5));
}

#[test]
fn unary_expression_not() {
    let input = "not True";
    assert_crosscheck_return!(input, bool!(false));

    let input = "not False";
    assert_crosscheck_return!(input, bool!(true));

    let input = "not None";
    assert_crosscheck_return!(input, bool!(true));

    let input = "not 1";
    assert_crosscheck_return!(input, bool!(false));

    let input = "not 0";
    assert_crosscheck_return!(input, bool!(true));

    let input = "not 1.0";
    assert_crosscheck_return!(input, bool!(false));

    let input = "not 0.0";
    assert_crosscheck_return!(input, bool!(true));

    let input = r#"not "a""#;
    assert_crosscheck_return!(input, bool!(false));

    let input = r#"not """#;
    assert_crosscheck_return!(input, bool!(true));

    let input = "not [1]";
    assert_crosscheck_return!(input, bool!(false));

    let input = "not []";
    assert_crosscheck_return!(input, bool!(true));
}

#[test]
fn unary_expression_invert() {
    let input = "~0b1101";
    assert_crosscheck_return!(input, int!(-14));
}
