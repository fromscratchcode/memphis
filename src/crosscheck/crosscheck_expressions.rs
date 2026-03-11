use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn binary_expression() {
    let input = "2 + 2";
    assert_crosscheck_return!(input, int!(4));

    let input = "2 + 2.1";
    assert_crosscheck_return!(input, float!(4.1));

    let input = "2 / 2";
    assert_crosscheck_return!(input, float!(1.0));

    let input = "4 * (2 + 3)";
    assert_crosscheck_return!(input, int!(20));
}

#[test]
fn operator_chaining() {
    // Equal chains
    let input = "2 == 2 == 2";
    assert_crosscheck_return!(input, bool!(true));

    let input = "2 == 2 == 3";
    assert_crosscheck_return!(input, bool!(false));

    let input = "2 == 2 == 3 == 3";
    assert_crosscheck_return!(input, bool!(false));

    let input = "2 == 2 == 2 < 3";
    assert_crosscheck_return!(input, bool!(true));

    // Increasing chain
    let input = "1 < 2 < 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "1 < 2 < 2";
    assert_crosscheck_return!(input, bool!(false));

    let input = "1 < 3 < 2";
    assert_crosscheck_return!(input, bool!(false));

    let input = "1 < 2 < 3 < 4";
    assert_crosscheck_return!(input, bool!(true));

    // Mixed increasing / equality
    let input = "1 < 2 == 2 < 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "1 < 2 == 3 < 4";
    assert_crosscheck_return!(input, bool!(false));

    // Mixed with >= and <=
    let input = "3 >= 2 > 1";
    assert_crosscheck_return!(input, bool!(true));

    let input = "3 >= 2 > 2";
    assert_crosscheck_return!(input, bool!(false));

    let input = "2 <= 2 < 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "2 <= 1 < 3";
    assert_crosscheck_return!(input, bool!(false));

    // Mixed equality and less-than
    let input = "2 == 2 < 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "2 == 3 < 4";
    assert_crosscheck_return!(input, bool!(false));

    let input = "2 < 3 == 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "2 < 3 == 4";
    assert_crosscheck_return!(input, bool!(false));

    // Descending chain
    let input = "5 > 4 > 3 > 2";
    assert_crosscheck_return!(input, bool!(true));

    let input = "5 > 4 > 5";
    assert_crosscheck_return!(input, bool!(false));

    // With floats mixed in
    let input = "1 < 2.0 < 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "1.0 < 2 < 1.5";
    assert_crosscheck_return!(input, bool!(false));

    let input = "2.0 == 2 < 3.0";
    assert_crosscheck_return!(input, bool!(true));

    // Not-equals chain
    let input = "1 != 2 != 3";
    assert_crosscheck_return!(input, bool!(true));

    let input = "1 != 1 != 2";
    assert_crosscheck_return!(input, bool!(false));

    // Mix of == and !=
    let input = "1 == 1 != 2";
    assert_crosscheck_return!(input, bool!(true));

    let input = "1 == 2 != 3";
    assert_crosscheck_return!(input, bool!(false));

    // Chained in — both true
    let input = "2 in [1,2,3] in [[1,2,3],[4,5,6]]";
    assert_crosscheck_return!(input, bool!(true));

    let input = "2 in [1,2,3] in [[2,3,4],[4,5,6]]";
    assert_crosscheck_return!(input, bool!(false));

    // Chained not in
    let input = "4 not in [1,2,3] not in [[1,2,3],[4,5,6]]";
    assert_crosscheck_return!(input, bool!(false));

    // Mixed in / not in
    let input = "2 in [1,2,3] not in [[1,2,3],[4,5,6]]";
    assert_crosscheck_return!(input, bool!(false));

    // Mixed not in / in
    let input = "4 not in [1,2,3] in [[4,5,6],[7,8,9]]";
    assert_crosscheck_return!(input, bool!(false));
}

#[test]
fn binary_logical_and() {
    let input = "False and False";
    assert_crosscheck_return!(input, bool!(false));

    let input = "True and False";
    assert_crosscheck_return!(input, bool!(false));

    let input = "False and True";
    assert_crosscheck_return!(input, bool!(false));

    let input = "True and True";
    assert_crosscheck_return!(input, bool!(true));
}

#[test]
fn binary_logical_or() {
    let input = "False or False";
    assert_crosscheck_return!(input, bool!(false));

    let input = "True or False";
    assert_crosscheck_return!(input, bool!(true));

    let input = "False or True";
    assert_crosscheck_return!(input, bool!(true));

    let input = "True or True";
    assert_crosscheck_return!(input, bool!(true));
}

#[test]
fn binary_addition() {
    let text = "4 + 3";
    assert_crosscheck_return!(text, int!(7));

    let text = "4.1 + 3.01";
    assert_crosscheck_return!(text, float!(7.11));

    let text = "4 + 3.01";
    assert_crosscheck_return!(text, float!(7.01));

    let text = "4.1 + 3";
    assert_crosscheck_return!(text, float!(7.1));

    let text = "-4 + 3";
    assert_crosscheck_return!(text, int!(-1));

    let text = "4.1 + 'a'";
    let e = crosscheck_expect_error!(text);
    assert_type_error!(e.exception, "unsupported operand type(s) for +");
}

#[test]
fn binary_subtraction() {
    let text = "4 - 3";
    assert_crosscheck_return!(text, int!(1));

    let text = "4.1 - 3.01";
    assert_crosscheck_return!(text, float!(1.09));

    let text = "4 - 3.01";
    assert_crosscheck_return!(text, float!(0.99));

    let text = "4.1 - 3";
    assert_crosscheck_return!(text, float!(1.1));

    let text = "-4 - 3";
    assert_crosscheck_return!(text, int!(-7));

    let text = "4.1 - 'a'";
    let e = crosscheck_expect_error!(text);
    assert_type_error!(e.exception, "unsupported operand type(s) for -");
}

#[test]
fn binary_multiplication() {
    let text = "4 * 3";
    assert_crosscheck_return!(text, int!(12));

    let text = "4.1 * 3.01";
    assert_crosscheck_return!(text, float!(12.341));

    let text = "4 * 3.01";
    assert_crosscheck_return!(text, float!(12.04));

    let text = "4.1 * 3";
    assert_crosscheck_return!(text, float!(12.3));

    let text = "-4 * 3";
    assert_crosscheck_return!(text, int!(-12));
}

#[test]
fn binary_division() {
    let text = "4 / 3";
    assert_crosscheck_return!(text, float!(1.33333333333));

    let text = "-4 / 3";
    assert_crosscheck_return!(text, float!(-1.33333333333));

    let text = "4.1 / 3.01";
    assert_crosscheck_return!(text, float!(1.36212624585));

    let text = "4 / 3.01";
    assert_crosscheck_return!(text, float!(1.32890365449));

    let text = "4.1 / 3";
    assert_crosscheck_return!(text, float!(1.36666666667));

    let text = "4.1 / 'a'";
    let e = crosscheck_expect_error!(text);
    assert_type_error!(e.exception, "unsupported operand type(s) for /");
}
