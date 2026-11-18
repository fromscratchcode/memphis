use crate::domain::test_utils::*;

use super::macros::*;

#[test]
fn comparison_in() {
    let text = "4 in [1,2,3,4]";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4 in [1,2,3,5]";
    assert_crosscheck_return!(text, bool!(false));

    let text = "not (4 in [1,2,3,5])";
    assert_crosscheck_return!(text, bool!(true));

    let text = r#""a" in ["a"]"#;
    assert_crosscheck_return!(text, bool!(true));

    let input = "4 in range(5)";
    assert_crosscheck_return!(input, bool!(true));

    let input = "4 in range(3)";
    assert_crosscheck_return!(input, bool!(false));
}

#[test]
#[ignore]
fn comparison_in_str() {
    let text = r#""a" in "a""#;
    let e = crosscheck_expect_error!(text);
    // TODO this shouldn't actually fail
    assert_type_error!(e.exception, "'str' object is not iterable");
}

#[test]
fn comparison_not_in() {
    let text = "4 not in [1,2,3,4]";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4 not in [1,2,3,5]";
    assert_crosscheck_return!(text, bool!(true));

    let text = r#""a" not in ["a"]"#;
    assert_crosscheck_return!(text, bool!(false));

    let input = "4 not in range(5)";
    assert_crosscheck_return!(input, bool!(false));

    let input = "4 not in range(3)";
    assert_crosscheck_return!(input, bool!(true));
}

#[test]
#[ignore]
fn comparison_not_in_str() {
    let text = r#""a" not in "a""#;
    let e = crosscheck_expect_error!(text);
    // TODO this shouldn't actually fail
    assert_type_error!(e.exception, "'str' object is not iterable");
}

#[test]
fn comparison_is() {
    let text = "4 is None";
    assert_crosscheck_return!(text, bool!(false));
}

#[test]
fn comparison_is_not() {
    let text = "4 is not None";
    assert_crosscheck_return!(text, bool!(true));
}

#[test]
fn comparison_eq() {
    let text = "4 == 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4 == 4";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4.1 == 4.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4 == 4.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4 == 4.0";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4.0 == 4";
    assert_crosscheck_return!(text, bool!(true));

    let text = r#""a" == "a""#;
    assert_crosscheck_return!(text, bool!(true));

    let text = r#""a" == "b""#;
    assert_crosscheck_return!(text, bool!(false));
}

#[test]
fn comparison_ne() {
    let text = "4 != 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4 != 4";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4.1 != 4.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4 != 4.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4 != 4.0";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4.0 != 4";
    assert_crosscheck_return!(text, bool!(false));

    let text = r#""a" != "a""#;
    assert_crosscheck_return!(text, bool!(false));

    let text = r#""a" != "b""#;
    assert_crosscheck_return!(text, bool!(true));
}

#[test]
fn comparison_less_than() {
    let text = "4 < 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6 < 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "5 < 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4.1 < 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6.1 < 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4 < 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6 < 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4.1 < 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6.1 < 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6.1 < 6.1";
    assert_crosscheck_return!(text, bool!(false));
}

#[test]
fn comparison_less_than_or_equal() {
    let text = "4 <= 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6 <= 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "5 <= 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4.1 <= 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6.1 <= 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4 <= 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6 <= 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4.1 <= 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6.1 <= 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6.1 <= 6.1";
    assert_crosscheck_return!(text, bool!(true));
}

#[test]
fn comparison_greater_than() {
    let text = "4 > 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6 > 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "5 > 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "4.1 > 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6.1 > 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4 > 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6 > 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4.1 > 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6.1 > 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6.1 > 6.1";
    assert_crosscheck_return!(text, bool!(false));
}

#[test]
fn comparison_greater_than_or_equal() {
    let text = "4 >= 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6 >= 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "5 >= 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4.1 >= 5";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6.1 >= 5";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4 >= 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6 >= 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "4.1 >= 5.1";
    assert_crosscheck_return!(text, bool!(false));

    let text = "6.1 >= 5.1";
    assert_crosscheck_return!(text, bool!(true));

    let text = "6.1 >= 6.1";
    assert_crosscheck_return!(text, bool!(true));
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
