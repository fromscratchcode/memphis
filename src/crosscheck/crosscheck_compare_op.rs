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
