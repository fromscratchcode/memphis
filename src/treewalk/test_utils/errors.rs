macro_rules! assert_type_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::TypeError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected TypeError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected TypeError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::TypeError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected TypeError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected TypeError message"
                        );
                    }
                    other => panic!(
                        "Expected TypeError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected TypeError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_type_error_args {
    ($exc:expr, [$($expected:expr),* $(,)?]) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::TypeError,
                payload,
            } => {
                let expected = vec![$($expected),*];

                assert_eq!(
                    payload.len(),
                    expected.len(),
                    "Expected TypeError with {} args, got payload: {:?}",
                    expected.len(),
                    payload
                );

                for (i, (value, expected_str)) in payload.iter().zip(expected.iter()).enumerate() {
                    match value {
                        $crate::treewalk::TreewalkValue::Str(s) => {
                            assert_eq!(
                                s.as_str(),
                                *expected_str,
                                "Mismatch at payload index {}",
                                i
                            );
                        }
                        other => panic!(
                            "Expected payload[{}] to be a string, got {:?}",
                            i,
                            other
                        ),
                    }
                }
            }
            _ => panic!("Expected TypeError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_value_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::ValueError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected ValueError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected ValueError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::ValueError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected ValueError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected ValueError message"
                        );
                    }
                    other => panic!(
                        "Expected ValueError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected ValueError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_syntax_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::SyntaxError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected SyntaxError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected SyntaxError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::SyntaxError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected SyntaxError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected SyntaxError message"
                        );
                    }
                    other => panic!(
                        "Expected SyntaxError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected SyntaxError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_assertion_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::AssertionError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected AssertionError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected AssertionError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::AssertionError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected AssertionError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected AssertionError message"
                        );
                    }
                    other => panic!(
                        "Expected AssertionError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected AssertionError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_stop_iteration {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::StopIteration,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected StopIteration with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected StopIteration, got: {:?}", &$exc),
        }
    }}; // TODO we don't check for a message here beacuse StopIteration often includes a full object
}

macro_rules! assert_div_by_zero_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::DivisionByZero,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected DivisionByZero with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected DivisionByZero, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::DivisionByZero,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected DivisionByZero with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected DivisionByZero message"
                        );
                    }
                    other => panic!(
                        "Expected DivisionByZero message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected DivisionByZero, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_runtime_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::RuntimeError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected RuntimeError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected RuntimeError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::RuntimeError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected RuntimeError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected RuntimeError message"
                        );
                    }
                    other => panic!(
                        "Expected RuntimeError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected RuntimeError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_attribute_error {
    ($exc:expr, $expected_class:expr, $expected_attr:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::AttributeError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    2,
                    "Expected AttributeError with two arguments, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_class,
                            "Unexpected AttributeError message"
                        );
                    }
                    other => panic!(
                        "Expected AttributeError message to be a string, got: {:?}",
                        other
                    ),
                }

                match &payload[1] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_attr,
                            "Unexpected AttributeError message"
                        );
                    }
                    other => panic!(
                        "Expected AttributeError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected AttributeError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_key_error {
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::KeyError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected KeyError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(s.as_str(), $expected_message, "Unexpected KeyError message");
                    }
                    other => panic!("Expected KeyError message to be a string, got: {:?}", other),
                }
            }
            _ => panic!("Expected KeyError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_index_error {
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::IndexError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected IndexError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected IndexError message"
                        );
                    }
                    other => panic!(
                        "Expected IndexError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected IndexError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_name_error {
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::NameError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected NameError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected NameError message"
                        );
                    }
                    other => panic!(
                        "Expected NameError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected NameError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_lookup_error {
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::LookupError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected LookupError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected LookupError message"
                        );
                    }
                    other => panic!(
                        "Expected LookupError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected LookupError, got: {:?}", &$exc),
        }
    }};
}

pub(crate) use assert_assertion_error;
pub(crate) use assert_attribute_error;
pub(crate) use assert_div_by_zero_error;
pub(crate) use assert_index_error;
pub(crate) use assert_key_error;
pub(crate) use assert_lookup_error;
pub(crate) use assert_name_error;
pub(crate) use assert_runtime_error;
pub(crate) use assert_stop_iteration;
pub(crate) use assert_syntax_error;
pub(crate) use assert_type_error;
pub(crate) use assert_type_error_args;
pub(crate) use assert_value_error;
