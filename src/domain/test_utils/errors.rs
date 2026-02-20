macro_rules! assert_type_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
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
            $crate::domain::MemphisException {
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
                    $crate::domain::MemphisValue::Str(s) => {
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

macro_rules! assert_index_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
                kind: $crate::domain::ExceptionKind::IndexError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected IndexError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected IndexError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
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
                    $crate::domain::MemphisValue::Str(s) => {
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
            $crate::domain::MemphisException {
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
                    $crate::domain::MemphisValue::Str(s) => {
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

macro_rules! assert_import_error {
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
                kind: $crate::domain::ExceptionKind::ImportError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected ImportError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::domain::MemphisValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected ImportError message"
                        );
                    }
                    other => panic!(
                        "Expected ImportError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected ImportError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_stop_iteration {
    ($exc:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
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
    }};
}

macro_rules! assert_div_by_zero_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
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
            $crate::domain::MemphisException {
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
                    $crate::domain::MemphisValue::Str(s) => {
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

macro_rules! assert_syntax_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
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
            $crate::domain::MemphisException {
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
                    $crate::domain::MemphisValue::Str(s) => {
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

macro_rules! assert_runtime_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::domain::MemphisException {
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
            $crate::domain::MemphisException {
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
                    $crate::domain::MemphisValue::Str(s) => {
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

pub(crate) use assert_div_by_zero_error;
pub(crate) use assert_import_error;
pub(crate) use assert_index_error;
pub(crate) use assert_name_error;
pub(crate) use assert_runtime_error;
pub(crate) use assert_stop_iteration;
pub(crate) use assert_syntax_error;
pub(crate) use assert_type_error;
