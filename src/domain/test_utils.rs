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

pub(crate) use assert_import_error;
pub(crate) use assert_name_error;
pub(crate) use assert_stop_iteration;
pub(crate) use assert_type_error;
