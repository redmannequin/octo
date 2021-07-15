use std::{error, fmt, num};

#[derive(Debug, Clone)]
pub enum ParseError {
    ExpectedExpression(String),
    UnexpectedToken { expected: String, found: String },
    InternalError(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::ExpectedExpression(found) => {
                write!(f, "Expected Expression, Found: '{}'", found)
            }
            ParseError::UnexpectedToken { expected, found } => {
                write!(f, "Expected Token(s): {}. Found: {}", expected, found,)
            }
            ParseError::InternalError(err) => write!(f, "Internal Error: {}", err),
        }
    }
}

impl error::Error for ParseError {}

macro_rules! internal_error {
    ($T:ty, $E:ty) => {
        impl From<$E> for $T {
            fn from(err: $E) -> Self {
                Self::InternalError(err.to_string())
            }
        }
    };
}

internal_error!(ParseError, num::ParseFloatError);
internal_error!(ParseError, std::str::ParseBoolError);
