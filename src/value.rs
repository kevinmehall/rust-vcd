use std::fmt::{self, Display};
use std::str::FromStr;

use super::InvalidData;

/// A four-valued logic scalar value.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Value {
    /// Logic low (prefixed with `V` to make a valid Rust identifier)
    V0,

    /// Logic high (prefixed with `V` to make a valid Rust identifier)
    V1,

    /// An uninitialized or unknown value
    X,

    /// The "high-impedance" value
    Z,
}

impl Value {
    pub(crate) fn parse(v: u8) -> Result<Value, InvalidData> {
        use Value::*;
        match v {
            b'0' => Ok(V0),
            b'1' => Ok(V1),
            b'x' | b'X' => Ok(X),
            b'z' | b'Z' => Ok(Z),
            _ => Err(InvalidData("invalid VCD value")),
        }
    }
}

impl FromStr for Value {
    type Err = InvalidData;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Value::parse(*s.as_bytes().get(0).unwrap_or(&b' '))
    }
}

impl From<bool> for Value {
    /// `true` converts to `V1`, `false` to `V0`
    fn from(v: bool) -> Value {
        if v {
            Value::V1
        } else {
            Value::V0
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        write!(
            f,
            "{}",
            match *self {
                V0 => "0",
                V1 => "1",
                X => "x",
                Z => "z",
            }
        )
    }
}
