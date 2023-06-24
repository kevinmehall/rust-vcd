use std::fmt::{self, Display};
use std::iter::FromIterator;
use std::str::FromStr;

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

crate::unit_error_struct!(InvalidValue, "invalid VCD logic value");

impl Value {
    pub(crate) fn parse(v: u8) -> Result<Value, InvalidValue> {
        use Value::*;
        match v {
            b'0' => Ok(V0),
            b'1' => Ok(V1),
            b'x' | b'X' => Ok(X),
            b'z' | b'Z' => Ok(Z),
            _ => Err(InvalidValue),
        }
    }
}

impl FromStr for Value {
    type Err = InvalidValue;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.as_bytes() {
            &[c] => Value::parse(c),
            _ => Err(InvalidValue)
        }
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

/// Vector of `Value`
/// 
/// This currently wraps a `Vec<Value>` but could be implemented with
/// a bitmap in the future.
#[derive(Clone, PartialEq, Eq)]
pub struct Vector(Vec<Value>);

impl Vector {
    /// Returns the number of bits in the vector.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns an iterator over the values in the vector.
    pub fn iter(&self) -> VectorIter {
        VectorIter(self.0.iter())
    }

    /// Returns a `Vector` of the specified `width` filled with the value `v`
    pub fn filled(v: Value, width: usize) -> Vector {
        Vector(std::iter::repeat(v).take(width).collect())
    }

    /// Returns a vector of `width` zeros
    pub fn zeros(width: usize) -> Vector {
        Vector::filled(Value::V0, width)
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for v in self {
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vector({})", self)
    }
}

impl FromStr for Vector {
    type Err = InvalidValue;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.bytes().map(Value::parse).collect()
    }
}

impl From<Vec<Value>> for Vector {
    fn from(v: Vec<Value>) -> Self {
        Vector(v)
    }
}

impl From<Vector> for Vec<Value> {
    fn from(v: Vector) -> Self {
        v.0
    }
}

impl<const N: usize> From<[Value; N]> for Vector {
    fn from(v: [Value; N]) -> Self {
        Vector(v.into())
    }
}

/// Iterator for a `Vector`
pub struct VectorIter<'a>(std::slice::Iter<'a, Value>);

impl<'a> IntoIterator for &'a Vector {
    type Item = Value;

    type IntoIter = VectorIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> Iterator for VectorIter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().copied()
    }
}

impl FromIterator<Value> for Vector {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Vector(iter.into_iter().collect())
    }
}

#[test]
fn test_vector_parse() {
    assert_eq!("1010".parse::<Vector>().unwrap(), [Value::V1, Value::V0, Value::V1, Value::V0].into());
    assert_eq!("xz".parse::<Vector>().unwrap(), [Value::X, Value::Z].into());
    assert_eq!("X".parse::<Vector>().unwrap(), [Value::X].into());
}

#[test]
fn test_vector_display() {
    assert_eq!(format!("{}", Vector::from_iter([Value::V0, Value::V1, Value::X, Value::Z])), "01xz");
}
