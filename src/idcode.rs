use std::fmt::{self, Display};
use std::str::FromStr;

use crate::InvalidData;

/// An ID used within the file to refer to a particular variable.
#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct IdCode(u64);

const ID_CHAR_MIN: u8 = b'!';
const ID_CHAR_MAX: u8 = b'~';
const NUM_ID_CHARS: u64 = (ID_CHAR_MAX - ID_CHAR_MIN + 1) as u64;

impl IdCode {
    fn new(v: &[u8]) -> Result<IdCode, InvalidData> {
        if v.is_empty() {
            return Err(InvalidData("ID cannot be empty"));
        }
        let mut result = 0u64;
        for &i in v.iter().rev() {
            if i < ID_CHAR_MIN || i > ID_CHAR_MAX {
                return Err(InvalidData("invalid characters in ID"));
            }
            let c = ((i - ID_CHAR_MIN) as u64) + 1;
            result = result
                .checked_mul(NUM_ID_CHARS)
                .and_then(|x| x.checked_add(c))
                .ok_or(InvalidData("ID too long"))?;
        }
        Ok(IdCode(result - 1))
    }

    /// An arbitrary IdCode with a short representation.
    pub const FIRST: IdCode = IdCode(0);

    /// Returns the IdCode following this one in an arbitrary sequence.
    pub fn next(&self) -> IdCode {
        IdCode(self.0 + 1)
    }
}

impl FromStr for IdCode {
    type Err = InvalidData;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        IdCode::new(s.as_bytes())
    }
}

impl From<u32> for IdCode {
    fn from(i: u32) -> IdCode {
        IdCode(i as u64)
    }
}

impl From<u64> for IdCode {
    fn from(i: u64) -> IdCode {
        IdCode(i)
    }
}

impl Display for IdCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut i = self.0;
        loop {
            let r = i % NUM_ID_CHARS;
            write!(f, "{}", (r as u8 + ID_CHAR_MIN) as char)?;
            if i < NUM_ID_CHARS {
                break;
            }
            i = i / NUM_ID_CHARS - 1;
        }
        Ok(())
    }
}

#[test]
fn test_id_code() {
    let mut id = IdCode::FIRST;
    for i in 0..10000 {
        println!("{} {}", i, id);
        assert_eq!(id.to_string().parse::<IdCode>().unwrap(), id);
        id = id.next();
    }

    assert_eq!("!".parse::<IdCode>().unwrap().to_string(), "!");
    assert_eq!(
        "!!!!!!!!!!".parse::<IdCode>().unwrap().to_string(),
        "!!!!!!!!!!"
    );
    assert_eq!("~".parse::<IdCode>().unwrap().to_string(), "~");
    assert_eq!(
        "~~~~~~~~~".parse::<IdCode>().unwrap().to_string(),
        "~~~~~~~~~"
    );
    assert_eq!(
        "n999999999".parse::<IdCode>().unwrap().to_string(),
        "n999999999"
    );
    assert!("n9999999999".parse::<IdCode>().is_err());
}
