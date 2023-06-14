use std::fmt::{self, Display};
use std::str::FromStr;

use super::InvalidData;

/// A unit of time for the `$timescale` command.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TimescaleUnit {
    S,
    MS,
    US,
    NS,
    PS,
    FS,
}

impl FromStr for TimescaleUnit {
    type Err = InvalidData;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use TimescaleUnit::*;
        match s {
            "s" => Ok(S),
            "ms" => Ok(MS),
            "us" => Ok(US),
            "ns" => Ok(NS),
            "ps" => Ok(PS),
            "fs" => Ok(FS),
            _ => Err(InvalidData("invalid timescale unit")),
        }
    }
}

impl Display for TimescaleUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TimescaleUnit::*;
        write!(
            f,
            "{}",
            match *self {
                S => "s",
                MS => "ms",
                US => "us",
                NS => "ns",
                PS => "ps",
                FS => "fs",
            }
        )
    }
}

impl TimescaleUnit {
    /// The number of timescale ticks per second.
    pub fn divisor(&self) -> u64 {
        use TimescaleUnit::*;
        match *self {
            S => 1,
            MS => 1_000,
            US => 1_000_000,
            NS => 1_000_000_000,
            PS => 1_000_000_000_000,
            FS => 1_000_000_000_000_000,
        }
    }

    /// The duration of a timescale tick in seconds.
    pub fn fraction(&self) -> f64 {
        1.0 / (self.divisor() as f64)
    }
}
