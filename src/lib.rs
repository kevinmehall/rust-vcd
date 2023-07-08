//! This crate reads and writes [VCD (Value Change Dump)][wp] files, a common
//! format used with logic analyzers, HDL simulators, and other EDA tools.
//!
//! [wp]: https://en.wikipedia.org/wiki/Value_change_dump
//! 
//! It provides:
//!  * A [`Parser`] wraps [`std::io::BufRead`] and provides the ability to parse a
//!    VCD [`Header`] and [`Command`]s.
//!  * A [`Writer`] that allows writing VCD to a [`std::io::Write`].
//!  * Several structs and enums representing the elements of a VCD file that
//!    can be used with the `Parser` or `Writer`, or individually by
//!    implementing [`FromStr`][`std::str::FromStr`] and [`Display`].
//!
//! ## Example
//!
//! ```
//! use std::io;
//! use std::io::ErrorKind::InvalidInput;
//! use vcd::{ self, Value, TimescaleUnit, SimulationCommand };
//!
//! /// Write out a clock and data signal to a VCD file
//! fn write_clocked_vcd(shift_reg: u32, w: &mut dyn io::Write) -> io::Result<()> {
//!   let mut writer = vcd::Writer::new(w);
//!
//!   // Write the header
//!   writer.timescale(1, TimescaleUnit::US)?;
//!   writer.add_module("top")?;
//!   let clock = writer.add_wire(1, "clock")?;
//!   let data = writer.add_wire(1, "data")?;
//!   writer.upscope()?;
//!   writer.enddefinitions()?;
//!
//!   // Write the initial values
//!   writer.begin(SimulationCommand::Dumpvars)?;
//!   writer.change_scalar(clock, Value::V0)?;
//!   writer.change_scalar(data, Value::V0)?;
//!   writer.end()?;
//!
//!   // Write the data values
//!   let mut t = 0;
//!   for i in 0..32 {
//!     t += 4;
//!     writer.timestamp(t)?;
//!     writer.change_scalar(clock, Value::V1)?;
//!     writer.change_scalar(data, ((shift_reg >> i) & 1) != 0)?;
//!
//!     t += 4;
//!     writer.timestamp(t)?;
//!     writer.change_scalar(clock, Value::V0)?;
//!   }
//!   Ok(())
//! }
//!
//! /// Parse a VCD file containing a clocked signal and decode the signal
//! fn read_clocked_vcd(r: &mut dyn io::BufRead) -> io::Result<u32> {
//!    let mut parser = vcd::Parser::new(r);
//!
//!    // Parse the header and find the wires
//!    let header = parser.parse_header()?;
//!    let clock = header.find_var(&["top", "clock"])
//!       .ok_or_else(|| io::Error::new(InvalidInput, "no wire top.clock"))?.code;
//!    let data = header.find_var(&["top", "data"])
//!       .ok_or_else(|| io::Error::new(InvalidInput, "no wire top.data"))?.code;
//!
//!    // Iterate through the remainder of the file and decode the data
//!    let mut shift_reg = 0;
//!    let mut data_val = Value::X;
//!    let mut clock_val = Value::X;
//!
//!    for command_result in parser {
//!      let command = command_result?;
//!      use vcd::Command::*;
//!      match command {
//!        ChangeScalar(i, v) if i == clock => {
//!          if clock_val == Value::V1 && v == Value::V0 { // falling edge on clock
//!             let shift_bit = match data_val { Value::V1 => (1 << 31), _ => 0 };
//!             shift_reg = (shift_reg >> 1) | shift_bit;
//!          }
//!          clock_val = v;
//!        }
//!        ChangeScalar(i, v) if i == data => {
//!          data_val = v;
//!        }
//!        _ => (),
//!      }
//!    }
//!
//!    Ok(shift_reg)
//! }
//!
//! let mut buf = Vec::new();
//! let data = 0xC0DE1234;
//! write_clocked_vcd(data, &mut buf).expect("Failed to write");
//! let value = read_clocked_vcd(&mut &buf[..]).expect("Failed to read");
//! assert_eq!(value, data);
//! ```
#![warn(missing_docs)]

use std::fmt::{self, Display};

mod parser;
pub use parser::{ Parser, ParseError, ParseErrorKind };

mod write;
pub use write::Writer;

mod idcode;
pub use idcode::{IdCode, InvalidIdCode};

mod value;
pub use value::{ Value, Vector, VectorIter, InvalidValue };

mod timescale;
pub use timescale::{ TimescaleUnit, InvalidTimescaleUnit };

mod scope;
pub use scope::{
    Scope,
    ScopeItem,
    ScopeType,
    InvalidScopeType,
    Var,
    VarType,
    InvalidVarType,
    ReferenceIndex,
    InvalidReferenceIndex,
};

macro_rules! unit_error_struct {
    ($name:ident, $err:literal) => {
        #[doc = concat!("Parse error for ", $err, ".")]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        #[non_exhaustive]
        pub struct $name;

        impl Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, $err)
            }
        }

        impl std::error::Error for $name {}
    };
}
pub(crate) use unit_error_struct;

/// An element in a VCD file.
#[derive(Debug, PartialEq, Clone)]
#[non_exhaustive]
pub enum Command {
    /// A `$comment` command
    Comment(String),

    /// A `$date` command
    Date(String),

    /// A `$version` command
    Version(String),

    /// A `$timescale` command
    Timescale(u32, TimescaleUnit),

    /// A `$scope` command
    ScopeDef(ScopeType, String),

    /// An `$upscope` command
    Upscope,

    /// A `$var` command
    VarDef(VarType, u32, IdCode, String, Option<ReferenceIndex>),

    /// An `$enddefinitions` command
    Enddefinitions,

    /// A `#xxx` timestamp
    Timestamp(u64),

    /// A `0a` change to a scalar variable
    ChangeScalar(IdCode, Value),

    /// A `b0000 a` change to a vector variable
    ChangeVector(IdCode, Vector),

    /// A `r1.234 a` change to a real variable
    ChangeReal(IdCode, f64),

    /// A `sSTART a` change to a string variable
    ChangeString(IdCode, String),

    /// A beginning of a simulation command. Unlike header commands, which are parsed atomically,
    /// simulation commands emit a Begin, followed by the data changes within them, followed by
    /// End.
    Begin(SimulationCommand),

    /// An end of a simulation command.
    End(SimulationCommand),
}

/// A simulation command type, used in `Command::Begin` and `Command::End`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum SimulationCommand {
    Dumpall,
    Dumpoff,
    Dumpon,
    Dumpvars,
}

impl Display for SimulationCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SimulationCommand::*;
        write!(
            f,
            "{}",
            match *self {
                Dumpall => "dumpall",
                Dumpoff => "dumpoff",
                Dumpon => "dumpon",
                Dumpvars => "dumpvars",
            }
        )
    }
}

/// Structure containing the data from the header of a VCD file.
/// 
/// A `Header` can be parsed from VCD with [`Parser::parse_header`], or create an
/// empty `Header` with [`Header::default`].
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct Header {
    /// `$date` text
    pub date: Option<String>,

    /// `$version` text
    pub version: Option<String>,

    /// Parsed `$timescale` indicating the time unit used in the file
    pub timescale: Option<(u32, TimescaleUnit)>,

    /// Top-level variables, scopes, and comments
    pub items: Vec<ScopeItem>,
}

impl Header {
    /// Find the scope object at a specified path.
    ///
    /// ## Example
    ///
    /// ```rust
    /// let mut parser = vcd::Parser::new(&b"
    /// $scope module a $end
    /// $scope module b $end
    /// $var integer 16 n0 counter $end
    /// $upscope $end
    /// $upscope $end
    /// $enddefinitions $end
    /// "[..]);
    /// let header = parser.parse_header().unwrap();
    /// let scope = header.find_scope(&["a", "b"]).unwrap();
    /// assert_eq!(scope.identifier, "b");
    /// ```
    pub fn find_scope<S>(&self, path: &[S]) -> Option<&Scope>
    where
        S: std::borrow::Borrow<str>,
    {
        fn find_nested_scope<'a, S>(mut scope: &'a Scope, mut path: &[S]) -> Option<&'a Scope>
        where
            S: std::borrow::Borrow<str>,
        {
            'deeper: while !path.is_empty() {
                for child in &scope.children {
                    match child {
                        ScopeItem::Scope(ref new_scope)
                            if new_scope.identifier == path[0].borrow() =>
                        {
                            scope = new_scope;
                            path = &path[1..];
                            continue 'deeper;
                        }
                        _ => (),
                    }
                }
                return None;
            }
            Some(scope)
        }

        if path.is_empty() {
            return None;
        }

        let scope = self.items.iter().find(|item| match item {
            ScopeItem::Scope(scope) => scope.identifier == path[0].borrow(),
            _ => false,
        });

        if let Some(ScopeItem::Scope(scope)) = scope {
            find_nested_scope(scope, &path[1..])
        } else {
            None
        }
    }

    /// Find the variable object at a specified path.
    ///
    /// ## Example
    ///
    /// ```rust
    /// let mut parser = vcd::Parser::new(&b"
    /// $scope module a $end
    /// $scope module b $end
    /// $var integer 16 n0 counter $end
    /// $upscope $end
    /// $upscope $end
    /// $enddefinitions $end
    /// "[..]);
    /// let header = parser.parse_header().unwrap();
    /// let var = header.find_var(&["a", "b", "counter"]).unwrap();
    /// assert_eq!(var.reference, "counter");
    /// ```
    pub fn find_var<S>(&self, path: &[S]) -> Option<&Var>
    where
        S: std::borrow::Borrow<str>,
    {
        let scope = self.find_scope(&path[..path.len() - 1])?;
        scope.find_var(path[path.len() - 1].borrow())
    }
}
