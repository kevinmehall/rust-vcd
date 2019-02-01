//! This crate reads and writes [VCD (Value Change Dump)][wp] files, a common format used with
//! logic analyzers, HDL simulators, and other EDA tools.
//! [wp]: https://en.wikipedia.org/wiki/Value_change_dump
//!
//! ## Example
//!
//! ```
//! use std::io;
//! use std::io::ErrorKind::InvalidInput;
//! use vcd::{ self, Value, TimescaleUnit, SimulationCommand };
//!
//! /// Write out a clocked signal to a VCD file
//! fn write_clocked_vcd(shift_reg: u32, w: &mut io::Write) -> io::Result<()> {
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
//! fn read_clocked_vcd(r: &mut io::Read) -> io::Result<u32> {
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
//!      use vcd::Command::*;
//!      let command = command_result?;
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

use std::str::FromStr;
use std::fmt::{self, Display};
use std::error::Error;
use std::io;

mod read;
pub use read::Parser;

mod write;
pub use write::Writer;

/// A unit of time for the `$timescale` command.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TimescaleUnit {
    S, MS, US, NS, PS, FS,
}

/// Error wrapping a static string message explaining why parsing failed.
#[derive(Debug)]
pub struct InvalidData(&'static str);
impl Display for InvalidData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.0.fmt(f) }
}
impl Error for InvalidData {
    fn description(&self) -> &str { self.0 }
}
impl From<InvalidData> for io::Error {
    fn from(e: InvalidData) -> io::Error { io::Error::new(io::ErrorKind::InvalidData, e.0) }
}

impl FromStr for TimescaleUnit {
    type Err = InvalidData;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::TimescaleUnit::*;
        match s {
            "s"  => Ok(S),
            "ms" => Ok(MS),
            "us" => Ok(US),
            "ns" => Ok(NS),
            "ps" => Ok(PS),
            "fs" => Ok(FS),
            _ => Err(InvalidData("invalid timescale unit"))
        }
    }
}

impl Display for TimescaleUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TimescaleUnit::*;
        write!(f, "{}", match *self {
            S  => "s",
            MS => "ms",
            US => "us",
            NS => "ns",
            PS => "ps",
            FS => "fs",
        })
    }
}

impl TimescaleUnit {
    pub fn divisor(&self) -> u64 {
        use self::TimescaleUnit::*;
        match *self {
            S  => 1,
            MS => 1_000,
            US => 1_000_000,
            NS => 1_000_000_000,
            PS => 1_000_000_000_000,
            FS => 1_000_000_000_000_000,
        }
    }

    pub fn fraction(&self) -> f64 {
        1.0 / (self.divisor() as f64)
    }
 }

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
    fn parse(v: u8) -> Result<Value, InvalidData> {
        use Value::*;
        match v {
            b'0' => Ok(V0),
            b'1' => Ok(V1),
            b'x' | b'X' => Ok(X),
            b'z' | b'Z' => Ok(Z),
            _ => Err(InvalidData("invalid VCD value"))
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
        if v { Value::V1 } else { Value::V0 }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        write!(f, "{}", match *self {
            V0 => "0",
            V1 => "1",
            X => "x",
            Z => "z",
        })
    }
}

/// A type of scope, as used in the `$scope` command.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeType {
    Module,
    Task,
    Function,
    Begin,
    Fork,
}

impl FromStr for ScopeType {
    type Err = InvalidData;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::ScopeType::*;
        match s {
            "module" => Ok(Module),
            "task" => Ok(Task),
            "function" => Ok(Function),
            "begin" => Ok(Begin),
            "fork" => Ok(Fork),
            _ => Err(InvalidData("invalid scope type"))
        }
    }
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ScopeType::*;
        write!(f, "{}", match *self {
            Module => "module",
            Task  => "task",
            Function => "function",
            Begin  => "begin",
            Fork  => "fork",
        })
    }
}

/// A type of variable, as used in the `$var` command.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarType {
    Event,
    Integer,
    Parameter,
    Real,
    Reg,
    Supply0,
    Supply1,
    Time,
    Tri,
    TriAnd,
    TriOr,
    TriReg,
    Tri0,
    Tri1,
    WAnd,
    Wire,
    WOr,
}

impl FromStr for VarType {
    type Err = InvalidData;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::VarType::*;
        match s {
            "event" => Ok(Event),
            "integer" => Ok(Integer),
            "parameter" => Ok(Parameter),
            "real" => Ok(Real),
            "reg" => Ok(Reg),
            "supply0" => Ok(Supply0),
            "supply1" => Ok(Supply1),
            "time" => Ok(Time),
            "tri" => Ok(Tri),
            "triand" => Ok(TriAnd),
            "trior" => Ok(TriOr),
            "trireg" => Ok(TriReg),
            "tri0" => Ok(Tri0),
            "tri1" => Ok(Tri1),
            "wand" => Ok(WAnd),
            "wire" => Ok(Wire),
            "wor" => Ok(WOr),
            _ => Err(InvalidData("invalid variable type"))
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::VarType::*;
        write!(f, "{}", match *self {
            Event => "event",
            Integer => "integer",
            Parameter => "parameter",
            Real => "real",
            Reg => "reg",
            Supply0 => "supply0",
            Supply1 => "supply1",
            Time => "time",
            Tri => "tri",
            TriAnd => "triand",
            TriOr => "trior",
            TriReg => "trireg",
            Tri0 => "tri0",
            Tri1 => "tri1",
            WAnd => "wand",
            Wire => "wire",
            WOr => "wor",
        })
    }
}

/// An ID used within the file to refer to a particular variable.
#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct IdCode(u64);

const ID_CHAR_MIN: u8 = b'!';
const ID_CHAR_MAX: u8 = b'~';
const NUM_ID_CHARS: u64 = (ID_CHAR_MAX - ID_CHAR_MIN + 1) as u64;

impl IdCode {
    fn new(v: &[u8]) -> Result<IdCode, InvalidData> {
        if v.is_empty() { return Err(InvalidData("ID cannot be empty")) }
        let mut result = 0u64;
        for &i in v.iter().rev() {
            if i < ID_CHAR_MIN || i > ID_CHAR_MAX { return Err(InvalidData("invalid characters in ID")) }
            let c = ((i - ID_CHAR_MIN) as u64) + 1;
            result = result.checked_mul(NUM_ID_CHARS).and_then(|x| x.checked_add(c))
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
    fn from(i: u32) -> IdCode { IdCode(i as u64) }
}

impl From<u64> for IdCode {
    fn from(i: u64) -> IdCode { IdCode(i) }
}

impl Display for IdCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut i = self.0;
        loop {
            let r = i % NUM_ID_CHARS;
            try!(write!(f, "{}", (r as u8 + ID_CHAR_MIN) as char));
            if i < NUM_ID_CHARS { break; }
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
    assert_eq!("!!!!!!!!!!".parse::<IdCode>().unwrap().to_string(), "!!!!!!!!!!");
    assert_eq!("~".parse::<IdCode>().unwrap().to_string(), "~");
    assert_eq!("~~~~~~~~~".parse::<IdCode>().unwrap().to_string(), "~~~~~~~~~");
    assert_eq!("n999999999".parse::<IdCode>().unwrap().to_string(), "n999999999");
    assert!("n9999999999".parse::<IdCode>().is_err());
}

/// Information on a VCD scope as represented by a `$scope` command and its children.
#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub identifier: String,
    pub children: Vec<ScopeItem>
}

impl Scope {
    /// Looks up a variable by reference.
    pub fn find_var<'a>(&'a self, reference: &str) -> Option<&'a Var> {
        for c in &self.children {
            if let &ScopeItem::Var(ref v) = c {
                if v.reference == reference {
                    return Some(v)
                }
            }
        }
        None
    }
}

impl Default for Scope {
    fn default() -> Scope {
        Scope { scope_type: ScopeType::Module, identifier: "".to_string(), children: Vec::new() }
    }
}

/// Index of a VCD variable reference, either a bit select index `[i]` or a range index `[msb:lsb]`
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReferenceIndex {
    BitSelect(u32),
    Range(u32, u32),
}

impl FromStr for ReferenceIndex {
    type Err = std::io::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_start_matches('[').trim_end_matches(']');
        use ReferenceIndex::*;
        use io::{Error, ErrorKind};
        match s.find(':') {
            Some(idx) => {
                let msb: u32 = s[..idx].trim().parse().map_err(|e| Error::new(ErrorKind::InvalidData, e))?;
                let lsb: u32 = s[idx..].trim_start_matches(':').trim().parse().map_err(|e| Error::new(ErrorKind::InvalidData, e))?;
                Ok(Range(msb, lsb))
            },
            None => {
                let idx  = s.trim().parse().map_err(|e| Error::new(ErrorKind::InvalidData, e))?;
                Ok(BitSelect(idx))
            }
        }
    }
}

impl Display for ReferenceIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ReferenceIndex::*;
        match self {
            BitSelect(idx) => write!(f, "[{}]", idx)?,
            Range(msb, lsb) => write!(f, "[{}:{}]", msb, lsb)?,
        };
        Ok(())
    }
}

/// Information on a VCD variable as represented by a `$var` command.
#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub var_type: VarType,
    pub size: u32,
    pub code: IdCode,
    pub reference: String,
    pub index: Option<ReferenceIndex>,
}

/// An item in a scope -- either a child scope or a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeItem {
    Scope(Scope),
    Var(Var),
}

/// An element in a VCD file.
#[derive(Debug, PartialEq, Clone)]
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
    ChangeVector(IdCode, Vec<Value>),

    /// A `r1.234 a` change to a real variable
    ChangeReal(IdCode, f64),

    /// A `sSTART a` change to a (real?) variable
    ChangeString(IdCode, String),

    /// A beginning of a simulation command. Unlike header commands, which are parsed atomically,
    /// simulation commands emit a Begin, followed by the data changes within them, followed by
    /// End.
    Begin(SimulationCommand),

    /// An end of a simulation command.
    End(SimulationCommand)
}

/// A simulation command type, used in `Command::Begin` and `Command::End`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SimulationCommand {
    Dumpall,
    Dumpoff,
    Dumpon,
    Dumpvars,
}

impl Display for SimulationCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SimulationCommand::*;
        write!(f, "{}", match *self {
            Dumpall  => "dumpall",
            Dumpoff  => "dumpoff",
            Dumpon   => "dumpon",
            Dumpvars => "dumpvars",
        })
    }
}

/// Structure containing the data from the header of a VCD file.
#[derive(Debug, Default)]
pub struct Header {
    pub comment: Option<String>,
    pub date: Option<String>,
    pub version: Option<String>,
    pub timescale: Option<(u32, TimescaleUnit)>,
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
        where S: std::borrow::Borrow<str>
    {
        fn find_nested_scope<'a, S>(mut scope: &'a Scope, mut path: &[S]) -> Option<&'a Scope>
            where S: std::borrow::Borrow<str>
        {
            'deeper: while !path.is_empty() {
                for child in &scope.children {
                    match child {
                        ScopeItem::Scope(ref new_scope) if new_scope.identifier == path[0].borrow() => {
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
            _ => false
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
        where S: std::borrow::Borrow<str>
    {
        let scope = self.find_scope(&path[..path.len()-1])?;
        scope.find_var(path[path.len()-1].borrow())
    }
}
