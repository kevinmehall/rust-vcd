use std::str::FromStr;
use std::fmt::{self, Display};

mod read;
pub use read::{Error, Parser};

mod write;
pub use write::Writer;

/// A unit of time for the `$timescale` command
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TimescaleUnit {
    S, MS, US, NS, PS, FS,
}

impl FromStr for TimescaleUnit {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::TimescaleUnit::*;
        match s {
            "s"  => Ok(S),
            "ms" => Ok(MS),
            "us" => Ok(US),
            "ns" => Ok(NS),
            "ps" => Ok(PS),
            "fs" => Ok(FS),
            _ => Err(Error::Parse("Invalid timescale unit"))
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

/// A VCD scalar value
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Value {
    /// Logic high (prefixed with `V` to make a valid Rust identifier
    V0,

    /// Logic low (prefixed with `V` to make a valid Rust identifier
    V1,

    /// An uninitialized or unknown value
    X,

    /// The "high-impedance" value
    Z,
}

impl Value {
    fn parse(v: u8) -> Result<Value, Error> {
        use Value::*;
        match v {
            b'0' => Ok(V0),
            b'1' => Ok(V1),
            b'x' | b'X' => Ok(X),
            b'z' | b'Z' => Ok(Z),
            _ => Err(Error::Parse("Invalid wire value"))
        }
    }
}

impl FromStr for Value {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Value::parse(*s.as_bytes().get(0).unwrap_or(&b' '))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        write!(f, "{}", match *self {
            V0  => "0",
            V1 => "1",
            X => "x",
            Z => "z",
        })
    }
}

/// A type of scope, as used in the `$scope` command
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeType {
    Module,
    Task,
    Function,
    Begin,
    Fork,
}

impl FromStr for ScopeType {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::ScopeType::*;
        match s {
            "module" => Ok(Module),
            "task" => Ok(Task),
            "function" => Ok(Function),
            "begin" => Ok(Begin),
            "fork" => Ok(Fork),
            _ => Err(Error::Parse("Invalid scope type"))
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

/// A type of variable, as used in the `$var` command
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarType {
    //Event,
    //Integer,
    //Parameter,
    Real,
    Reg,
    //Supply0,
    //Supply1,
    //Time,
    //Tri,
    //Triant,
    //Trior,
    //Trireg,
    //Tri0,
    //Tri1,
    //Wand,
    Wire,
    //Wor,
}

impl FromStr for VarType {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::VarType::*;
        match s {
            "wire" => Ok(Wire),
            "reg" => Ok(Reg),
            "real" => Ok(Real),
            _ => Err(Error::Parse("Invalid var type"))
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::VarType::*;
        write!(f, "{}", match *self {
            Wire => "wire",
            Reg => "reg",
            Real => "real",
        })
    }
}

/// An ID used within the file to refer to a particular variable.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct IdCode(u32);

impl IdCode {
    fn new(v: &[u8]) -> Result<IdCode, Error> {
        let mut result = 0u32;
        for &i in v {
            result = result << 7 | ((i - b'!') as u32);
        }
        Ok(IdCode(result))
    }
}

impl FromStr for IdCode {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        IdCode::new(s.as_bytes())
    }
}

impl From<u32> for IdCode {
    fn from(i: u32) -> IdCode { IdCode(i) }
}

impl Display for IdCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut i = self.0;
        loop {
            try!(write!(f, "{}", ((i&0x7f) as u8 + b'!') as char));
            i = i >> 7;
            if i == 0 { break; }
        }
        Ok(())
    }
}

/// Information on a VCD scope as represented by a `$scope` command and its children
#[derive(Debug, Clone)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub identifier: String,
    pub children: Vec<ScopeItem>
}

impl Default for Scope {
    fn default() -> Scope {
        Scope { scope_type: ScopeType::Module, identifier: "".to_string(), children: Vec::new() }
    }
}

/// Information on a VCD variable as represented by a `$var` command.
#[derive(Debug, Clone)]
pub struct Var {
    pub var_type: VarType,
    pub size: u32,
    pub code: IdCode,
    pub reference: String,
}

/// An item in a scope -- either a child scope or a variable
#[derive(Debug, Clone)]
pub enum ScopeItem {
    Scope(Scope),
    Var(Var),
}

/// An element in a VCD file
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
    VarDef(VarType, u32, IdCode, String),

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

/// A simulation command type, used in Command::Begin and Command::End
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

/// Structure containing the data from the header of a VCD file
#[derive(Debug, Default)]
pub struct Header {
    pub comment: Option<String>,
    pub date: Option<String>,
    pub version: Option<String>,
    pub timescale: Option<(u32, TimescaleUnit)>,
    pub scope: Scope,
}
