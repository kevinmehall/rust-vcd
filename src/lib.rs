use std::str::FromStr;
use std::fmt::{self, Display};

pub mod read;
use read::Error;

pub mod write;

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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Value {
    V0,
    V1,
    X,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum VarType {
    Wire,
}

impl FromStr for VarType {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::VarType::*;
        match s {
            "wire" => Ok(Wire),
            _ => Err(Error::Parse("Invalid scope type"))
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::VarType::*;
        write!(f, "{}", match *self {
            Wire => "wire",
        })
    }
}

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

#[derive(Debug, Clone)]
pub struct Var {
    pub var_type: VarType,
    pub size: u32,
    pub code: IdCode,
    pub reference: String,
}

#[derive(Debug, Clone)]
pub enum ScopeItem {
    Scope(Scope),
    Var(Var),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    Comment(String),
    Date(String),
    Version(String),
    Timescale(u32, TimescaleUnit),
    ScopeDef(ScopeType, String),
    Upscope,
    VarDef(VarType, u32, IdCode, String),
    Enddefinitions,
    Timestamp(u64),
    ChangeScalar(IdCode, Value),
    ChangeVector(IdCode, Vec<Value>),
    ChangeReal(IdCode, f64),
    Begin(SimulationCommand),
    End(SimulationCommand)
}

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

#[derive(Debug, Default)]
pub struct Header {
    pub comment: Option<String>,
    pub date: Option<String>,
    pub version: Option<String>,
    pub timescale: Option<(u32, TimescaleUnit)>,
    pub scope: Scope,
}
