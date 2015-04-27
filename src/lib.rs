use std::str::FromStr;

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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct IdCode(u32);

impl IdCode {
    fn new(v: &[u8]) -> Result<IdCode, Error> {
        let mut result = 0u32;
        for &i in v {
            result = result * 128 + (i - b'!') as u32;
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

#[derive(Debug, Clone)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub identifier: String,
    pub children: Vec<ScopeItem>
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SimulationCommand {
    Dumpall,
    Dumpoff,
    Dumpon,
    Dumpvars,
}

#[derive(Debug, Default)]
pub struct Header {
    pub comment: Option<String>,
    pub date: Option<String>,
    pub version: Option<String>,
    pub timescale: Option<(u32, TimescaleUnit)>,
    pub module: Option<Scope>,
}
