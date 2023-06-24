use std::fmt::{self, Display};
use std::str::FromStr;

use crate::IdCode;

/// A type of scope, as used in the `$scope` command.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub enum ScopeType {
    Module,
    Task,
    Function,
    Begin,
    Fork,
}

crate::unit_error_struct!(InvalidScopeType, "invalid scope type");

impl FromStr for ScopeType {
    type Err = InvalidScopeType;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use ScopeType::*;
        match s {
            "module" => Ok(Module),
            "task" => Ok(Task),
            "function" => Ok(Function),
            "begin" => Ok(Begin),
            "fork" => Ok(Fork),
            _ => Err(InvalidScopeType),
        }
    }
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ScopeType::*;
        write!(
            f,
            "{}",
            match *self {
                Module => "module",
                Task => "task",
                Function => "function",
                Begin => "begin",
                Fork => "fork",
            }
        )
    }
}

/// A type of variable, as used in the `$var` command.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
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
    String,
}

crate::unit_error_struct!(InvalidVarType, "invalid variable type");

impl FromStr for VarType {
    type Err = InvalidVarType;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use VarType::*;
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
            "string" => Ok(String),
            _ => Err(InvalidVarType),
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use VarType::*;
        write!(
            f,
            "{}",
            match *self {
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
                String => "string",
            }
        )
    }
}

/// Information on a VCD scope as represented by a `$scope` command and its children.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub struct Scope {
    pub scope_type: ScopeType,
    pub identifier: String,
    pub children: Vec<ScopeItem>,
}

impl Scope {
    pub fn new(scope_type: ScopeType, identifier: String) -> Self {
        Self { scope_type, identifier, children: Vec::new() }
    }

    /// Looks up a variable by reference.
    pub fn find_var<'a>(&'a self, reference: &str) -> Option<&'a Var> {
        for c in &self.children {
            if let ScopeItem::Var(v) = c {
                if v.reference == reference {
                    return Some(v);
                }
            }
        }
        None
    }
}

impl Default for Scope {
    fn default() -> Scope {
        Scope {
            scope_type: ScopeType::Module,
            identifier: "".to_string(),
            children: Vec::new(),
        }
    }
}

/// Index of a VCD variable reference, either a bit select index `[i]` or a range index `[msb:lsb]`
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReferenceIndex {
    BitSelect(u32),
    Range(u32, u32),
}

crate::unit_error_struct!(InvalidReferenceIndex, "invalid reference index");

impl FromStr for ReferenceIndex {
    type Err = InvalidReferenceIndex;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix("[").ok_or(InvalidReferenceIndex)?;
        let s = s.strip_suffix("]").ok_or(InvalidReferenceIndex)?;
        match s.split_once(':') {
            Some((msb_str, lsb_str)) => {
                let msb: u32 = msb_str
                    .trim()
                    .parse()
                    .map_err(|_| InvalidReferenceIndex)?;
                let lsb: u32 = lsb_str
                    .trim_start_matches(':')
                    .trim()
                    .parse()
                    .map_err(|_| InvalidReferenceIndex)?;
                Ok(ReferenceIndex::Range(msb, lsb))
            }
            None => {
                let idx = s
                    .trim()
                    .parse()
                    .map_err(|_| InvalidReferenceIndex{})?;
                Ok(ReferenceIndex::BitSelect(idx))
            }
        }
    }
}

impl Display for ReferenceIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
#[non_exhaustive]
pub struct Var {
    pub var_type: VarType,
    pub size: u32,
    pub code: IdCode,
    pub reference: String,
    pub index: Option<ReferenceIndex>,
}

impl Var {
    pub fn new(
        var_type: VarType,
        size: u32,
        code: IdCode,
        reference: String,
        index: Option<ReferenceIndex>,
    ) -> Self {
        Self { var_type, size, code, reference, index }
    }
}

/// An item in a scope -- either a child scope or a variable.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum ScopeItem {
    Scope(Scope),
    Var(Var),
    Comment(String),
}
