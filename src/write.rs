use std::io;

use {
    TimescaleUnit,
    Value,
    IdCode,
    Scope,
    Var,
    ScopeItem,
    SimulationCommand,
    Header,
    ScopeType,
    VarType,
    Command
};

/// Struct wrapping an `io::Write` with methods for writing VCD commands and data.
pub struct Writer<'w> {
	writer: &'w mut io::Write,
}

impl<'s> Writer<'s> {
    /// Create a Writer, wrapping an io::Write
    ///
    /// ```
    /// let mut buf = Vec::new();
    /// let mut vcd = vcd::Writer::new(&mut buf);
    /// ```
    pub fn new(writer: &mut io::Write) -> Writer {
        Writer { writer: writer }
    }

    /// Write a header with the data from a `Header` struct
    pub fn header(&mut self, h: &Header) -> io::Result<()> {
        if let Some(ref s) = h.date     { try!(self.date(s)); }
        if let Some(ref s) = h.version  { try!(self.version(s)); }
        if let Some(ref s) = h.comment  { try!(self.comment(s)); }
        if let Some((v, u)) = h.timescale { try!(self.timescale(v, u)); }
        try!(self.scope(&h.scope));
        self.enddefinitions()
    }

    /// Write a `$comment` command
    pub fn comment(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$comment\n    {}\n$end", v)
    }

    /// Write a `$date` command
    pub fn date(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$date\n    {}\n$end", v)
    }

    /// Write a `$version` command
    pub fn version(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$version\n    {}\n$end", v)
    }

    /// Write a `$timescale` command
    pub fn timescale(&mut self, ts: u32, unit: TimescaleUnit) -> io::Result<()> {
        writeln!(self.writer, "$timescale {} {} $end", ts, unit)
    }

    /// Write a `$scope` command
    pub fn scope_def(&mut self, t: ScopeType, i: &str) -> io::Result<()> {
        writeln!(self.writer, "$scope {} {} $end", t, i)
    }

    /// Write an `$upscope` command
    pub fn upscope(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$upscope $end")
    }

    /// Write a `$scope` command, a series of `$var` commands, and an `$upscope` commands from
    /// a `Scope` structure
    pub fn scope(&mut self, s: &Scope) -> io::Result<()> {
        try!(self.scope_def(s.scope_type, &s.identifier[..]));
        for i in &s.children {
            match *i {
                ScopeItem::Var(ref v) => try!(self.var(v)),
                ScopeItem::Scope(ref s) => try!(self.scope(s)),
            }
        }
        self.upscope()
    }

    /// Write a `$var` command
    pub fn var_def(&mut self, t: VarType, s: u32, i: IdCode, r: &str) -> io::Result<()> {
        writeln!(self.writer, "$var {} {} {} {} $end", t, s, i, r)
    }

    /// Write a `$var` command from a `Var` structure
    pub fn var(&mut self, v: &Var) -> io::Result<()> {
        self.var_def(v.var_type, v.size, v.code, &v.reference[..])
    }

    /// Write a `$enddefinitions` command
    pub fn enddefinitions(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$enddefinitions $end")
    }

    /// Write a `#xxx` timestamp
    pub fn timestamp(&mut self, ts: u64) -> io::Result<()> {
        writeln!(self.writer, "#{}", ts)
    }

    /// Write a change to a scalar variable
    pub fn change_scalar(&mut self, id: IdCode, v: Value) -> io::Result<()> {
        writeln!(self.writer, "{}{}", v, id)
    }

    /// Write a change to a vector variable
    pub fn change_vector(&mut self, id: IdCode, v: &[Value]) -> io::Result<()> {
        try!(write!(self.writer, "b"));
        for i in v { try!(write!(self.writer, "{}", i)) }
        writeln!(self.writer, " {}", id)
    }

    /// Write a change to a real variable
    pub fn change_real(&mut self, id: IdCode, v: f64) -> io::Result<()> {
        writeln!(self.writer, "r{} {}", v, id)
    }

    /// Write a change to a string variable
    pub fn change_string(&mut self, id: IdCode, v: &str) -> io::Result<()> {
        writeln!(self.writer, "s{} {}", v, id)
    }

    /// Write the beginning of a simulation command
    pub fn begin(&mut self, c: SimulationCommand) -> io::Result<()> {
        writeln!(self.writer, "${}", c)
    }

    /// Write an `$end` to end a simulation command
    pub fn end(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$end")
    }

    /// Write a command from a `Command` enum as parsed by the parser.
    pub fn command(&mut self, c: &Command) -> io::Result<()> {
        use super::Command::*;
        match *c {
            Comment(ref c) => self.comment(&c[..]),
            Date(ref c) => self.date(&c[..]),
            Version(ref c) => self.version(&c[..]),
            Timescale(v, u) => self.timescale(v, u),
            ScopeDef(t, ref i) => self.scope_def(t, &i[..]),
            Upscope => self.upscope(),
            VarDef(t, s, i, ref r) => self.var_def(t, s, i, &r[..]),
            Enddefinitions => self.enddefinitions(),
            Timestamp(t) => self.timestamp(t),
            ChangeScalar(i, v) => self.change_scalar(i, v),
            ChangeVector(i, ref v) => self.change_vector(i, &v[..]),
            ChangeReal(i, v) => self.change_real(i, v),
            ChangeString(i, ref v) => self.change_string(i, v),
            Begin(c) => self.begin(c),
            End(_) => self.end(),
        }
    }
}
