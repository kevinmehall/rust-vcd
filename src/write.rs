use std::io;

use {
    TimescaleUnit,
    Value,
    IdCode,
    Scope,
    Var,
    ReferenceIndex,
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
    next_id_code: IdCode,
    scope_depth: usize,
}

impl<'s> Writer<'s> {
    /// Creates a Writer, wrapping an io::Write.
    ///
    /// ```
    /// let mut buf = Vec::new();
    /// let mut vcd = vcd::Writer::new(&mut buf);
    /// ```
    pub fn new(writer: &mut io::Write) -> Writer {
        Writer { writer: writer, next_id_code: IdCode::FIRST, scope_depth: 0 }
    }

    /// Writes a complete header with the fields from a `Header` struct from the parser.
    pub fn header(&mut self, h: &Header) -> io::Result<()> {
        if let Some(ref s) = h.date     { try!(self.date(s)); }
        if let Some(ref s) = h.version  { try!(self.version(s)); }
        if let Some(ref s) = h.comment  { try!(self.comment(s)); }
        if let Some((v, u)) = h.timescale { try!(self.timescale(v, u)); }
        for i in &h.items {
            match *i {
                ScopeItem::Var(ref v) => try!(self.var(v)),
                ScopeItem::Scope(ref s) => try!(self.scope(s)),
            }
        }
        self.enddefinitions()
    }

    /// Writes a `$comment` command.
    pub fn comment(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$comment\n    {}\n$end", v)
    }

    /// Writes a `$date` command.
    pub fn date(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$date\n    {}\n$end", v)
    }

    /// Writes a `$version` command.
    pub fn version(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$version\n    {}\n$end", v)
    }

    /// Writes a `$timescale` command.
    pub fn timescale(&mut self, ts: u32, unit: TimescaleUnit) -> io::Result<()> {
        writeln!(self.writer, "$timescale {} {} $end", ts, unit)
    }

    /// Writes a `$scope` command.
    pub fn scope_def(&mut self, t: ScopeType, i: &str) -> io::Result<()> {
        self.scope_depth += 1;
        writeln!(self.writer, "$scope {} {} $end", t, i)
    }

    /// Writes a `$scope` command for a module.
    ///
    /// Convenience wrapper around `scope_def`.
    pub fn add_module(&mut self, identifier: &str) -> io::Result<()> {
        self.scope_def(ScopeType::Module, identifier)
    }

    /// Writes an `$upscope` command.
    pub fn upscope(&mut self) -> io::Result<()> {
        debug_assert!(self.scope_depth > 0, "Generating invalid VCD: upscope without a matching scope");
        self.scope_depth -= 1;
        writeln!(self.writer, "$upscope $end")
    }

    /// Writes a `$scope` command, a series of `$var` commands, and an `$upscope` commands from
    /// a `Scope` structure from the parser.
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

    /// Writes a `$var` command with a specified id.
    pub fn var_def(&mut self, var_type: VarType, width: u32, id: IdCode, reference: &str, index: Option<ReferenceIndex>) -> io::Result<()> {
        debug_assert!(self.scope_depth > 0, "Generating invalid VCD: variable must be in a scope");
        if id >= self.next_id_code {
            self.next_id_code = id.next();
        }
        match index {
            Some(idx) => writeln!(self.writer, "$var {} {} {} {} {} $end", var_type, width, id, reference, idx),
            None => writeln!(self.writer, "$var {} {} {} {} $end", var_type, width, id, reference),
        }
    }

    /// Writes a `$var` command with the next available ID, returning the assigned ID.
    ///
    /// Convenience wrapper around `var_def`.
    pub fn add_var(&mut self, var_type: VarType, width: u32, reference: &str, index: Option<ReferenceIndex>) -> io::Result<IdCode> {
        let id = self.next_id_code;
        self.var_def(var_type, width, id, reference, index)?;
        Ok(id)
    }

    /// Adds a `$var` for a wire with the next available ID, returning the assigned ID.
    ///
    /// Convenience wrapper around `add_var`.
    pub fn add_wire(&mut self, width: u32, reference: &str) -> io::Result<IdCode> {
        self.add_var(VarType::Wire, width, reference, None)
    }

    /// Writes a `$var` command from a `Var` structure from the parser.
    pub fn var(&mut self, v: &Var) -> io::Result<()> {
        self.var_def(v.var_type, v.size, v.code, &v.reference[..], v.index)
    }

    /// Writes a `$enddefinitions` command to end the header.
    pub fn enddefinitions(&mut self) -> io::Result<()> {
        debug_assert!(self.scope_depth == 0, "Generating invalid VCD: {} scopes must be closed with $upscope before $enddefinitions");
        writeln!(self.writer, "$enddefinitions $end")
    }

    /// Writes a `#xxx` timestamp.
    pub fn timestamp(&mut self, ts: u64) -> io::Result<()> {
        writeln!(self.writer, "#{}", ts)
    }

    /// Writes a change to a scalar variable.
    pub fn change_scalar<V: Into<Value>>(&mut self, id: IdCode, v: V) -> io::Result<()> {
        writeln!(self.writer, "{}{}", v.into(), id)
    }

    /// Writes a change to a vector variable.
    pub fn change_vector(&mut self, id: IdCode, v: &[Value]) -> io::Result<()> {
        try!(write!(self.writer, "b"));
        for i in v { try!(write!(self.writer, "{}", i)) }
        writeln!(self.writer, " {}", id)
    }

    /// Writes a change to a real variable.
    pub fn change_real(&mut self, id: IdCode, v: f64) -> io::Result<()> {
        writeln!(self.writer, "r{} {}", v, id)
    }

    /// Writes a change to a string variable.
    pub fn change_string(&mut self, id: IdCode, v: &str) -> io::Result<()> {
        writeln!(self.writer, "s{} {}", v, id)
    }

    /// Writes the beginning of a simulation command.
    pub fn begin(&mut self, c: SimulationCommand) -> io::Result<()> {
        writeln!(self.writer, "${}", c)
    }

    /// Writes an `$end` to end a simulation command.
    pub fn end(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$end")
    }

    /// Writes a command from a `Command` enum as parsed by the parser.
    pub fn command(&mut self, c: &Command) -> io::Result<()> {
        use super::Command::*;
        match *c {
            Comment(ref c) => self.comment(&c[..]),
            Date(ref c) => self.date(&c[..]),
            Version(ref c) => self.version(&c[..]),
            Timescale(v, u) => self.timescale(v, u),
            ScopeDef(t, ref i) => self.scope_def(t, &i[..]),
            Upscope => self.upscope(),
            VarDef(t, s, i, ref r, idx) => self.var_def(t, s, i, &r[..], idx),
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
