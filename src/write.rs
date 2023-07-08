use std::io;

use crate::{
    Command, Header, IdCode, ReferenceIndex, Scope, ScopeItem, ScopeType, SimulationCommand,
    TimescaleUnit, Value, Var, VarType,
};

/// Struct wrapping an [`std::io::Write`] with methods for writing VCD commands and data.
/// 
/// ## Example
/// ```rust,no_run
/// # use std::error::Error;
/// # fn main() -> Result<(), Box<dyn Error>> {
/// use std::{fs::File, io::BufWriter};
/// use vcd::{Value, TimescaleUnit};
/// 
/// let mut writer = vcd::Writer::new(BufWriter::new(File::create("test.vcd")?));
/// 
/// writer.timescale(1, TimescaleUnit::US)?;
/// writer.add_module("top")?;
/// let clock = writer.add_wire(1, "clock")?;
/// writer.upscope()?;
/// writer.enddefinitions()?;
/// 
/// let mut t = 0;
/// while t < 100 {
///     writer.timestamp(t)?;
///     writer.change_scalar(clock, Value::V0)?;
///     t += 2;
/// 
///     writer.timestamp(t)?;
///     writer.change_scalar(clock, Value::V1)?;
///     t += 2;
/// }
/// # Ok(()) }
/// ```
pub struct Writer<W: io::Write> {
    writer: W,
    next_id_code: IdCode,
    scope_depth: usize,
}

impl<W: io::Write> Writer<W> {
    /// Creates a Writer wrapping an [`io::Write`].
    ///
    /// ```
    /// let mut buf = Vec::new();
    /// let mut vcd = vcd::Writer::new(&mut buf);
    /// ```
    pub fn new(writer: W) -> Writer<W> {
        Writer {
            writer,
            next_id_code: IdCode::FIRST,
            scope_depth: 0,
        }
    }

    /// Get the wrapped [`io::Write`].
    pub fn writer(&mut self) -> &mut W {
        &mut self.writer
    }

    /// Flush the wrapped [`io::Write`].
    pub fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }

    /// Writes a complete header with the fields from a [`Header`] struct from the parser.
    pub fn header(&mut self, h: &Header) -> io::Result<()> {
        if let Some(ref s) = h.date {
            self.date(s)?;
        }
        if let Some(ref s) = h.version {
            self.version(s)?;
        }
        if let Some((v, u)) = h.timescale {
            self.timescale(v, u)?;
        }
        for i in &h.items {
            match *i {
                ScopeItem::Var(ref v) => self.var(v)?,
                ScopeItem::Scope(ref s) => self.scope(s)?,
                ScopeItem::Comment(ref c) => self.comment(c)?,
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
    /// Convenience wrapper around [`Writer::scope_def`].
    pub fn add_module(&mut self, identifier: &str) -> io::Result<()> {
        self.scope_def(ScopeType::Module, identifier)
    }

    /// Writes an `$upscope` command.
    pub fn upscope(&mut self) -> io::Result<()> {
        debug_assert!(
            self.scope_depth > 0,
            "Generating invalid VCD: upscope without a matching scope"
        );
        self.scope_depth -= 1;
        writeln!(self.writer, "$upscope $end")
    }

    /// Writes a `$scope` command, a series of `$var` commands, and an
    /// `$upscope` commands from a [`Scope`] structure from the parser.
    pub fn scope(&mut self, s: &Scope) -> io::Result<()> {
        self.scope_def(s.scope_type, &s.identifier[..])?;
        for i in &s.items {
            match *i {
                ScopeItem::Var(ref v) => self.var(v)?,
                ScopeItem::Scope(ref s) => self.scope(s)?,
                ScopeItem::Comment(ref c) => self.comment(c)?,
            }
        }
        self.upscope()
    }

    /// Writes a `$var` command with a specified id.
    pub fn var_def(
        &mut self,
        var_type: VarType,
        width: u32,
        id: IdCode,
        reference: &str,
        index: Option<ReferenceIndex>,
    ) -> io::Result<()> {
        debug_assert!(
            self.scope_depth > 0,
            "Generating invalid VCD: variable must be in a scope"
        );
        if id >= self.next_id_code {
            self.next_id_code = id.next();
        }
        match index {
            Some(idx) => writeln!(
                self.writer,
                "$var {} {} {} {} {} $end",
                var_type, width, id, reference, idx
            ),
            None => writeln!(
                self.writer,
                "$var {} {} {} {} $end",
                var_type, width, id, reference
            ),
        }
    }

    /// Writes a `$var` command with the next available ID, returning the assigned ID.
    ///
    /// Convenience wrapper around [`Writer::var_def`].
    pub fn add_var(
        &mut self,
        var_type: VarType,
        width: u32,
        reference: &str,
        index: Option<ReferenceIndex>,
    ) -> io::Result<IdCode> {
        let id = self.next_id_code;
        self.var_def(var_type, width, id, reference, index)?;
        Ok(id)
    }

    /// Adds a `$var` for a wire with the next available ID, returning the assigned ID.
    ///
    /// Convenience wrapper around [`Writer::add_var`].
    pub fn add_wire(&mut self, width: u32, reference: &str) -> io::Result<IdCode> {
        self.add_var(VarType::Wire, width, reference, None)
    }

    /// Writes a `$var` command from a [`Var`] structure from the parser.
    pub fn var(&mut self, v: &Var) -> io::Result<()> {
        self.var_def(v.var_type, v.size, v.code, &v.reference[..], v.index)
    }

    /// Writes a `$enddefinitions` command to end the header.
    pub fn enddefinitions(&mut self) -> io::Result<()> {
        debug_assert!(
            self.scope_depth == 0,
            "Generating invalid VCD: {} scopes must be closed with $upscope before $enddefinitions",
            self.scope_depth
        );
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
    pub fn change_vector(&mut self, id: IdCode, v: impl IntoIterator<Item=Value>) -> io::Result<()> {
        write!(self.writer, "b")?;
        for i in v {
            write!(self.writer, "{}", i)?
        }
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

    /// Writes a command from a [`Command`] enum as parsed by the parser.
    pub fn command(&mut self, c: &Command) -> io::Result<()> {
        use Command::*;
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
            ChangeVector(i, ref v) => self.change_vector(i, v),
            ChangeReal(i, v) => self.change_real(i, v),
            ChangeString(i, ref v) => self.change_string(i, v),
            Begin(c) => self.begin(c),
            End(_) => self.end(),
        }
    }
}
