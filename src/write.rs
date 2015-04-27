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

pub struct Writer<'w> {
	writer: &'w mut io::Write,
}

impl<'s> Writer<'s> {
    pub fn new(writer: &mut io::Write) -> Writer {
        Writer { writer: writer }
    }

    pub fn header(&mut self, h: &Header) -> io::Result<()> {
        if let Some(ref s) = h.date     { try!(self.date(s)); }
        if let Some(ref s) = h.version  { try!(self.version(s)); }
        if let Some(ref s) = h.comment  { try!(self.comment(s)); }
        if let Some((v, u)) = h.timescale { try!(self.timescale(v, u)); }
        try!(self.scope(&h.scope));
        self.enddefinitions()
    }

    pub fn comment(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$comment\n    {}\n$end", v)
    }

    pub fn date(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$date\n    {}\n$end", v)
    }

    pub fn version(&mut self, v: &str) -> io::Result<()> {
        writeln!(self.writer, "$version\n    {}\n$end", v)
    }

    pub fn timescale(&mut self, ts: u32, unit: TimescaleUnit) -> io::Result<()> {
        writeln!(self.writer, "$timescale {} {} $end", ts, unit)
    }

    pub fn scope_def(&mut self, t: ScopeType, i: &str) -> io::Result<()> {
        writeln!(self.writer, "$scope {} {} $end", t, i)
    }

    pub fn upscope(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$upscope $end")
    }

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

    pub fn var_def(&mut self, t: VarType, s: u32, i: IdCode, r: &str) -> io::Result<()> {
        writeln!(self.writer, "$var {} {} {} {} $end", t, s, i, r)
    }

    pub fn var(&mut self, v: &Var) -> io::Result<()> {
        self.var_def(v.var_type, v.size, v.code, &v.reference[..])
    }

    pub fn enddefinitions(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$enddefinitions $end")
    }

    pub fn timestamp(&mut self, ts: u64) -> io::Result<()> {
        writeln!(self.writer, "#{}", ts)
    }

    pub fn change_scalar(&mut self, id: IdCode, v: Value) -> io::Result<()> {
        writeln!(self.writer, "{}{}", v, id)
    }

    pub fn change_vector(&mut self, id: IdCode, v: &[Value]) -> io::Result<()> {
        try!(write!(self.writer, "b"));
        for i in v { try!(write!(self.writer, "{}", i)) }
        writeln!(self.writer, " {}", id)
    }

    pub fn change_real(&mut self, id: IdCode, v: f64) -> io::Result<()> {
        writeln!(self.writer, "r{} {}", v, id)
    }

    pub fn begin(&mut self, c: SimulationCommand) -> io::Result<()> {
        writeln!(self.writer, "${}", c)
    }

    pub fn end(&mut self) -> io::Result<()> {
        writeln!(self.writer, "$end")
    }

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
            Begin(c) => self.begin(c),
            End(_) => self.end(),
        }
    }
}
