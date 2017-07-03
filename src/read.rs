use std::io;
use std::str::{ FromStr, from_utf8 };
use super::InvalidData;

use {
    Value,
    ScopeType,
    Scope,
    Var,
    ScopeItem,
    SimulationCommand,
    Header,
    Command
};

fn whitespace_byte(b: u8) -> bool {
    match b {
        b' ' | b'\n' | b'\r' | b'\t' => true,
        _ => false,
    }
}

/// VCD parser. Wraps an `io::Read` and acts as an iterator of `Command`s.
pub struct Parser<R: io::Read> {
    bytes_iter: io::Bytes<R>,
    simulation_command: Option<SimulationCommand>,
}

impl<R: io::Read> Parser<R> {
    /// Create a parser wrapping an `io::Read`
    ///
    /// ```
    /// let buf = b"...";
    /// let mut vcd = vcd::Parser::new(&buf[..]);
    /// ```
    pub fn new(r: R) -> Parser<R> {
        Parser {
            bytes_iter: r.bytes(),
            simulation_command: None,
        }
    }

    fn read_byte(&mut self) -> Result<u8, io::Error> {
        match self.bytes_iter.next() {
            Some(b) => b,
            None => return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected end of VCD file")),
        }
    }

    fn read_token<'a>(&mut self, buf: &'a mut [u8]) -> Result<&'a [u8], io::Error> {
        let mut len = 0;
        loop {
            let b = self.read_byte()?;
            if whitespace_byte(b) {
                if len > 0 { break; } else { continue; }
            }

            if let Some(p) = buf.get_mut(len) {
                *p = b;
            } else {
                return Err(InvalidData("token too long").into());
            }

            len += 1;
        }
        Ok(&buf[..len])
    }

    fn read_token_str<'a>(&mut self, buf: &'a mut [u8]) -> Result<&'a str, io::Error> {
        from_utf8(self.read_token(buf)?).map_err(|_| InvalidData("string is not UTF-8").into())
    }

    fn read_token_string(&mut self) -> Result<String, io::Error> {
        let mut r = Vec::new();
        loop {
            let b = self.read_byte()?;
            if whitespace_byte(b) {
                if r.len() > 0 { break; } else { continue; }
            }
            r.push(b);
        }
        String::from_utf8(r).map_err(|_| InvalidData("string is not UTF-8").into())
    }

    fn read_token_parse<T>(&mut self) -> Result<T, io::Error> where T: FromStr, <T as FromStr>::Err: 'static + ::std::error::Error + Send + Sync {
        let mut buf = [0; 32];
        let tok = self.read_token_str(&mut buf)?;

        if tok == "$end" {
            return Err(InvalidData("unexpected $end").into());
        }

        tok.parse().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    fn read_command_end(&mut self) -> Result<(), io::Error> {
        let mut buf = [0; 8];
        let tok = self.read_token(&mut buf)?;
        if tok == b"$end" { Ok(()) } else { Err(InvalidData("expected $end").into()) }
    }

    fn read_string_command(&mut self) -> Result<String, io::Error> {
        let mut r = Vec::new();
        loop {
            r.push(self.read_byte()?);
            if r.ends_with(b"$end") { break; }
        }
        let len = r.len() - 4;
        r.truncate(len);

        let s = from_utf8(&r).map_err(|_| io::Error::from(InvalidData("string is not UTF-8")))?;
        Ok(s.trim().to_string()) // TODO: don't reallocate
    }

    fn parse_command(&mut self) -> Result<Command, io::Error> {
        use super::Command::*;
        use super::SimulationCommand::*;

        let mut cmdbuf = [0; 16];
        let cmd = self.read_token(&mut cmdbuf)?;

        match cmd {
            b"comment" => Ok(Comment(self.read_string_command()?)),
            b"date"    => Ok(Date(self.read_string_command()?)),
            b"version" => Ok(Version(self.read_string_command()?)),
            b"timescale" => {
                let (mut buf, mut buf2) = ([0; 8], [0; 8]);
                let tok = self.read_token_str(&mut buf)?;
                // Support both "1ps" and "1 ps"
                let (num_str, unit_str) = match tok.find(|c: char| !c.is_numeric()) {
                    Some(idx) => (&tok[0..idx], &tok[idx..]),
                    None => (tok, self.read_token_str(&mut buf2)?)
                };
                self.read_command_end()?;
                let quantity = num_str.parse().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
                let unit = unit_str.parse()?;
                Ok(Timescale(quantity, unit))
            }
            b"scope" => {
                let scope_type = self.read_token_parse()?;
                let identifier = self.read_token_string()?;
                self.read_command_end()?;
                Ok(ScopeDef(scope_type, identifier))
            }
            b"upscope" => {
                self.read_command_end()?;
                Ok(Upscope)
            }
            b"var" => {
                let var_type = self.read_token_parse()?;
                let size = self.read_token_parse()?;
                let code = self.read_token_parse()?;
                let reference = self.read_token_string()?;
                self.read_command_end()?;
                Ok(VarDef(var_type, size, code, reference))
            }
            b"enddefinitions" => {
                self.read_command_end()?;
                Ok(Enddefinitions)
            }

            // Simulation commands
            b"dumpall" => self.begin_simulation_command(Dumpall),
            b"dumpoff" => self.begin_simulation_command(Dumpoff),
            b"dumpon" => self.begin_simulation_command(Dumpon),
            b"dumpvars" => self.begin_simulation_command(Dumpvars),

            b"end" => {
                if let Some(c) = self.simulation_command.take() {
                    Ok(End(c))
                } else {
                    Err(InvalidData("unmatched $end").into())
                }
            }

            _ => Err(InvalidData("invalid keyword").into())
        }
    }

    fn begin_simulation_command(&mut self, c: SimulationCommand) -> Result<Command, io::Error> {
        self.simulation_command = Some(c);
        Ok(Command::Begin(c))
    }

    fn parse_timestamp(&mut self) -> Result<Command, io::Error> {
        Ok(Command::Timestamp(self.read_token_parse()?))
    }

    fn parse_scalar(&mut self, initial: u8) ->Result<Command, io::Error> {
        let id = self.read_token_parse()?;
        let val = Value::parse(initial)?;
        Ok(Command::ChangeScalar(id, val))
    }

    fn parse_vector(&mut self) -> Result<Command, io::Error> {
        let mut buf = [0; 32];
        let val = self.read_token(&mut buf)?.iter().map(|&v| { Value::parse(v) })
            .collect::<Result<Vec<Value>, InvalidData>>()?;
        let id = self.read_token_parse()?;
        Ok(Command::ChangeVector(id, val))
    }

    fn parse_real(&mut self) -> Result<Command, io::Error> {
        let val = self.read_token_parse()?;
        let id = self.read_token_parse()?;
        Ok(Command::ChangeReal(id, val))
    }

    fn parse_string(&mut self) -> Result<Command, io::Error> {
        let val = self.read_token_string()?;
        let id = self.read_token_parse()?;
        Ok(Command::ChangeString(id, val))
    }

    fn parse_scope(&mut self, scope_type: ScopeType, reference: String) -> Result<Scope, io::Error> {
        use super::Command::*;
        let mut children = Vec::new();

        loop {
            match self.next() {
                Some(Ok(Upscope)) => break,
                Some(Ok(ScopeDef(tp, id))) => {
                    children.push(ScopeItem::Scope(self.parse_scope(tp, id)?));
                }
                Some(Ok(VarDef(tp, size, id, r))) => {
                    children.push(ScopeItem::Var(
                        Var { var_type: tp, size: size, code: id, reference: r }
                    ));
                }
                Some(Ok(_)) => return Err(InvalidData("unexpected command in $scope").into()),
                Some(Err(e)) => return Err(e),
                None => return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected EOF with open $scope"))
            }
        }

        Ok(Scope { scope_type: scope_type, identifier: reference, children: children })
    }

    /// Parse the header of a VCD file into a `Header` struct. After returning, the stream has been
    /// read just past the `$enddefinitions` command and can be iterated to obtain the data.
    pub fn parse_header(&mut self) -> Result<Header, io::Error> {
        use super::Command::*;
        let mut header: Header = Default::default();
        loop {
            match self.next() {
                Some(Ok(Enddefinitions)) => break,
                Some(Ok(Comment(s))) => { header.comment = Some(s); }
                Some(Ok(Date(s)))    => { header.date    = Some(s); }
                Some(Ok(Version(s))) => { header.version = Some(s); }
                Some(Ok(Timescale(val, unit))) => { header.timescale = Some((val, unit)); }
                Some(Ok(ScopeDef(tp, id))) => {
                    header.scope = self.parse_scope(tp, id)?;
                }
                Some(Ok(_)) => {
                    return Err(InvalidData("unexpected command in header").into())
                }
                Some(Err(e)) => return Err(e),
                None => return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected end of VCD file before $enddefinitions"))
            }
        }
        Ok(header)
    }
}

impl<P: io::Read> Iterator for Parser<P> {
    type Item = Result<Command, io::Error>;
    fn next(&mut self) -> Option<Result<Command, io::Error>> {
        while let Some(b) = self.bytes_iter.next() {
            let b = match b {
                Ok(b) => b,
                Err(e) => return Some(Err(e))
            };
            match b {
                b' ' | b'\n' | b'\r' | b'\t' => (),
                b'$' => return Some(self.parse_command()),
                b'#' => return Some(self.parse_timestamp()),
                b'0' | b'1' | b'z' | b'Z' | b'x' | b'X' => return Some(self.parse_scalar(b)),
                b'b' | b'B' => return Some(self.parse_vector()),
                b'r' | b'R' => return Some(self.parse_real()),
                b's' | b'S' => return Some(self.parse_string()),
                _ => return Some(Err(InvalidData("unexpected character at start of command").into()))
            }
        }
        None
    }
}

#[test]
fn wikipedia_sample() {
    use super::Command::*;
    use super::SimulationCommand::*;
    use super::Value::*;
    use super::{ TimescaleUnit, IdCode, VarType, ScopeType };

    let sample = b"
    $date
       Date text.
    $end
    $version
       VCD generator text.
    $end
    $comment
       Any comment text.
    $end
    $timescale 100 ns $end
    $scope module logic $end
    $var wire 8 # data $end
    $var wire 1 $ data_valid $end
    $var wire 1 % en $end
    $var wire 1 & rx_en $end
    $var wire 1 ' tx_en $end
    $var wire 1 ( empty $end
    $var wire 1 ) underrun $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    bxxxxxxxx #
    x$
    0%
    x&
    x'
    1(
    0)
    $end
    #0
    b10000001 #
    0$
    1%
    #2211
    0'
    #2296
    b0 #
    1$
    #2302
    0$
    #2303
        ";

    let mut b = Parser::new(&sample[..]);

    let header = b.parse_header().unwrap();
    assert_eq!(header.comment, Some("Any comment text.".to_string()));
    assert_eq!(header.date, Some("Date text.".to_string()));
    assert_eq!(header.version, Some("VCD generator text.".to_string()));
    assert_eq!(header.timescale, Some((100, TimescaleUnit::NS)));

    assert_eq!(&header.scope.identifier[..], "logic");
    assert_eq!(header.scope.scope_type, ScopeType::Module);

    if let ScopeItem::Var(ref v) = header.scope.children[0] {
        assert_eq!(v.var_type, VarType::Wire);
        assert_eq!(&v.reference[..], "data");
        assert_eq!(v.size, 8);
    } else {
        panic!("Expected Var, found {:?}", header.scope.children[0]);
    }

    let expected = &[
        Begin(Dumpvars),
        ChangeVector(IdCode(2), vec![X, X, X, X, X, X, X, X]),
        ChangeScalar(IdCode(3), X),
        ChangeScalar(IdCode(4), V0),
        ChangeScalar(IdCode(5), X),
        ChangeScalar(IdCode(6), X),
        ChangeScalar(IdCode(7), V1),
        ChangeScalar(IdCode(8), V0),
        End(Dumpvars),
        Timestamp(0),
        ChangeVector(IdCode(2), vec![V1, V0, V0, V0, V0, V0, V0, V1]),
        ChangeScalar(IdCode(3), V0),
        ChangeScalar(IdCode(4), V1),
        Timestamp(2211),
        ChangeScalar(IdCode(6), V0),
        Timestamp(2296),
        ChangeVector(IdCode(2), vec![V0]),
        ChangeScalar(IdCode(3), V1),
        Timestamp(2302),
        ChangeScalar(IdCode(3), V0),
        Timestamp(2303),
    ];

    for (i, e) in b.zip(expected.iter()) {
        assert_eq!(&i.unwrap(), e);
    }
}
