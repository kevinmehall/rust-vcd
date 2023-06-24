use std::num::{ParseIntError, ParseFloatError};
use std::{io, fmt};
use std::str::{from_utf8, FromStr};

use crate::{
    Command, Header, InvalidIdCode, InvalidReferenceIndex, InvalidScopeType, InvalidTimescaleUnit,
    InvalidValue, InvalidVarType, ReferenceIndex, Scope, ScopeItem, ScopeType, SimulationCommand,
    Value, Var,
};

fn whitespace_byte(b: u8) -> bool {
    matches!(b, b' ' | b'\n' | b'\r' | b'\t')
}

/// VCD parser. Wraps an `io::Read` and acts as an iterator of `Command`s.
pub struct Parser<R: io::Read> {
    reader: R,
    line: u64,
    end_of_line: bool,
    simulation_command: Option<SimulationCommand>,
}

impl<R: io::Read> Parser<R> {
    /// Creates a parser wrapping an `io::Read`.
    ///
    /// ```
    /// let buf = b"...";
    /// let mut vcd = vcd::Parser::new(&buf[..]);
    /// ```
    pub fn new(reader: R) -> Parser<R> {
        Parser {
            reader,
            line: 1,
            end_of_line: false,
            simulation_command: None,
        }
    }

    /// get the wrapped [`io::Read`]
    pub fn reader(&mut self) -> &mut R {
        &mut self.reader
    }

    /// Get the current line number.
    pub fn line(&self) -> u64 {
        self.line
    }

    fn error(&self, k: impl Into<ParseErrorKind>) -> ParseError {
        ParseError { line: self.line, kind: k.into() }
    }

    fn read_byte_or_eof(&mut self) -> Result<Option<u8>, io::Error> {
        let b = io::Read::bytes(&mut self.reader).next().transpose();

        // delay incrementing the line number until the first character of the next line
        // so that errors at the end of the line refer to the correct line number
        if self.end_of_line {
            self.line += 1;
        }
        self.end_of_line = matches!(b, Ok(Some(b'\n')));

        b
    }

    fn read_byte(&mut self) -> Result<u8, io::Error> {
        match self.read_byte_or_eof()? {
            None => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end of VCD file",
            )),
            Some(v) => Ok(v),
        }
    }

    fn read_token<'a>(&mut self, buf: &'a mut [u8]) -> Result<&'a [u8], io::Error> {
        let mut len = 0;
        loop {
            let b = self.read_byte()?;
            if whitespace_byte(b) {
                if len > 0 {
                    break;
                } else {
                    continue;
                }
            }

            if let Some(p) = buf.get_mut(len) {
                *p = b;
            } else {
                return Err(self.error(ParseErrorKind::TokenTooLong).into());
            }

            len += 1;
        }
        Ok(&buf[..len])
    }

    fn read_token_str<'a>(&mut self, buf: &'a mut [u8]) -> Result<&'a str, io::Error> {
        from_utf8(self.read_token(buf)?).map_err(|_| self.error(ParseErrorKind::InvalidUtf8).into())
    }

    fn read_token_string(&mut self) -> Result<String, io::Error> {
        let mut r = Vec::new();
        loop {
            let b = self.read_byte()?;
            if whitespace_byte(b) {
                if r.len() > 0 {
                    break;
                } else {
                    continue;
                }
            }
            r.push(b);
        }
        String::from_utf8(r).map_err(|_| self.error(ParseErrorKind::InvalidUtf8).into())
    }

    fn read_token_parse<T>(&mut self) -> Result<T, io::Error>
    where
        T: FromStr,
        <T as FromStr>::Err: Into<ParseErrorKind>,
    {
        let mut buf = [0; 32];
        let tok = self.read_token_str(&mut buf)?;
        tok.parse()
            .map_err(|e| self.error(e).into())
    }

    fn read_command_end(&mut self) -> Result<(), io::Error> {
        let mut buf = [0; 8];
        let tok = self.read_token(&mut buf)?;
        if tok == b"$end" {
            Ok(())
        } else {
            Err(self.error(ParseErrorKind::ExpectedEndCommand).into())
        }
    }

    fn read_string_command(&mut self) -> Result<String, io::Error> {
        let mut r = Vec::new();
        loop {
            r.push(self.read_byte()?);
            if r.ends_with(b"$end") {
                break;
            }
        }
        let len = r.len() - 4;
        r.truncate(len);

        let s = from_utf8(&r).map_err(|_| io::Error::from(self.error(ParseErrorKind::InvalidUtf8)))?;
        Ok(s.trim().to_string()) // TODO: don't reallocate
    }

    fn parse_command(&mut self) -> Result<Command, io::Error> {
        use Command::*;
        use SimulationCommand::*;

        let mut cmdbuf = [0; 16];
        let cmd = self.read_token(&mut cmdbuf)?;

        match cmd {
            b"comment" => Ok(Comment(self.read_string_command()?)),
            b"date" => Ok(Date(self.read_string_command()?)),
            b"version" => Ok(Version(self.read_string_command()?)),
            b"timescale" => {
                let (mut buf, mut buf2) = ([0; 8], [0; 8]);
                let tok = self.read_token_str(&mut buf)?;
                // Support both "1ps" and "1 ps"
                let (num_str, unit_str) = match tok.find(|c: char| !c.is_numeric()) {
                    Some(idx) => (&tok[0..idx], &tok[idx..]),
                    None => (tok, self.read_token_str(&mut buf2)?),
                };
                let quantity = num_str
                    .parse()
                    .map_err(|e| self.error(e))?;
                let unit = unit_str
                    .parse()
                    .map_err(|e| self.error(e))?;
                self.read_command_end()?;
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

                let mut buf = [0; 32];
                let mut tok = self.read_token_str(&mut buf)?;
                let index = {
                    if tok.starts_with("[") {
                        let r = Some(tok.parse::<ReferenceIndex>().map_err(|e| self.error(e))?);
                        tok = self.read_token_str(&mut buf)?;
                        r
                    } else {
                        None
                    }
                };

                if tok.as_bytes() != b"$end" {
                    return Err(self.error(ParseErrorKind::ExpectedEndCommand).into());
                }
                Ok(VarDef(var_type, size, code, reference, index))
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
                    Err(self.error(ParseErrorKind::UnmatchedEnd).into())
                }
            }

            _ => Err(self.error(ParseErrorKind::UnknownCommand).into()),
        }
    }

    fn begin_simulation_command(&mut self, c: SimulationCommand) -> Result<Command, io::Error> {
        self.simulation_command = Some(c);
        Ok(Command::Begin(c))
    }

    fn parse_timestamp(&mut self) -> Result<Command, io::Error> {
        Ok(Command::Timestamp(self.read_token_parse()?))
    }

    fn parse_scalar(&mut self, initial: u8) -> Result<Command, io::Error> {
        let id = self.read_token_parse()?;
        let val = Value::parse(initial).map_err(|e| self.error(e))?;
        Ok(Command::ChangeScalar(id, val))
    }

    fn parse_vector(&mut self) -> Result<Command, io::Error> {
        let mut val = Vec::new();
        loop {
            let b = self.read_byte()?;
            if whitespace_byte(b) {
                if val.len() > 0 {
                    break;
                } else {
                    continue;
                }
            }
            val.push(Value::parse(b).map_err(|e| self.error(e))?);
        }
        let id = self.read_token_parse()?;
        Ok(Command::ChangeVector(id, val.into()))
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

    fn parse_scope(
        &mut self,
        scope_type: ScopeType,
        reference: String,
    ) -> Result<Scope, io::Error> {
        use Command::*;
        let mut children = Vec::new();

        loop {
            match self.next() {
                Some(Ok(Upscope)) => break,
                Some(Ok(ScopeDef(tp, id))) => {
                    children.push(ScopeItem::Scope(self.parse_scope(tp, id)?));
                }
                Some(Ok(VarDef(tp, size, id, r, idx))) => {
                    children.push(ScopeItem::Var(Var {
                        var_type: tp,
                        size,
                        code: id,
                        reference: r,
                        index: idx,
                    }));
                }
                Some(Ok(Comment(comment))) => {
                    children.push(ScopeItem::Comment(comment));
                }
                Some(Ok(_)) => return Err(self.error(ParseErrorKind::UnexpectedHeaderCommand).into()),
                Some(Err(e)) => return Err(e),
                None => {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "unexpected EOF with open $scope",
                    ))
                }
            }
        }

        Ok(Scope {
            scope_type,
            identifier: reference,
            children,
        })
    }

    /// Parses the header of a VCD file into a `Header` struct.
    ///
    /// After returning, the stream has been read just past the `$enddefinitions` command and can
    /// be iterated to obtain the data.
    pub fn parse_header(&mut self) -> Result<Header, io::Error> {
        use Command::*;
        let mut header: Header = Default::default();
        loop {
            match self.next() {
                Some(Ok(Enddefinitions)) => break,
                Some(Ok(Comment(s))) => {
                    header.comment = Some(s);
                }
                Some(Ok(Date(s))) => {
                    header.date = Some(s);
                }
                Some(Ok(Version(s))) => {
                    header.version = Some(s);
                }
                Some(Ok(Timescale(val, unit))) => {
                    header.timescale = Some((val, unit));
                }
                Some(Ok(VarDef(var_type, size, code, reference, index))) => {
                    header.items.push(ScopeItem::Var(Var {
                        var_type,
                        size,
                        code,
                        reference,
                        index,
                    }));
                }
                Some(Ok(ScopeDef(tp, id))) => {
                    header
                        .items
                        .push(ScopeItem::Scope(self.parse_scope(tp, id)?));
                }
                Some(Ok(_)) => return Err(self.error(ParseErrorKind::UnexpectedHeaderCommand).into()),
                Some(Err(e)) => return Err(e),
                None => {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "unexpected end of VCD file before $enddefinitions",
                    ))
                }
            }
        }
        Ok(header)
    }
}

impl<P: io::Read> Iterator for Parser<P> {
    type Item = Result<Command, io::Error>;
    fn next(&mut self) -> Option<Result<Command, io::Error>> {
        while let Some(b) = self.read_byte_or_eof().transpose() {
            let b = match b {
                Ok(b) => b,
                Err(e) => return Some(Err(e)),
            };
            match b {
                b' ' | b'\n' | b'\r' | b'\t' => (),
                b'$' => return Some(self.parse_command()),
                b'#' => return Some(self.parse_timestamp()),
                b'0' | b'1' | b'z' | b'Z' | b'x' | b'X' => return Some(self.parse_scalar(b)),
                b'b' | b'B' => return Some(self.parse_vector()),
                b'r' | b'R' => return Some(self.parse_real()),
                b's' | b'S' => return Some(self.parse_string()),
                _ => {
                    return Some(Err(
                        self.error(ParseErrorKind::UnexpectedCharacter).into()
                    ))
                }
            }
        }
        None
    }
}

/// Error from parsing VCD
#[derive(Debug, Clone)]
pub struct ParseError {
    line: u64,
    kind: ParseErrorKind,
}

impl ParseError {
    /// Returns the 1-indexed line number in the VCD file where the error occurred.
    pub fn line(&self) -> u64 { self.line }

    /// Returns an enum value identifying the kind of error.
    pub fn kind(&self) -> &ParseErrorKind { &self.kind }
}

/// Errors that can be encountered while parsing.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ParseErrorKind {
    InvalidUtf8,
    UnexpectedCharacter,
    TokenTooLong,
    ExpectedEndCommand,
    UnmatchedEnd,
    UnknownCommand,
    UnexpectedHeaderCommand,
    ParseIntError(ParseIntError),
    ParseFloatError(ParseFloatError),
    InvalidTimescaleUnit(InvalidTimescaleUnit),
    InvalidScopeType(InvalidScopeType),
    InvalidVarType(InvalidVarType),
    InvalidReferenceIndex(InvalidReferenceIndex),
    InvalidValueChar(InvalidValue),
    InvalidIdCode(InvalidIdCode),
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at line {}", self.kind, self.line)
    }
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::InvalidUtf8 => write!(f, "invalid UTF-8"),
            ParseErrorKind::UnexpectedCharacter => write!(f, "unexpected character at start of command"),
            ParseErrorKind::TokenTooLong => write!(f, "token too long"),
            ParseErrorKind::ExpectedEndCommand => write!(f, "expected `$end`"),
            ParseErrorKind::UnmatchedEnd => write!(f, "unmatched `$end`"),
            ParseErrorKind::UnknownCommand => write!(f, "unknown command"),
            ParseErrorKind::UnexpectedHeaderCommand => write!(f, "unexpected command in header"),
            ParseErrorKind::ParseIntError(e) => write!(f, "{}", e),
            ParseErrorKind::ParseFloatError(e) => write!(f, "{}", e),
            ParseErrorKind::InvalidTimescaleUnit(e) => write!(f, "{}", e),
            ParseErrorKind::InvalidScopeType(e) => write!(f, "{}", e),
            ParseErrorKind::InvalidVarType(e) => write!(f, "{}", e),
            ParseErrorKind::InvalidReferenceIndex(e) => write!(f, "{}", e),
            ParseErrorKind::InvalidValueChar(e) => write!(f, "{}", e),
            ParseErrorKind::InvalidIdCode(e) => write!(f, "{}", e),
        }
    }
}

impl From<ParseIntError> for ParseErrorKind {
    fn from(e: ParseIntError) -> Self { ParseErrorKind::ParseIntError(e) }
}

impl From<ParseFloatError> for ParseErrorKind {
    fn from(e: ParseFloatError) -> Self { ParseErrorKind::ParseFloatError(e) }
}

impl From<InvalidScopeType> for ParseErrorKind {
    fn from(e: InvalidScopeType) -> Self { ParseErrorKind::InvalidScopeType(e) }
}

impl From<InvalidVarType> for ParseErrorKind {
    fn from(e: InvalidVarType) -> Self { ParseErrorKind::InvalidVarType(e) }
}

impl From<InvalidTimescaleUnit> for ParseErrorKind {
    fn from(e: InvalidTimescaleUnit) -> Self { ParseErrorKind::InvalidTimescaleUnit(e) }
}

impl From<InvalidReferenceIndex> for ParseErrorKind {
    fn from(e: InvalidReferenceIndex) -> Self { ParseErrorKind::InvalidReferenceIndex(e) }
}

impl From<InvalidIdCode> for ParseErrorKind {
    fn from(e: InvalidIdCode) -> Self { ParseErrorKind::InvalidIdCode(e) }
}

impl From<InvalidValue> for ParseErrorKind {
    fn from(e: InvalidValue) -> Self { ParseErrorKind::InvalidValueChar(e) }
}

impl From<ParseError> for io::Error {
    fn from(e: ParseError) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidData, e)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        Command::*, Parser, ReferenceIndex, ScopeItem, ScopeType, SimulationCommand::*,
        TimescaleUnit, Value::*, Var, VarType, Vector, ParseError, ParseErrorKind,
    };

    #[test]
    fn wikipedia_sample() {
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

        let scope = match &header.items[0] {
            ScopeItem::Scope(sc) => sc,
            x => panic!("Expected Scope, found {:?}", x),
        };

        assert_eq!(&scope.identifier[..], "logic");
        assert_eq!(scope.scope_type, ScopeType::Module);

        if let ScopeItem::Var(ref v) = scope.children[0] {
            assert_eq!(v.var_type, VarType::Wire);
            assert_eq!(&v.reference[..], "data");
            assert_eq!(v.size, 8);
        } else {
            panic!("Expected Var, found {:?}", scope.children[0]);
        }

        let expected = &[
            Begin(Dumpvars),
            ChangeVector(2u32.into(), [X, X, X, X, X, X, X, X].into()),
            ChangeScalar(3u32.into(), X),
            ChangeScalar(4u32.into(), V0),
            ChangeScalar(5u32.into(), X),
            ChangeScalar(6u32.into(), X),
            ChangeScalar(7u32.into(), V1),
            ChangeScalar(8u32.into(), V0),
            End(Dumpvars),
            Timestamp(0),
            ChangeVector(2u32.into(), [V1, V0, V0, V0, V0, V0, V0, V1].into()),
            ChangeScalar(3u32.into(), V0),
            ChangeScalar(4u32.into(), V1),
            Timestamp(2211),
            ChangeScalar(6u32.into(), V0),
            Timestamp(2296),
            ChangeVector(2u32.into(), [V0].into()),
            ChangeScalar(3u32.into(), V1),
            Timestamp(2302),
            ChangeScalar(3u32.into(), V0),
            Timestamp(2303),
        ];

        for (i, e) in b.zip(expected.iter()) {
            assert_eq!(&i.unwrap(), e);
        }
    }

    #[test]
    fn name_with_spaces() {
        let sample = b"
$scope module top $end
$var wire 1 ! i_vld [0] $end
$var wire 10 ~ i_data [9:0] $end
$upscope $end
$enddefinitions $end
#0
";
        let mut parser = Parser::new(&sample[..]);
        let header = parser.parse_header().unwrap();

        let scope = match &header.items[0] {
            ScopeItem::Scope(sc) => sc,
            x => panic!("Expected Scope, found {:?}", x),
        };

        assert_eq!(&scope.identifier[..], "top");
        assert_eq!(scope.scope_type, ScopeType::Module);

        if let ScopeItem::Var(ref v) = scope.children[0] {
            assert_eq!(v.var_type, VarType::Wire);
            assert_eq!(&v.reference[..], "i_vld");
            assert_eq!(v.index, Some(ReferenceIndex::BitSelect(0)));
            assert_eq!(v.size, 1);
        } else {
            panic!("Expected Var, found {:?}", scope.children[0]);
        }

        if let ScopeItem::Var(ref v) = scope.children[1] {
            assert_eq!(v.var_type, VarType::Wire);
            assert_eq!(&v.reference[..], "i_data");
            assert_eq!(v.index, Some(ReferenceIndex::Range(9, 0)));
            assert_eq!(v.size, 10);
        } else {
            panic!("Expected Var, found {:?}", scope.children[0]);
        }
    }

    #[test]
    fn more_type_examples() {
        let sample = b"
$scope module logic $end
$var integer 32 t smt_step $end
$var event 1 ! smt_clock $end
$upscope $end
$enddefinitions $end
#0
1!
b00000000000000000000000000000000 t
";
        let mut b = Parser::new(&sample[..]);

        let header = b.parse_header().unwrap();
        assert_eq!(header.comment, None);
        assert_eq!(header.date, None);
        assert_eq!(header.version, None);
        assert_eq!(header.timescale, None);

        let scope = match &header.items[0] {
            ScopeItem::Scope(sc) => sc,
            x => panic!("Expected Scope, found {:?}", x),
        };

        assert_eq!(&scope.identifier[..], "logic");
        assert_eq!(scope.scope_type, ScopeType::Module);

        if let ScopeItem::Var(ref v) = scope.children[0] {
            assert_eq!(v.var_type, VarType::Integer);
            assert_eq!(&v.reference[..], "smt_step");
            assert_eq!(v.size, 32);
        } else {
            panic!("Expected Var, found {:?}", scope.children[0]);
        }

        let expected = &[
            Timestamp(0),
            ChangeScalar(0u32.into(), V1),
            ChangeVector(83u32.into(), [V0; 32].into()),
        ];

        for (i, e) in b.zip(expected.iter()) {
            assert_eq!(&i.unwrap(), e);
        }
    }

    #[test]
    fn symbiyosys_example() {
        let sample = b"
$var integer 32 t smt_step $end
$var event 1 ! smt_clock $end
$scope module queue $end
$var wire 8 n11 buffer<0> $end
$var wire 1 n0 i_clk $end
$var wire 8 n1 i_data $end
$var wire 1 n2 i_read_enable $end
$var wire 1 n3 i_reset_n $end
$var wire 1 n4 i_write_enable $end
$var wire 1 n5 matches $end
$var wire 8 n6 o_data $end
$var wire 1 n7 o_empty $end
$var wire 1 n8 o_full $end
$var wire 5 n9 r_ptr $end
$var wire 5 n10 w_ptr $end
$upscope $end
$enddefinitions $end
#0
1!
b00000000000000000000000000000000 t
b1 n0
b00000000 n1
b0 n2
b0 n3
b0 n4
b1 n5
b00000000 n6
b1 n7
b0 n8
b00000 n9
b00000 n10
b00000000 n11
#5
b0 n0
#10
1!
b00000000000000000000000000000001 t
b1 n0
b00000000 n1
b0 n2
b0 n3
b0 n4
b1 n5
b00000000 n6
b1 n7
b0 n8
b00000 n9
b00000 n10
b00000000 n11
#15
b0 n0
#20
1!
b00000000000000000000000000000010 t
b1 n0
";

        let mut b = Parser::new(&sample[..]);

        let header = b.parse_header().unwrap();
        assert_eq!(header.comment, None);
        assert_eq!(header.date, None);
        assert_eq!(header.version, None);
        assert_eq!(header.timescale, None);

        assert_eq!(
            header.items[0],
            ScopeItem::Var(Var {
                var_type: VarType::Integer,
                size: 32,
                code: 83u32.into(),
                reference: "smt_step".into(),
                index: None,
            })
        );
        assert_eq!(
            header.items[1],
            ScopeItem::Var(Var {
                var_type: VarType::Event,
                size: 1,
                code: 0u32.into(),
                reference: "smt_clock".into(),
                index: None,
            })
        );

        let scope = match &header.items[2] {
            ScopeItem::Scope(sc) => sc,
            x => panic!("Expected Scope, found {:?}", x),
        };

        assert_eq!(&scope.identifier[..], "queue");
        assert_eq!(scope.scope_type, ScopeType::Module);

        if let ScopeItem::Var(ref v) = scope.children[0] {
            assert_eq!(v.var_type, VarType::Wire);
            assert_eq!(&v.reference[..], "buffer<0>");
            assert_eq!(v.size, 8);
        } else {
            panic!("Expected Var, found {:?}", scope.children[0]);
        }

        let expected = &[
            Timestamp(0),
            ChangeScalar(0u32.into(), V1),
            ChangeVector(83u32.into(), Vector::zeros(32)),
            ChangeVector(1581u32.into(), [V1].into()),
            ChangeVector(1675u32.into(), [V0, V0, V0, V0, V0, V0, V0, V0].into()),
            ChangeVector(1769u32.into(), [V0].into()),
            ChangeVector(1863u32.into(), [V0].into()),
            ChangeVector(1957u32.into(), [V0].into()),
            ChangeVector(2051u32.into(), [V1].into()),
            ChangeVector(2145u32.into(), [V0, V0, V0, V0, V0, V0, V0, V0].into()),
            ChangeVector(2239u32.into(), [V1].into()),
            ChangeVector(2333u32.into(), [V0].into()),
            ChangeVector(2427u32.into(), [V0, V0, V0, V0, V0].into()),
            ChangeVector(143051u32.into(), [V0, V0, V0, V0, V0].into()),
            ChangeVector(151887u32.into(), [V0, V0, V0, V0, V0, V0, V0, V0].into()),
            Timestamp(5),
            ChangeVector(1581u32.into(), [V0].into()),
            Timestamp(10),
            ChangeScalar(0u32.into(), V1),
            ChangeVector(
                83u32.into(),
                [
                    V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0,
                    V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V1,
                ].into(),
            ),
            ChangeVector(1581u32.into(), [V1].into()),
            ChangeVector(1675u32.into(), [V0, V0, V0, V0, V0, V0, V0, V0].into()),
            ChangeVector(1769u32.into(), [V0].into()),
            ChangeVector(1863u32.into(), [V0].into()),
            ChangeVector(1957u32.into(), [V0].into()),
            ChangeVector(2051u32.into(), [V1].into()),
            ChangeVector(2145u32.into(), [V0, V0, V0, V0, V0, V0, V0, V0].into()),
            ChangeVector(2239u32.into(), [V1].into()),
            ChangeVector(2333u32.into(), [V0].into()),
            ChangeVector(2427u32.into(), [V0, V0, V0, V0, V0].into()),
            ChangeVector(143051u32.into(), [V0, V0, V0, V0, V0].into()),
            ChangeVector(151887u32.into(), [V0, V0, V0, V0, V0, V0, V0, V0].into()),
            Timestamp(15),
            ChangeVector(1581u32.into(), [V0].into()),
            Timestamp(20),
            ChangeScalar(0u32.into(), V1),
            ChangeVector(
                83u32.into(),
                [
                    V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V0,
                    V0, V0, V0, V0, V0, V0, V0, V0, V0, V0, V1, V0,
                ].into(),
            ),
            ChangeVector(1581u32.into(), [V1].into()),
        ];

        for (i, e) in b.zip(expected.iter()) {
            assert_eq!(&i.unwrap(), e);
        }
    }


    #[test]
    fn comment_in_scope() {
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
        $comment test comment please ignore! $end
        $var wire 1 & rx_en $end
        $var wire 1 ' tx_en $end
        $var wire 1 ( empty $end
        $var wire 1 ) underrun $end
        $comment foo is not handled $end
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

        let scope = match &header.items[0] {
            ScopeItem::Scope(sc) => sc,
            x => panic!("Expected Scope, found {:?}", x),
        };

        assert_eq!(&scope.identifier[..], "logic");
        assert_eq!(scope.scope_type, ScopeType::Module);

        if let ScopeItem::Var(ref v) = scope.children[0] {
            assert_eq!(v.var_type, VarType::Wire);
            assert_eq!(&v.reference[..], "data");
            assert_eq!(v.size, 8);
        } else {
            panic!("Expected Var, found {:?}", scope.children[0]);
        }

        if let ScopeItem::Comment(_) = scope.children[3] {
        } else {
            panic!("Expected Comment, found {:?}", scope.children[3]);
        }

        if let ScopeItem::Comment(_) = scope.children[8] {
        } else {
            panic!("Expected Comment, found {:?}", scope.children[8]);
        }


        let expected = &[
            Begin(Dumpvars),
            ChangeVector(2u32.into(), [X, X, X, X, X, X, X, X].into()),
            ChangeScalar(3u32.into(), X),
            ChangeScalar(4u32.into(), V0),
            ChangeScalar(5u32.into(), X),
            ChangeScalar(6u32.into(), X),
            ChangeScalar(7u32.into(), V1),
            ChangeScalar(8u32.into(), V0),
            End(Dumpvars),
            Timestamp(0),
            ChangeVector(2u32.into(), [V1, V0, V0, V0, V0, V0, V0, V1].into()),
            ChangeScalar(3u32.into(), V0),
            ChangeScalar(4u32.into(), V1),
            Timestamp(2211),
            ChangeScalar(6u32.into(), V0),
            Timestamp(2296),
            ChangeVector(2u32.into(), [V0].into()),
            ChangeScalar(3u32.into(), V1),
            Timestamp(2302),
            ChangeScalar(3u32.into(), V0),
            Timestamp(2303),
        ];

        for (i, e) in b.zip(expected.iter()) {
            assert_eq!(&i.unwrap(), e);
        }
    }

    #[test]
    fn test_err_in_header() {
        let text = b"
        $timescale 100 ns $end
        $scope module logic $end
        $var invalid 1 $ x $end
        ";

        let err = Parser::new(&text[..]).parse_header().unwrap_err();
        let err: Box<ParseError> = err.into_inner().unwrap().downcast().unwrap();
        assert!(matches!(err.kind, ParseErrorKind::InvalidVarType(..)));
        assert_eq!(err.line(), 4);
    }

    #[test]
    fn test_err_in_data() {
        let text = b"
        $timescale 100 ns $end
        $scope module logic $end
        $var wire 1 $ x $end
        $upscope $end
        $enddefinitions $end
        n$
        x$
        ";

        let err = Parser::new(&text[..]).find_map(|f| f.err()).unwrap();
        let err: Box<ParseError> = err.into_inner().unwrap().downcast().unwrap();
        assert!(matches!(err.kind, ParseErrorKind::UnexpectedCharacter), "{:?}", err);
        assert_eq!(err.line(), 7);
    }
}
