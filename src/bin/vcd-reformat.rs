extern crate vcd;
use std::io;

/// A simple demo that uses the reader and writer to round-trip a VCD file from stdin to stdout
pub fn main() {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut reader = vcd::read::Parser::new(&mut stdin);
    let mut writer = vcd::write::Writer::new(&mut stdout);

    let header = reader.parse_header().unwrap();
    writer.header(&header).unwrap();

    for cmd in reader {
        writer.command(&cmd.unwrap()).unwrap();
    }
}
