use std::{io, error::Error, process::exit};

/// A simple demo that uses the reader and writer to round-trip a VCD file from stdin to stdout
fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        exit(1);
    }
}


pub fn run() -> Result<(), Box<dyn Error>> {
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout();

    let mut reader = vcd::Parser::new(&mut stdin);
    let mut writer = vcd::Writer::new(&mut stdout);

    let header = reader.parse_header()?;
    writer.header(&header)?;

    for cmd in reader {
        writer.command(&cmd?)?;
    }

    Ok(())
}
