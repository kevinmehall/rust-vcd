# VCD

**[Documentation](https://docs.rs/vcd)**

[![Build Status](https://travis-ci.org/kevinmehall/rust-vcd.svg?branch=master)](https://travis-ci.org/kevinmehall/rust-vcd)

This crate reads and writes [VCD (Value Change Dump)][wp] files, a common format used with logic analyzers, HDL simulators, and other EDA tools. It provides streaming wrappers around the `io::Read` and `io::Write` traits to read and write VCD commands and data.

[wp]: https://en.wikipedia.org/wiki/Value_change_dump
