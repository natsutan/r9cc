
use std::env;
use std::path::Path;
use std::fs::File;
use std::process;
use std::error::Error;
use std::io::prelude::*;

fn main() -> Result<(), Box<dyn Error>> {
    let args :Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage r9cc n");
        process::exit(1);
    }

    let path = Path::new("tmp.s");

    let mut file = File::create(path)?;
    writeln!(file, ".intel_syntax noprefix")?;
    writeln!(file, ".globl main")?;
    writeln!(file, "main:")?;
    writeln!(file, "  mov rax, {}", args[1])?;
    writeln!(file, "  ret")?;

    Ok(())
}
