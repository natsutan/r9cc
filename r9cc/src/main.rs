extern crate core;

mod tokenizer;
mod parser;
mod generator;
mod ast;


use std::env;
use std::path::Path;
use std::fs::File;
use std::process;
use std::io::prelude::*;
use std::error::Error;


fn main() -> Result<(), Box<dyn Error>> {
    let args :Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage r9cc 'expr'");
        process::exit(1);
    }

    let path = Path::new("tmp.s");
    let input_str = args[1].clone();

    let mut tokenizer = tokenizer::Tokenizer::new("stdin");
    tokenizer.tokenize(&input_str);
    let mut parser = parser::Parser{};
    let ast = parser.parse(&mut tokenizer)?;

    ast::write_dot(&ast, Path::new("tmp.dot"))?;

    let mut file = File::create(path)?;
    writeln!(file, ".intel_syntax noprefix")?;
    writeln!(file, ".globl main")?;
    writeln!(file, "main:")?;


    generator::gen(&ast, &mut file)?;

    writeln!(file, "  pop rax")?;
    writeln!(file, "  ret")?;

    Ok(())
}
