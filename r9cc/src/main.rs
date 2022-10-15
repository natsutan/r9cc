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

    let asm_path = Path::new("tmp.s");
    let input_str = args[1].clone();

    let mut tokenizer = tokenizer::Tokenizer::new("stdin");
    tokenizer.tokenize(&input_str);
    let mut parser = parser::Parser::new();
    parser.parse(&mut tokenizer)?;

    ast::write_dot(&parser.nodes, Path::new("tmp.dot"))?;

    let mut asm_file = File::create(asm_path)?;
    generator::codegen(&parser.nodes, &parser.frame, &mut asm_file)?;


    Ok(())
}
