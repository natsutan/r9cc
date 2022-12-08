mod tokenizer;
mod parser;
mod generator;
mod ast;
mod typesys;


use std::env;
use std::path::Path;
use std::fs::File;
use std::process;
use std::error::Error;
use std::io::prelude::*;

fn main() -> Result<(), Box<dyn Error>> {
    let args :Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: r9cc source");
        process::exit(1);
    }
    let src = args[1].clone();
    let asm_path = Path::new("tmp.s");

    let mut f = File::open(src.clone()).expect(&format!("file not found {}", src));
    let mut input_str = String::new();
    f.read_to_string(&mut input_str)?;

    let mut tokenizer = tokenizer::Tokenizer::new("stdin");
    tokenizer.tokenize(&input_str);
    let mut parser = parser::Parser::new();
    parser.parse(&mut tokenizer)?;

    ast::write_dot(&parser.nodes, Path::new("tmp.dot"))?;

    let mut asm_file = File::create(asm_path)?;
    generator::codegen(&mut parser.nodes, &parser.globals, &mut asm_file)?;


    Ok(())
}
