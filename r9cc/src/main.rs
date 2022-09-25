
mod tokenizer;

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

    let tokens = tokenizer::tokenize( &args[1]);
    println!("{:?}", tokens);

    let mut file = File::create(path)?;
    writeln!(file, ".intel_syntax noprefix")?;
    writeln!(file, ".globl main")?;
    writeln!(file, "main:")?;

    let first_val = match tokens[0] {
        tokenizer::Token::Integer(n) => n,
        _ => return Err(tokenizer::TokenError{ err: "first token must be integer.".to_string() })?,
    };
    writeln!(file, "  mov rax, {}", first_val)?;
    let range = 1..tokens.len();

    for ( token, i) in tokens[1..].iter().zip(range.into_iter()) {
        match (token, i) {
            (tokenizer::Token::Operator(s), i) => match s.as_str() {
                "+" => {
                    let val = match tokens[i+1] {
                        tokenizer::Token::Integer(n) => n,
                        _ => return Err(tokenizer::TokenError{ err: format!("unexpected token {:?}", tokens[i+1])})?
                    };
                    writeln!(file, "  add rax, {}", val)?
                } ,
                "-" => {
                    let val = match tokens[i+1] {
                        tokenizer::Token::Integer(n) => n,
                        _ => return Err(tokenizer::TokenError{ err: format!("unexpected token {:?}", tokens[i+1])})?
                    };
                    writeln!(file, "  sub rax, {}", val)?
                } ,
                _ => return Err(tokenizer::TokenError{ err: format!("Unknown Operator {}", s) })?
            }
            (_, _) => return Err(tokenizer::TokenError{ err: format!("Unexpected token {:?}", token)})?
        }
    }
    writeln!(file, "  ret")?;

    Ok(())
}
