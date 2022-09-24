
use std::env;
use std::path::Path;
use std::fs::File;
use std::process;
use std::error::Error;
use std::io::prelude::*;
use std::fmt;


#[derive(Debug)]
enum Token {
    Integer(i64),
    Operator(String),
}

#[derive(Debug)]
pub struct TokenError {
    err: String,
}

impl Error for TokenError {}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tokenization error :{}", self.err)
    }
}

fn is_digit(c :&char) -> bool {
    match c {
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
        _ => false,
    }
}


fn tokenize(input_str :&String) -> Vec<Token> {
    let mut result: Vec<Token> = Vec::new();
    let input_vec:Vec<char> = input_str.chars().collect();

    let mut s = String::new();
    let last_index = input_str.len();

    for idx in 0..input_str.len() {
        let c = input_vec[idx];
        let next_c =  if idx == last_index - 1 {
            '\n'
        } else {
            input_vec[idx+1]
        };

        match c {
            '+' => result.push(Token::Operator("+".to_string())),
            '-' => result.push(Token::Operator("-".to_string())),
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                if is_digit(&next_c) {
                    s.push(c);
                } else {
                    s.push(c);
                    if let Ok(val) = s.parse::<i64> () {
                        result.push(Token::Integer(val));
                        s = "".to_string();
                    } else {
                        println!("Tokenize error parse::<i64> {}", s);
                    }
                }
            },
            _ => println!("Tokenize error {}", c),
        }
    }
    result
}

fn main() -> Result<(), Box<dyn Error>> {
    let args :Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage r9cc 'expr'");
        process::exit(1);
    }

    let path = Path::new("tmp.s");
    let input_str = args[1].clone();

    let tokens = tokenize( &args[1]);
    println!("{:?}", tokens);

    let mut file = File::create(path)?;
    writeln!(file, ".intel_syntax noprefix")?;
    writeln!(file, ".globl main")?;
    writeln!(file, "main:")?;

    let first_val = match tokens[0] {
        Token::Integer(n) => n,
        _ => return Err(TokenError{ err: "first token must be integer.".to_string() })?,
    };
    writeln!(file, "  mov rax, {}", first_val)?;

    for token in tokens[1..].iter() {
        match token {
            Token::Integer(n) => writeln!(file, "{}", n)?,
            Token::Operator(s) => match s.as_str() {
                "+" =>  write!(file, "  add rax, ")?,
                "-" =>  write!(file, "  sub rax, ")?,
                _ => return Err(TokenError{ err: format!("Unknown Operator {}", s) })?
            }
            _ => return Err(TokenError{ err: "Unknown token ".to_string() })?
        }
    }
    writeln!(file, "  ret")?;

    Ok(())
}
