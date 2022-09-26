
use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(i64),
    Operator(String),
    EOF(),
}

#[derive(Debug)]
pub struct TokenError {
    pub err: String,
}

impl Error for TokenError {}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tokenization error :{}", self.err)
    }
}

pub struct Tokenizer {
    tokens: Vec<Token>,
    pos :usize
}

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            tokens: vec![],
            pos: 0
        }
    }

    pub fn tokenize(&mut self, input_str :&String) {
        self.tokens = tokenize(input_str);
    }

    pub fn get(&mut self) -> Token {
        if self.pos == self.tokens.len() {
            Token::EOF()
        } else {
            let t = self.tokens[self.pos].clone();
            self.pos += 1;
            t
        }
    }

    pub fn expected_number(&mut self) -> Result<i64, TokenError> {
        let token = self.get();
        match token {
            Token::Integer(n) => Ok(n),
            _ => Err(TokenError{ err: format!("{:?} is not number.", token)}),
        }
    }

    pub fn at_eof(&self) -> bool {
        self.tokens[self.pos] == Token::EOF()
    }
}



fn is_digit(c :&char) -> bool {
    match c {
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
        _ => false,
    }
}


pub fn tokenize(input_str :&String) -> Vec<Token> {
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
            ' ' => (),
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
    result.push(Token::EOF());
    result
}


//test
//5 + 20-4
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f0() {
        let mut tokenizer : Tokenizer = Tokenizer::new();
        tokenizer.tokenize(&"5+20".to_string());
        assert_eq!(
            tokenizer.tokens,
            vec![
                Token::Integer(5),
                Token::Operator("+".to_string()),
                Token::Integer(20),
                Token::EOF()
            ]
        );
    }
    #[test]
    fn test_f1() {
        let mut tokenizer : Tokenizer = Tokenizer::new();
        tokenizer.tokenize(&"5 + 20-4".to_string());
        assert_eq!(
            tokenizer.tokens,
            vec![
                Token::Integer(5),
                Token::Operator("+".to_string()),
                Token::Integer(20),
                Token::Operator("-".to_string()),
                Token::Integer(4),
                Token::EOF()
            ]
        );
    }
    #[test]
    fn test_get() {
        let mut tokenizer : Tokenizer = Tokenizer::new();
        tokenizer.tokenize(&"5+20".to_string());
        let t0 = tokenizer.get();
        assert_eq!(t0, Token::Integer(5));
        let t1 = tokenizer.get();
        assert_eq!(t1, Token::Operator("+".to_string()));
        let t2 = tokenizer.get();
        assert_eq!(t2, Token::Integer(20));
        let t3 = tokenizer.get();
        assert_eq!(t3, Token::EOF());
        let t4 = tokenizer.get();
        assert_eq!(t4, Token::EOF());
    }
}
