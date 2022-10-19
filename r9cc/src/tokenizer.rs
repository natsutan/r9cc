
use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TType {
    Integer(i64),
    Operator(String),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Identifier(String),
    If,
    Else,
    For,
    While,
    Comma,
    Return,
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token  {
    pub ttype: TType,
    pub line_num: usize,
    pub pos: usize,
}

impl Token {
    pub fn new(tt:TType, lnum :usize, p :usize) -> Token {
        Token{ttype: tt, line_num: lnum, pos :p }
    }
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
    token_pos: usize,
    src_line_num: usize,
    src_file: String,
    src_code: Vec<String>
}

impl Tokenizer {
    pub fn new(src_file: &str) -> Tokenizer {
        Tokenizer {
            tokens: vec![],
            token_pos: 0,
            src_line_num: 0,
            src_file: src_file.to_string(),
            src_code: vec![]
        }
    }

    pub fn tokenize(&mut self, input_str :&String) {
        let input_vec:Vec<char> = input_str.chars().collect();

        let mut s = String::new();
        let last_index = input_str.len();

        self.src_code.push(input_str.to_string());
        let mut skip = false;

        for idx in 0..input_str.len() {
            let c = input_vec[idx];
            let next_c =  if idx == last_index - 1 {
                '\n'
            } else {
                input_vec[idx+1]
            };

            if skip {
                skip = false;
                continue;
            }

            match c {
                ' ' => (),
                '+' => self.tokens.push(Token::new(TType::Operator("+".to_string()), self.src_line_num, idx)),
                '-' => self.tokens.push(Token::new(TType::Operator("-".to_string()), self.src_line_num, idx)),
                '*' => self.tokens.push(Token::new(TType::Operator("*".to_string()), self.src_line_num, idx)),
                '/' => self.tokens.push(Token::new(TType::Operator("/".to_string()), self.src_line_num, idx)),
                '(' => self.tokens.push(Token::new(TType::LParen, self.src_line_num, idx)),
                ')' => self.tokens.push(Token::new(TType::RParen, self.src_line_num, idx)),
                '{' => self.tokens.push(Token::new(TType::LBrace, self.src_line_num, idx)),
                '}' => self.tokens.push(Token::new(TType::RBrace, self.src_line_num, idx)),
                ';' => self.tokens.push(Token::new(TType::Comma, self.src_line_num, idx)),
                '=' => {
                    if next_c == '=' {
                        self.tokens.push(Token::new(TType::Operator("==".to_string()), self.src_line_num, idx));
                        skip = true;
                    } else {
                        self.tokens.push(Token::new(TType::Operator("=".to_string()), self.src_line_num, idx));
                    }
                }
                '!' => {
                    if next_c == '=' {
                        self.tokens.push(Token::new(TType::Operator("!=".to_string()), self.src_line_num, idx));
                        skip = true;
                    } else {
                        println!("Tokenize error next to = {}", next_c);
                    }
                }
                '>' => {
                    if next_c == '=' {
                        self.tokens.push(Token::new(TType::Operator(">=".to_string()), self.src_line_num, idx));
                        skip = true;
                    } else {
                        self.tokens.push(Token::new(TType::Operator(">".to_string()), self.src_line_num, idx));
                    }
                }
                '<' => {
                    if next_c == '=' {
                        self.tokens.push(Token::new(TType::Operator("<=".to_string()), self.src_line_num, idx));
                        skip = true;
                    } else {
                        self.tokens.push(Token::new(TType::Operator("<".to_string()), self.src_line_num, idx));
                    }
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'a'..='z' => {
                    if is_digit(&next_c) || is_alpha(&next_c) {
                        s.push(c);
                    } else {
                        s.push(c);
                        if let Ok(val) = s.parse::<i64> () {
                            self.tokens.push(Token::new(TType::Integer(val), self.src_line_num, idx));
                        } else {
                            match &*s {
                                "return" => self.tokens.push(Token::new(TType::Return, self.src_line_num, idx)),
                                "if" => self.tokens.push(Token::new(TType::If, self.src_line_num, idx)),
                                "else" => self.tokens.push(Token::new(TType::Else, self.src_line_num, idx)),
                                "for" => self.tokens.push(Token::new(TType::For, self.src_line_num, idx)),
                                "while" => self.tokens.push(Token::new(TType::While, self.src_line_num, idx)),
                                _ =>  self.tokens.push(Token::new(TType::Identifier(s.to_string()), self.src_line_num, idx)),
                            }
                        }
                        s = "".to_string();
                    }
                },
                _ => println!("Tokenize error {}", c),
            }
        }
        self.tokens.push(Token::new(TType::EOF, self.src_line_num, last_index));

    }

    pub fn get(&mut self) -> Token {
        if self.token_pos == self.tokens.len() {
            Token::new(TType::EOF, self.src_line_num, 0)
        } else {
            let t = self.tokens[self.token_pos].clone();
            //println!("[TOKEN get]: {:?}", t);
            t
        }
    }

    pub fn consume(&mut self) -> () {
        if self.token_pos != self.tokens.len() {
            self.token_pos += 1;
        }
    }

    pub fn at_eof(&self) -> bool {
        self.tokens[self.token_pos].ttype == TType::EOF
    }
}



fn is_digit(c :&char) -> bool {
    match c {
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
        _ => false,
    }
}

fn is_alpha(c :&char) -> bool {
    match c {
        'a'..='z' => true,
        _ => false,
    }
}




//test
//5 + 20-4
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f0() {
        let mut tokenizer : Tokenizer = Tokenizer::new("stdin");
        tokenizer.tokenize(&"5+20".to_string());

        let mut tokens :Vec<TType> = Vec::new();
        for t in tokenizer.tokens {
            tokens.push(t.ttype);
        }


        assert_eq!(
            tokens,
            vec![
                TType::Integer(5),
                TType::Operator("+".to_string()),
                TType::Integer(20),
                TType::EOF
            ]
        );
    }

    #[test]
    fn test_f1() {
        let mut tokenizer : Tokenizer = Tokenizer::new("stdin");
        tokenizer.tokenize(&"5 + 20-4".to_string());

        let mut tokens :Vec<TType> = Vec::new();
        for t in tokenizer.tokens {
            tokens.push(t.ttype);
        }

        assert_eq!(
            tokens,
            vec![
                TType::Integer(5),
                TType::Operator("+".to_string()),
                TType::Integer(20),
                TType::Operator("-".to_string()),
                TType::Integer(4),
                TType::EOF
            ]
        );
    }
    #[test]
    fn test_get() {
        let mut tokenizer : Tokenizer = Tokenizer::new("stdin");
        tokenizer.tokenize(&"5+20".to_string());
        let t0 = tokenizer.get();
        assert_eq!(t0.ttype, TType::Integer(5));
        let t1 = tokenizer.get();
        assert_eq!(t1.ttype, TType::Operator("+".to_string()));
        let t2 = tokenizer.get();
        assert_eq!(t2.ttype, TType::Integer(20));
        let t3 = tokenizer.get();
        assert_eq!(t3.ttype, TType::EOF);
        let t4 = tokenizer.get();
        assert_eq!(t4.ttype, TType::EOF);
    }
}
