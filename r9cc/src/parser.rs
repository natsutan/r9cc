use crate::tokenizer::*;
use crate::ast::*;

use std::error::Error;
use std::fmt;


#[derive(Debug)]
pub struct ParseError {
    pub err: String,
}

impl Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error :{}", self.err)
    }
}

pub struct Parser {

}

impl Parser {
    pub fn parse (self,  tokenizer : &mut Tokenizer) -> Result<Ast, ParseError> {
        self.primary(tokenizer)
    }

    fn primary(self, tokenizer: &mut Tokenizer) -> Result<Ast, ParseError> {
        let token = tokenizer.get();
        match token.ttype {
            TType::Integer(n) => node_number(n, &token),
            _ => Err(ParseError{err: format!("token {:?} must be number", token)}),
        }
    }

}

fn node_number(n: i64, token :&Token) -> Result<Ast, ParseError> {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let astkind = AstKind::Num(n);
    Ok(Ast{ value: astkind  , loc: loc})
}