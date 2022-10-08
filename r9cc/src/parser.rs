use crate::tokenizer::*;
use crate::ast::*;
use std::mem;
use std::error::Error;
use std::fmt;
use crate::ast;
use crate::ast::AstKind::BinOp;


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
    pub fn parse(self, tokenizer: &mut Tokenizer) -> Result<Ast, ParseError> {
        expr(tokenizer)
    }
}

fn primary(tokenizer: &mut Tokenizer) -> Result<Ast, ParseError> {
    let token = tokenizer.get();
    tokenizer.consume();
    match token.ttype {
        TType::Integer(n) => node_number(n, &token),
        TType::LParen => {
            let node = expr(tokenizer)?;
            let next_token = tokenizer.get();
            if next_token.ttype == TType::RParen {
                tokenizer.consume();
                return Ok(node);
            }
            Err(ParseError{err: format!("token {:?} must be )", token)})
        }
        _ => Err(ParseError{err: format!("token {:?} must be number", token)}),
    }
}

fn mul(tokenizer : &mut Tokenizer) -> Result<Ast, ParseError> {
    let mut node = unarry(tokenizer)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "*" => {
                        tokenizer.consume();
                        let node_r = unarry(tokenizer)?;
                        node = new_node(BinOpKind::Mult, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                    }
                    "/" => {
                        tokenizer.consume();
                        let node_r = unarry(tokenizer)?;
                        node = new_node(BinOpKind::Div, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                    }
                    _ => return Ok(node),
                }
            }
            _ => {
                return Ok(node);
            }
        }
    }
}

fn expr(tokenizer : &mut Tokenizer) -> Result<Ast, ParseError> {
    let mut node = mul(tokenizer)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "+" => {
                        tokenizer.consume();
                        let node_r = mul(tokenizer)?;
                        node = new_node(BinOpKind::Add, node, node_r, Loc{0:next_token.line_num, 1:next_token.pos});
                    }
                    "-" => {
                        tokenizer.consume();
                        let node_r = mul(tokenizer)?;
                        node = new_node(BinOpKind::Sub, node, node_r, Loc{0:next_token.line_num, 1:next_token.pos});
                    }
                    _ => return Ok(node),
                }
            }
            _ => {
                return Ok(node);
            }
        }
    }

}

fn unarry(tokenizer : &mut Tokenizer) -> Result<Ast, ParseError> {
    let next_token = tokenizer.get();
    match next_token.ttype {
        TType::Operator(s) => {
            match &*s {
                "+" => {
                    tokenizer.consume();
                    return primary(tokenizer);
                },
                "-" => {
                    let dummy_token = tokenizer.get();
                    tokenizer.consume();
                    let node_0 = node_number(0, &dummy_token)?;
                    let node_r = primary(tokenizer)?;
                    let node = new_node(BinOpKind::Sub, node_0, node_r, Loc{0:next_token.line_num, 1:next_token.pos});
                    return Ok(node);
                },
                _ => return primary(tokenizer),
            }
        }
        _ => {
            return primary(tokenizer);
        }
    }
}

fn new_node(op :ast::BinOpKind, l: Ast, r: Ast, loc:Loc) -> Ast {

    let binop  = ast::BinOp{ value:op, loc: loc.clone()};
    let astkind = ast::AstKind::BinOp{op: binop, l: Box::new(l), r: Box::new(r)};
    let ast: Ast = Ast{value: astkind, loc};
    ast
}

fn node_number(n: i64, token :&Token) -> Result<Ast, ParseError> {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let astkind = AstKind::Num(n);
    Ok(Ast{ value: astkind  , loc: loc})
}

