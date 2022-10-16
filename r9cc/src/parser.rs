use crate::tokenizer::*;
use crate::ast::*;
use std::error::Error;
use std::fmt;
use crate::ast;


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
    pub nodes: Program,
    pub frame: Frame,
}

static mut lvar: Vec<LocalVariable> = vec![];

impl Parser {
    pub fn new() -> Parser {
        Parser {
            nodes: vec![],
            frame: vec![],
        }
    }

    pub fn parse(&mut self, tokenizer: &mut Tokenizer) -> Result<(), ParseError> {
        let token = tokenizer.get();
        if token.ttype != TType::LBrace {
            return Err(ParseError{err: format!("the first {{ is not found  {:?}", token)})
        }
        tokenizer.consume();
        let node = compound_stmt(tokenizer, &mut self.frame)?;
        self.nodes.push(node);
        Ok(())
    }

}

fn find_lvar(name: &String, frame: &Frame) -> Result<LocalVariable, ()> {
    for lv in frame.iter() {
        if *name == lv.name {
            return Ok(lv.clone())
        }
    }
    Err(())
}


// stmt = expr-stmt
fn stmt(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let token = tokenizer.get();
    match token.ttype {
        TType::Return => {
            tokenizer.consume();
            let node_l = expr(tokenizer, frame)?;
            let token_comma = tokenizer.get();
            if token_comma.ttype == TType::Comma {
                tokenizer.consume();
                let node = new_unary(UniOpKind::ND_RETURN, node_l, Loc { 0: token.line_num, 1: token.pos });
                Ok(node)
            } else {
                return Err(ParseError{err: format!("token {:?} must be ;)", token_comma)})
            }
        }
        TType::LBrace => {
            tokenizer.consume();
            return compound_stmt(tokenizer, frame)
        },

        _ =>  expr_stmt(tokenizer, frame)
    }
}


// stmt = expr-stmt
fn compound_stmt(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let token_head = tokenizer.get();
    let mut body :Vec<Box<Ast>> =  vec![];

    let mut token = tokenizer.get();
    while token.ttype != TType::RBrace {
        let st = stmt(tokenizer, frame)?;
        body.push(Box::from(st));
        token = tokenizer.get();
    }

    let mut node_block = new_block(body, Loc { 0: token_head.line_num, 1: token_head.pos });

    Ok(node_block)
}

// expr-stmt = expr ";"
fn expr_stmt(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let node = expr(tokenizer, frame)?;
    let token = tokenizer.get();
    match token.ttype {
        TType::Comma => {
            tokenizer.consume();
            let node = new_unary(UniOpKind::ND_EXPR_STMT, node, Loc { 0: token.line_num, 1: token.pos });
            return Ok(node);
       }
        _ => {
            return Err(ParseError{err: format!("token {:?} must be ;)", token)})
        }
    }
}

fn primary(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let token = tokenizer.get();
    tokenizer.consume();
    match token.ttype {
        TType::Integer(n) => node_number(n, &token),
        TType::Identifier(s) => {
            let tk = tokenizer.get();
            let name = s.clone();

            let search_result = find_lvar(&name, frame);

            match search_result {
                Ok(lv) => {
                    node_variable(name, lv.offset, &tk)
                }
                Err(()) => {
                    // new node
                    let offset = (frame.len() as i64 +  1) * 8;
                    let new_lv = LocalVariable{name: name.clone(), offset};
                    frame.push(new_lv);
                    node_variable(name, offset, &tk)
                }
            }
        },
        TType::LParen => {
            let node = expr(tokenizer, frame)?;
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

fn mul(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let mut node = unary(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "*" => {
                        tokenizer.consume();
                        let node_r = unary(tokenizer, frame)?;
                        node = new_node(BinOpKind::Mult, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                    }
                    "/" => {
                        tokenizer.consume();
                        let node_r = unary(tokenizer, frame)?;
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

fn expr(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    return assign(tokenizer, frame);
}

fn assign (tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let mut node = equality(tokenizer, frame)?;
    let token = tokenizer.get();

    match token.ttype {
        TType::Operator(s) => {
            match &*s {
                "=" => {
                    tokenizer.consume();
                    let node_r = assign(tokenizer, frame)?;
                    node = new_node(BinOpKind::Assign, node, node_r, Loc { 0: token.line_num, 1: token.pos });
                    return Ok(node);
                },
                _ => return  Ok(node),
            }
        },
        _ => return  Ok(node),
    }

}


fn add(tokenizer : &mut Tokenizer,frame: &mut Frame) -> Result<Ast, ParseError> {
    let mut node = mul(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "+" => {
                        tokenizer.consume();
                        let node_r = mul(tokenizer, frame)?;
                        node = new_node(BinOpKind::Add, node, node_r, Loc{0:next_token.line_num, 1:next_token.pos});
                    }
                    "-" => {
                        tokenizer.consume();
                        let node_r = mul(tokenizer, frame)?;
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

fn unary(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let next_token = tokenizer.get();
    match next_token.ttype {
        TType::Operator(s) => {
            match &*s {
                "+" => {
                    tokenizer.consume();
                    return unary(tokenizer, frame);
                },
                "-" => {
                    let dummy_token = tokenizer.get();
                    tokenizer.consume();
                    let node_0 = node_number(0, &dummy_token)?;
                    let node_r = primary(tokenizer, frame)?;
                    let node = new_node(BinOpKind::Sub, node_0, node_r, Loc{0:next_token.line_num, 1:next_token.pos});
                    return Ok(node);
                },
                _ => return primary(tokenizer, frame),
            }
        }
        _ => {
            return primary(tokenizer, frame);
        }
    }
}

fn equality(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, ParseError> {
    let node = relational(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "==" => {
                        tokenizer.consume();
                        let node_r = relational(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Eq, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                        return Ok(node);
                    },
                    "!=" => {
                        tokenizer.consume();
                        let node_r = relational(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Ne, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                        return Ok(node);
                    },
                    _ => {
                        return Ok(node);
                    }
                }
            }
            _ => {
                return Ok(node);
            }
        }
    }
}




fn relational(tokenizer : &mut Tokenizer,frame: &mut Frame) -> Result<Ast, ParseError> {
    let node = add(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "<" => {
                        tokenizer.consume();
                        let node_r = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Lt, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                        return Ok(node);
                    },
                    "<=" => {
                        tokenizer.consume();
                        let node_r = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Le, node, node_r, Loc { 0: next_token.line_num, 1: next_token.pos });
                        return Ok(node);
                    },
                    ">" => {
                        tokenizer.consume();
                        let node_l = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Lt, node_l, node, Loc { 0: next_token.line_num, 1: next_token.pos });
                        return Ok(node);
                    },
                    ">=" => {
                        tokenizer.consume();
                        let node_l = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Le, node_l, node, Loc { 0: next_token.line_num, 1: next_token.pos });
                        return Ok(node);
                    },
                    _ => {
                        return Ok(node);
                    }
                }
            }
            _ => {
                return Ok(node);
            }
        }
    }
}

fn new_node(op :ast::BinOpKind, l: Ast, r: Ast, loc:Loc) -> Ast {

    let binop  = ast::BinOp{ value:op, loc: loc.clone()};
    let astkind = ast::AstKind::BinOp{op: binop, l: Box::new(l), r: Box::new(r)};
    let ast: Ast = Ast{value: astkind, loc};
    ast
}

fn new_unary(op :ast::UniOpKind, l: Ast, loc:Loc) -> Ast {

    let uniop  = ast::UniOp{ value:op, loc: loc.clone()};
    let astkind = ast::AstKind::UniOp{op: uniop, l: Box::new(l)};
    let ast: Ast = Ast{value: astkind, loc};
    ast
}

fn new_block(body :Vec<Box<Ast>>, loc:Loc) -> Ast {
    let ast: Ast = Ast{value: AstKind::Block { body }, loc: loc.clone()};
    ast
}


fn node_number(n: i64, token :&Token) -> Result<Ast, ParseError> {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let astkind = AstKind::Num(n);
    Ok(Ast{ value: astkind, loc})
}

fn node_variable(name: String, offset :i64, token: &Token) ->  Result<Ast, ParseError> {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let astkind = AstKind::LocalVar{name, offset};
    Ok(Ast{ value: astkind, loc})
}