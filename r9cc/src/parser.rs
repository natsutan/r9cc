use crate::tokenizer::*;
use crate::ast::*;
use std::error::Error;
use std::fmt;
use crate::ast;
use crate::typesys::{add_type, is_integer, is_pointer};


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


impl Parser {
    pub fn new() -> Parser {
        Parser {
            nodes: vec![],
            frame: vec![],
        }
    }

    pub fn parse(&mut self, tokenizer: &mut Tokenizer) -> Result<(), Box<dyn Error>> {

        let token = tokenizer.get();
        while !tokenizer.at_eof() {
            let func = function(tokenizer, &mut self.frame)?;
            self.nodes.push(func);
        }
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
fn stmt(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let token = tokenizer.get();
    match token.ttype {
        TType::Return => {
            tokenizer.consume();
            let node_l = expr(tokenizer, frame)?;
            let token_semicolon = tokenizer.get();
            if token_semicolon.ttype == TType::SemiColon {
                tokenizer.consume();
                let node = new_unary(UniOpKind::NdReturn, node_l, &token);
                Ok(node)
            } else {
                return Err(Box::new(ParseError{err: format!("token {:?} must be ;", token_semicolon)}))
            }
        }
        TType::LBrace => {
            tokenizer.consume();
            return compound_stmt(tokenizer, frame)
        },
        TType::If => {
            tokenizer.consume();

            skip(tokenizer, TType::LParen)?;
            let cond = expr(tokenizer, frame)?;
            skip(tokenizer, TType::RParen)?;
            let then = stmt(tokenizer, frame)?;
            let token_else = tokenizer.get();
            if token_else.ttype == TType::Else {
                tokenizer.consume();
                let els = stmt(tokenizer, frame)?;
                return Ok(new_if_else(cond, then, els, &token));
            } else {
                return Ok(new_if(cond, then, &token));
            }
        },
        TType::For => {
            tokenizer.consume();
            skip(tokenizer, TType::LParen)?;
            let init = expr_stmt(tokenizer, frame)?;
            //skip(tokenizer, Comma)?;
            let tc = tokenizer.get();
            let cond = match tc.ttype {
                TType::SemiColon => new_block(vec![], &token),
                _ => expr(tokenizer, frame)?
            };
            skip(tokenizer, TType::SemiColon)?;
            let ti = tokenizer.get();
            let inc =  match ti.ttype {
                TType::RParen => new_block(vec![], &token),
                _ => expr(tokenizer, frame)?
            };
            skip(tokenizer, TType::RParen)?;
            let then = stmt(tokenizer, frame)?;

            return Ok(new_for(init, cond, inc, then, &token));
        },
        TType::While => {
            tokenizer.consume();
            skip(tokenizer, TType::LParen)?;
            let cond = expr(tokenizer, frame)?;
            skip(tokenizer, TType::RParen)?;
            let then = stmt(tokenizer, frame)?;

            let init = new_block(vec![], &token);
            let inc = new_block(vec![], &token);

            return Ok(new_for(init, cond, inc, then, &token));

        }
        _ =>  expr_stmt(tokenizer, frame)
    }
}


// stmt = expr-stmt
fn compound_stmt(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let _token_head = tokenizer.get();
    let mut body :Vec<Box<Ast>> =  vec![];

    let mut token = tokenizer.get();
    while token.ttype != TType::RBrace {
        match token.ttype {
            TType::Int => {
                let mut dec = declaration(tokenizer, frame)?;
                add_type(&mut dec)?;
                body.push(Box::from(dec));
            }
            _ => {
                let mut st = stmt(tokenizer, frame)?;
                add_type(&mut st)?;
                body.push(Box::from(st));
            }
        }
        token = tokenizer.get();
    }
    tokenizer.consume();
    let node_block = new_block(body, &token);

    Ok(node_block)
}

// expr-stmt = expr ";"
fn expr_stmt(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {

    let token = tokenizer.get();
    if token.ttype == TType::SemiColon {
        tokenizer.consume();
        let node = new_block(vec![], &token);
        return Ok(node)
    }

    let node_expr = expr(tokenizer, frame)?;
    let node = new_unary(UniOpKind::NdExprStmt, node_expr, &token);

    let token_semicolon = tokenizer.get();
    if token_semicolon.ttype != TType::SemiColon {
        return Err(Box::new(ParseError{err: format!("expr_stmt must end ; {:?}) ", token_semicolon)}))
    }
    tokenizer.consume();

    return  Ok(node)
}

fn primary(tokenizer: &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let token = tokenizer.get();

    tokenizer.consume();
    match token.ttype {
        TType::Integer(n) => node_number(n, &token),
        TType::Identifier(s) => {
            let tk = tokenizer.get();
            let name = s.clone();

            //function call
            let token_rpalan = tokenizer.get();
            if token_rpalan.ttype == TType::LParen {
                let node = new_funccall(name, &tk);
                tokenizer.consume();
                skip(tokenizer, TType::RParen)?;
                return Ok(node);
            }

            //variable
            let search_result = find_lvar(&name, frame);
            match search_result {
                Ok(lv) => {
                    node_variable(name, lv.ntype, lv.offset, &tk)
                }
                Err(()) => {
                    return Err(Box::new(ParseError{err: format!("undefined variable {:?}  )", tk)}));
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
            Err(Box::new(ParseError{err: format!("token {:?} must be )", token)}))
        }
        _ => Err(Box::new(ParseError{err: format!("token {:?} must be number", token)})),
    }
}

fn mul(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = unary(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        let token_loc = next_token.clone();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "*" => {
                        tokenizer.consume();
                        let node_r = unary(tokenizer, frame)?;
                        node = new_node(BinOpKind::Mult, node, node_r, &token_loc);
                    }
                    "/" => {
                        tokenizer.consume();
                        let node_r = unary(tokenizer, frame)?;
                        node = new_node(BinOpKind::Div, node, node_r, &token_loc);
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

fn expr(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    return assign(tokenizer, frame);
}

fn assign (tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = equality(tokenizer, frame)?;
    let token = tokenizer.get();
    let token_loc = token.clone();
    match token.ttype {
        TType::Operator(s) => {
            match &*s {
                "=" => {
                    tokenizer.consume();
                    let node_r = assign(tokenizer, frame)?;
                    node = new_node(BinOpKind::Assign, node, node_r, &token_loc);
                    return Ok(node);
                },
                _ => return  Ok(node),
            }
        },
        _ => return  Ok(node),
    }

}


fn add(tokenizer : &mut Tokenizer,frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = mul(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        let token_loc = next_token.clone();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "+" => {
                        tokenizer.consume();
                        let mut node_r = mul(tokenizer, frame)?;
                        node = new_add(&mut node, &mut node_r, &token_loc)?;
                    }
                    "-" => {
                        tokenizer.consume();
                        let mut node_r = mul(tokenizer, frame)?;
                        node = new_sub(&mut node, &mut node_r, &token_loc)?;
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

fn unary(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let next_token = tokenizer.get();
    let token_loc = next_token.clone();
    let dummy_token = tokenizer.get();

    match next_token.ttype {
        TType::Operator(s) => {
            match &*s {
                "+" => {
                    tokenizer.consume();
                    return unary(tokenizer, frame);
                },
                "-" => {
                    tokenizer.consume();
                    let node_0 = node_number(0, &dummy_token)?;
                    let node_r = primary(tokenizer, frame)?;
                    let node = new_node(BinOpKind::Sub, node_0, node_r, &token_loc);
                    return Ok(node);
                },
                "*" => {
                    tokenizer.consume();
                    let node_l = unary(tokenizer, frame)?;
                    let node = new_unary(UniOpKind::Deref,  node_l, &token_loc);
                    return Ok(node);
                }
                "&" => {
                    tokenizer.consume();
                    let node_l = unary(tokenizer, frame)?;
                    let node = new_unary(UniOpKind::Addr, node_l,  &token_loc);
                    return Ok(node);
                }

                _ => return primary(tokenizer, frame),
            }
        }
        _ => {
            return primary(tokenizer, frame);
        }
    }
}

fn equality(tokenizer : &mut Tokenizer, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let node = relational(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        let next_token_loc = next_token.clone();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "==" => {
                        tokenizer.consume();
                        let node_r = relational(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Eq, node, node_r, &next_token_loc);
                        return Ok(node);
                    },
                    "!=" => {
                        tokenizer.consume();
                        let node_r = relational(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Ne, node, node_r, &next_token_loc);
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




fn relational(tokenizer : &mut Tokenizer,frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let node = add(tokenizer, frame)?;

    loop {
        let next_token = tokenizer.get();
        let token_loc = next_token.clone();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "<" => {
                        tokenizer.consume();
                        let node_r = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Lt, node, node_r, &token_loc);
                        return Ok(node);
                    },
                    "<=" => {
                        tokenizer.consume();
                        let node_r = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Le, node, node_r, &token_loc);
                        return Ok(node);
                    },
                    ">" => {
                        tokenizer.consume();
                        let node_l = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Lt, node_l, node, &token_loc);
                        return Ok(node);
                    },
                    ">=" => {
                        tokenizer.consume();
                        let node_l = add(tokenizer, frame)?;
                        let node = new_node(BinOpKind::Le, node_l, node, &token_loc);
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



fn declaration(tokenizer : &mut Tokenizer,frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let base_type = declspec(tokenizer)?;
    //tokenizer.consume();
    let mut token = tokenizer.get();
    let mut i:i64 = 0;
    let mut blcok: Vec<Box<Ast>> = vec![];

    while token.ttype != TType::SemiColon {
        let tk = tokenizer.get();
        if i > 0 {
            skip(tokenizer, TType::Comma)?;
        }
        i = i+1;
        let ntype = declarator(tokenizer, &base_type)?;
        let var_name = get_ident(tokenizer)?;

        let search_result = find_lvar(&var_name, frame);
        match search_result {
            Ok(_) => {
                return Err(Box::new(ParseError{err: format!("duplicate variable {:?}  )", tk)}));
            }
            Err(()) => {}
        }
        let lhs = new_lvar(var_name, ntype, frame, &tk)?;

        let token_eq = tokenizer.get();
        match token_eq.ttype {
            TType::Operator(s) => {
                if s != "=" {
                    token = tokenizer.get();
                    continue;
                }
            }
            _ => {
                token = tokenizer.get();
                continue;
            }
        }
        tokenizer.consume();

        let rhs = assign(tokenizer, frame)?;
        let node_assign = new_node(BinOpKind::Assign, lhs, rhs, &tk);
        let node_expr_stmt = new_unary(UniOpKind::NdExprStmt, node_assign, &tk);
        blcok.push(Box::new(node_expr_stmt));

        token = tokenizer.get();
    }
    Ok(new_block(blcok, &token))
}

fn function(tokenizer : &mut Tokenizer,frame: &mut Frame) -> Result<Function, Box<dyn Error>> {
    let ntype = declspec(tokenizer)?;
    let return_type = declarator(tokenizer, &ntype)?;

    let func_name = get_ident(tokenizer)?;


    skip(tokenizer, TType::LParen)?;
    let mut params = create_param_lvars(tokenizer)?;
    skip(tokenizer, TType::RParen)?;
    skip(tokenizer, TType::LBrace)?;

    let mut locals = params.clone();

    let body = compound_stmt(tokenizer, &mut locals)?;
    let stack_size = (locals.len() * 8) as u64 ;

    // println!("func name = {}", func_name);
    // println!("params = {:?}", params);
    // println!("return type = {:?}", ntype);
    // println!("stack size  = {}", stack_size);

    Ok(Function{name: func_name , params, locals, stack_size, body, return_type})
}

fn create_param_lvars(tokenizer :&mut Tokenizer) -> Result<Frame, Box<dyn Error>> {
    let mut frame :Frame = vec![];
    let mut token = tokenizer.get();
    let mut i:i64 = 0;

    while token.ttype != TType::RParen {
        let tk = tokenizer.get();
        if i > 0 {
            skip(tokenizer, TType::Comma)?;
        }
        i = i + 1;
        let base_type = declspec(tokenizer)?;
        let ntype = declarator(tokenizer, &base_type)?;
        let var_name = get_ident(tokenizer)?;
        let search_result = find_lvar(&var_name, &frame);
        match search_result {
            Ok(_) => {
                return Err(Box::new(ParseError { err: format!("duplicate parameter {:?}  )", tk) }));
            }
            Err(()) => {}
        }
        let token_loc = tokenizer.get();
        new_lvar(var_name, ntype, &mut frame, &token_loc)?;
        token = tokenizer.get();
    }


    Ok(frame)
}


fn new_lvar(name: String, ntype: NodeType, frame: &mut Frame, token :&Token) -> Result<Ast, Box<dyn Error>> {
    let offset = -(frame.len() as i64 +  1) * 8;
    let new_lv = LocalVariable{name: name.clone(), ntype: ntype.clone(), offset};
    frame.push(new_lv);
    node_variable(name, ntype, offset, token)
}

fn get_ident(tokenizer : &mut Tokenizer) -> Result<String, Box<dyn Error>>  {
    let token = tokenizer.get();
    match token.ttype {
        TType::Identifier(s) => {
            tokenizer.consume();
            Ok(s)
        }
        _ => Err(Box::new(ParseError{err: format!("expected an identifier. {:?}", token)})),
    }
}

fn declspec(tokenizer : &mut Tokenizer) -> Result<NodeType, Box<dyn Error>> {
    skip(tokenizer, TType::Int)?;
    Ok(NodeType{kind: NodeTypeKind::Int, base: None})
}

fn declarator(tokenizer : &mut Tokenizer, ntype: &NodeType) -> Result<NodeType, Box<dyn Error>> {
    let mut token = tokenizer.get();
    let mut node_type = ntype.clone();

    while is_token_ast(&token) {
        node_type = pointer_to(&node_type);
        tokenizer.consume();
        token = tokenizer.get();
    }

    match token.ttype {
        TType::Identifier(_) => {
            Ok(node_type)
        }
        _ => Err(Box::new(ParseError{err: format!("expected a variable name {:?}.", token)}))
    }
}

fn pointer_to(base_type: &NodeType) -> NodeType {
    NodeType{kind: NodeTypeKind::Ptr, base: Some(Box::new(base_type.clone()))}
}

fn is_token_ast(token :&Token) -> bool {
    match &token.ttype {
        TType::Operator(s) => {
            s == "*"
        },
        _ => false
    }
}


fn new_node(op :ast::BinOpKind, l: Ast, r: Ast, token: &Token) -> Ast {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let binop  = BinOp::new(op, Box::new(l),  Box::new(r), token);
    Ast{value: AstKind::BinOp(binop), loc}
}

fn new_node_int(op :ast::BinOpKind, l: Ast, r: Ast, token: &Token) -> Ast {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let mut binop  = BinOp::new(op, Box::new(l),  Box::new(r), token);
    let ntype = NodeType{kind: NodeTypeKind::Int, base: None};
    binop.set_node_type(ntype);
    Ast{value: AstKind::BinOp(binop), loc}
}

fn new_unary(op :ast::UniOpKind, l: Ast, token: &Token) -> Ast {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let uniop = UniOp::new(op, Box::new(l), token);
    Ast{value: AstKind::UniOp(uniop), loc}
}

fn new_block(body :Vec<Box<Ast>>, token: &Token) -> Ast {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    Ast{value: AstKind::Block { body }, loc}
}

fn new_if_else(cond :Ast, then :Ast, els :Ast, token: &Token) -> Ast {
    let cond_box = Box::new(cond);
    let then_box = Box::new(then);
    let els_box = Box::new(els);
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    Ast{value: AstKind::If_ {cond: cond_box , then: then_box, els: els_box }, loc}
}


fn new_if(cond :Ast, then :Ast , token: &Token) -> Ast {
    let els = new_block(vec![],  token);
    new_if_else(cond, then, els, token)
}

fn new_for(init :Ast, cond :Ast, inc :Ast, then :Ast , token: &Token) -> Ast {
    let init_box = Box::new(init);
    let cond_box = Box::new(cond);
    let inc_box = Box::new(inc);
    let then_box = Box::new(then);
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    Ast{value: AstKind::For {init: init_box, cond: cond_box, inc: inc_box, then: then_box}, loc}
}


fn new_funccall(funcname: String, token: &Token) -> Ast {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    Ast{value: AstKind::FunCall { funcname, args: vec![] }, loc}
}


fn get_type(node :&Ast) -> Option<NodeType> {
    match node.clone().value {
        AstKind::BinOp(binop) => Some(binop.ntype.clone()),
        AstKind::UniOp(uniop) => Some(uniop.ntype.clone()),
        AstKind::Num {n : _, ntype} => Some(ntype.clone()),
        _ => None
    }
}





fn new_add(l: &mut Ast, r: &mut Ast, token: &Token) -> Result<Ast, Box<dyn Error>>  {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    add_type(l)?;
    add_type(r)?;

    if is_integer(l) && is_integer(r) {
        let binop  = BinOp::new(BinOpKind::Add, Box::new(l.clone()),  Box::new(r.clone()), token);
        return Ok(Ast{value: AstKind::BinOp(binop), loc});
    }

    let ltype0 = get_type(&l);
    let rtype0 = get_type(&r);


    let (ltype1, rtype1) = match (ltype0, rtype0) {
        (Some(l), Some(r)) => (l, r),
        _ => return Err(Box::new(ParseError{err: format!("operand of add has no type {:?} {:?} ", l, r)})),
    };

    let (lhs, rhs) = match (&ltype1.base, &rtype1.base) {
        (None, Some(_b)) => (r, l), //num + pointer swap
        _=> (l, r)
    };

    //ltype2:pointer
    //rtype2:num
    let obj_size = node_number(8, &token)?;
    let node_mul = new_node(BinOpKind::Mult, rhs.clone(), obj_size, &token);
    let binop  = new_node(BinOpKind::Add, lhs.clone(),  node_mul, token);
    return Ok(binop);
}

fn new_sub(l: &mut Ast, r: &mut Ast, token: &Token) -> Result<Ast, Box<dyn Error>>  {
    let loc = Loc{ 0: token.line_num, 1:token.pos };

    add_type(l)?;
    add_type(r)?;

    if is_integer(l) && is_integer(r) {
        let binop  = BinOp::new(BinOpKind::Sub, Box::new(l.clone()),  Box::new(r.clone()), token);
        return Ok(Ast{value: AstKind::BinOp(binop), loc});
    }

    let ltype0 = get_type(&l);
    let rtype0 = get_type(&r);

    let (ltype1, rtype1) = match (ltype0, rtype0) {
        (Some(l), Some(r)) => (l, r),
        _ => return Err(Box::new(ParseError{err: format!("operand of sub has no type {:?} {:?} ", l, r)})),
    };

    let (lhs, rhs) = match (&ltype1.base, &rtype1.base) {
        (None, Some(_b)) => (r, l), //num + pointer swap
        _=> (l, r)
    };

    let obj_size = node_number(8, &token)?;

    // ptr - ptr, which returns how many elements are between the two.
    if is_pointer(lhs) && is_pointer(rhs) {
        let sub_op = new_node_int(BinOpKind::Sub, lhs.clone(), rhs.clone(), &token);
        let binop = new_node_int(BinOpKind::Div, sub_op, obj_size, &token);
        return Ok(binop);
    }

    //ltype2:pointer
    //rtype2:num
    let node_mul = new_node(BinOpKind::Mult, rhs.clone(), obj_size, &token);
    let binop  = new_node(BinOpKind::Sub, lhs.clone(),  node_mul, token);
    return Ok(binop);
}

fn node_number(n: i64, token :&Token) -> Result<Ast, Box<dyn Error>> {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let astkind = AstKind::Num{n, ntype: NodeType{kind: NodeTypeKind::Int, base: None }};
    Ok(Ast{ value: astkind, loc})
}

fn node_variable(name: String, ntype:NodeType, offset :i64, token: &Token) ->  Result<Ast, Box<dyn Error>> {
    let loc = Loc{ 0: token.line_num, 1:token.pos };
    let astkind = AstKind::LocalVar{name, ntype, offset};
    Ok(Ast{ value: astkind, loc})
}

fn skip(tokenizer: &mut Tokenizer, token :TType) -> Result<(), Box<dyn Error>> {
    let token_next = tokenizer.get();
    if token_next.ttype != token {
        return Err(Box::new(ParseError{err: format!("SKIP:token {:?} must be {:?} ", token_next, token)}));
    }
    tokenizer.consume();
    Ok(())
}
