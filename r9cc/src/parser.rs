use crate::tokenizer::*;
use crate::ast::*;
use std::error::Error;
use std::fmt;
use crate::ast;
use crate::ast::AstKind::LocalVar;
use crate::ast::UniOpKind::Deref;
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
    pub locals: Frame,
    pub globals: Frame,
}



impl Parser {
    pub fn new() -> Parser {
        Parser {
            nodes: vec![],
            locals: vec![],
            globals: vec![]
        }
    }

    pub fn parse(&mut self, tokenizer: &mut Tokenizer) -> Result<(), Box<dyn Error>> {
        while !tokenizer.at_eof() {
            let base_type = declspec(tokenizer)?;

            if is_function(tokenizer)? {
                let func = function(tokenizer, &base_type, &mut self.globals)?;
                self.nodes.push(func);
                continue
            }

            global_variable(tokenizer, &base_type, &mut self.globals)?;

        }
        Ok(())
    }

}

fn find_lvar(name: &String, locals: &Frame, globals: &Frame) -> Result<Obj, ()> {
    for lv in locals.iter() {
        if *name == lv.name {
            return Ok(lv.clone())
        }
    }

    for lv in globals.iter() {
        if *name == lv.name {
            return Ok(lv.clone())
        }
    }

    Err(())
}


// stmt = expr-stmt
fn stmt(tokenizer: &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let token = tokenizer.get();
    match token.ttype {
        TType::Return => {
            tokenizer.consume();
            let node_l = expr(tokenizer, locals, globals)?;
            let token_semicolon = tokenizer.get();
            if token_semicolon.ttype == TType::SemiColon {
                tokenizer.consume();
                let node = new_unary(UniOpKind::NdReturn, node_l);
                Ok(node)
            } else {
                return Err(Box::new(ParseError{err: format!("token {:?} must be ;", token_semicolon)}))
            }
        }
        TType::LBrace => {
            tokenizer.consume();
            return compound_stmt(tokenizer, locals, globals)
        },
        TType::If => {
            tokenizer.consume();

            skip(tokenizer, TType::LParen)?;
            let cond = expr(tokenizer, locals, globals)?;
            skip(tokenizer, TType::RParen)?;
            let then = stmt(tokenizer, locals, globals)?;
            let token_else = tokenizer.get();
            if token_else.ttype == TType::Else {
                tokenizer.consume();
                let els = stmt(tokenizer, locals, globals)?;
                return Ok(new_if_else(cond, then, els));
            } else {
                return Ok(new_if(cond, then));
            }
        },
        TType::For => {
            tokenizer.consume();
            skip(tokenizer, TType::LParen)?;
            let init = expr_stmt(tokenizer, locals, globals)?;
            //skip(tokenizer, Comma)?;
            let tc = tokenizer.get();
            let cond = match tc.ttype {
                TType::SemiColon => new_block(vec![]),
                _ => expr(tokenizer, locals, globals)?
            };
            skip(tokenizer, TType::SemiColon)?;
            let ti = tokenizer.get();
            let inc =  match ti.ttype {
                TType::RParen => new_block(vec![]),
                _ => expr(tokenizer, locals, globals)?
            };
            skip(tokenizer, TType::RParen)?;
            let then = stmt(tokenizer, locals, globals)?;

            return Ok(new_for(init, cond, inc, then));
        },
        TType::While => {
            tokenizer.consume();
            skip(tokenizer, TType::LParen)?;
            let cond = expr(tokenizer, locals, globals)?;
            skip(tokenizer, TType::RParen)?;
            let then = stmt(tokenizer, locals, globals)?;

            let init = new_block(vec![]);
            let inc = new_block(vec![]);

            return Ok(new_for(init, cond, inc, then));

        }
        _ =>  expr_stmt(tokenizer, locals, globals)
    }
}


// stmt = expr-stmt
fn compound_stmt(tokenizer: &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let _token_head = tokenizer.get();
    let mut body :Vec<Box<Ast>> =  vec![];

    let mut token = tokenizer.get();
    while token.ttype != TType::RBrace {
        match token.ttype {
            TType::Int => {
                let mut dec = declaration(tokenizer, locals, globals)?;
                add_type(&mut dec)?;
                body.push(Box::from(dec));
            }
            _ => {
                let mut st = stmt(tokenizer, locals, globals)?;
                add_type(&mut st)?;
                body.push(Box::from(st));
            }
        }
        token = tokenizer.get();
    }
    tokenizer.consume();
    let node_block = new_block(body);

    Ok(node_block)
}

// expr-stmt = expr ";"
fn expr_stmt(tokenizer: &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {

    let token = tokenizer.get();
    if token.ttype == TType::SemiColon {
        tokenizer.consume();
        let node = new_block(vec![]);
        return Ok(node)
    }

    let node_expr = expr(tokenizer, locals, globals)?;
    let node = new_unary(UniOpKind::NdExprStmt, node_expr);

    let token_semicolon = tokenizer.get();
    if token_semicolon.ttype != TType::SemiColon {
        return Err(Box::new(ParseError{err: format!("expr_stmt must end ; {:?}) ", token_semicolon)}))
    }
    tokenizer.consume();

    return  Ok(node)
}

fn primary(tokenizer: &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let token = tokenizer.get();

    tokenizer.consume();
    match token.ttype {
        TType::Integer(n) => node_number(n),
        TType::Identifier(s) => {
            let tk = tokenizer.get();
            let name = s.clone();

            //function call
            let token_rpalan = tokenizer.get();
            if token_rpalan.ttype == TType::LParen {
                tokenizer.consume();
                return funcall(tokenizer, name, locals, globals);
            }

            //variable
            let search_result = find_lvar(&name, locals, globals);
            match search_result {
                Ok(lv) => {
                    node_variable(name, lv.ntype, lv.offset)
                }
                Err(()) => {
                    return Err(Box::new(ParseError{err: format!("undefined variable {:?}  )", tk)}));
                }
            }
        },
        TType::LParen => {
            let node = expr(tokenizer, locals,globals)?;
            let next_token = tokenizer.get();
            if next_token.ttype == TType::RParen {
                tokenizer.consume();
                return Ok(node);
            }
            Err(Box::new(ParseError{err: format!("token {:?} must be )", token)}))
        }
        TType::SizeOf => {
            let mut node = unary(tokenizer, locals, globals)?;
            let node_type = add_type(&mut node)?;
            let target_size = match node_type {
                Some(nt) => nt.size,
                _ => return Err(Box::new(ParseError{err: format!("target of sizeof has no nodetype {:?}  )", node)})),
            };
            node_number(target_size as i64)
        }
        _ => Err(Box::new(ParseError{err: format!("token {:?} must be number", token)})),
    }
}

fn funcall(tokenizer : &mut Tokenizer, funcname: String, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {

    let mut args :Vec<Box<Ast>> = vec![];
    let mut token = tokenizer.get();

    let mut first = true;
    while token.ttype != TType::RParen {
        if !first {
            skip(tokenizer, TType::Comma)?;
        }
        first = false;
        let node = assign(tokenizer, locals, globals)?;
        args.push(Box::new(node));
        token = tokenizer.get();
    }
    skip(tokenizer, TType::RParen)?;

    let node = new_funccall(funcname, args);
    return Ok(node);
}

fn mul(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = unary(tokenizer, locals, globals)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "*" => {
                        tokenizer.consume();
                        let node_r = unary(tokenizer, locals, globals)?;
                        node = new_node(BinOpKind::Mult, node, node_r);
                    }
                    "/" => {
                        tokenizer.consume();
                        let node_r = unary(tokenizer, locals, globals)?;
                        node = new_node(BinOpKind::Div, node, node_r);
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

fn expr(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    return assign(tokenizer, locals, globals);
}

fn assign (tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = equality(tokenizer, locals, globals)?;
    let token = tokenizer.get();
    match token.ttype {
        TType::Operator(s) => {
            match &*s {
                "=" => {
                    tokenizer.consume();
                    let node_r = assign(tokenizer, locals, globals)?;
                    node = new_node(BinOpKind::Assign, node, node_r);
                    return Ok(node);
                },
                _ => return  Ok(node),
            }
        },
        _ => return  Ok(node),
    }

}


fn add(tokenizer : &mut Tokenizer, locals: &mut Frame, globals :&mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = mul(tokenizer, locals, globals)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "+" => {
                        tokenizer.consume();
                        let mut node_r = mul(tokenizer, locals, globals)?;
                        node = new_add(&mut node, &mut node_r)?;
                    }
                    "-" => {
                        tokenizer.consume();
                        let mut node_r = mul(tokenizer, locals, globals)?;
                        node = new_sub(&mut node, &mut node_r)?;
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

fn unary(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let next_token = tokenizer.get();

    match next_token.ttype {
        TType::Operator(s) => {
            match &*s {
                "+" => {
                    tokenizer.consume();
                    return unary(tokenizer, locals, globals);
                },
                "-" => {
                    tokenizer.consume();
                    let node_0 = node_number(0)?;
                    let node_r = primary(tokenizer, locals, globals)?;
                    let node = new_node(BinOpKind::Sub, node_0, node_r);
                    return Ok(node);
                },
                "*" => {
                    tokenizer.consume();
                    let node_l = unary(tokenizer, locals, globals)?;
                    let node = new_unary(UniOpKind::Deref,  node_l);
                    return Ok(node);
                }
                "&" => {
                    tokenizer.consume();
                    let node_l = unary(tokenizer, locals, globals)?;
                    let node = new_unary(UniOpKind::Addr, node_l);
                    return Ok(node);
                }

                _ => return postfix(tokenizer, locals, globals),
            }
        }
        _ => {
            return postfix(tokenizer, locals, globals);
        }
    }
}

fn postfix(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let mut node = primary(tokenizer, locals, globals)?;
    let mut token = tokenizer.get();

    while token.ttype == TType::LBracket {
        tokenizer.consume();
        let mut idx = expr(tokenizer, locals, globals)?;
        skip(tokenizer, TType::RBracket)?;
        let node_add = new_add(&mut node.clone(), &mut idx.clone())?;
        node = new_unary(UniOpKind::Deref, node_add);
        token = tokenizer.get();

    }

    Ok(node)
}

fn equality(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let node = relational(tokenizer, locals, globals)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "==" => {
                        tokenizer.consume();
                        let node_r = relational(tokenizer, locals, globals)?;
                        let node = new_node(BinOpKind::Eq, node, node_r);
                        return Ok(node);
                    },
                    "!=" => {
                        tokenizer.consume();
                        let node_r = relational(tokenizer, locals, globals)?;
                        let node = new_node(BinOpKind::Ne, node, node_r);
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




fn relational(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let node = add(tokenizer, locals, globals)?;

    loop {
        let next_token = tokenizer.get();
        match next_token.ttype {
            TType::Operator(s) => {
                match &*s {
                    "<" => {
                        tokenizer.consume();
                        let node_r = add(tokenizer, locals, globals)?;
                        let node = new_node(BinOpKind::Lt, node, node_r);
                        return Ok(node);
                    },
                    "<=" => {
                        tokenizer.consume();
                        let node_r = add(tokenizer, locals, globals)?;
                        let node = new_node(BinOpKind::Le, node, node_r);
                        return Ok(node);
                    },
                    ">" => {
                        tokenizer.consume();
                        let node_l = add(tokenizer, locals, globals)?;
                        let node = new_node(BinOpKind::Lt, node_l, node);
                        return Ok(node);
                    },
                    ">=" => {
                        tokenizer.consume();
                        let node_l = add(tokenizer, locals, globals)?;
                        let node = new_node(BinOpKind::Le, node_l, node);
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



fn declaration(tokenizer : &mut Tokenizer, locals: &mut Frame, globals: &mut Frame) -> Result<Ast, Box<dyn Error>> {
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
        let (var_name, ntype) = declarator(tokenizer, &base_type)?;

        let search_result = find_lvar(&var_name, locals, globals);
        match search_result {
            Ok(_) => {
                return Err(Box::new(ParseError{err: format!("duplicate variable {:?}  )", tk)}));
            }
            Err(()) => {}
        }
        let lhs = new_lvar(var_name, ntype, locals)?;

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

        let rhs = assign(tokenizer, locals, globals)?;
        let node_assign = new_node(BinOpKind::Assign, lhs, rhs);
        let node_expr_stmt = new_unary(UniOpKind::NdExprStmt, node_assign);
        blcok.push(Box::new(node_expr_stmt));

        token = tokenizer.get();
    }
    Ok(new_block(blcok))
}

fn function(tokenizer : &mut Tokenizer, base_type: &NodeType, globals :&mut Frame) -> Result<Obj, Box<dyn Error>> {
    let (func_name, return_type) = declarator(tokenizer, &base_type)?;

    skip(tokenizer, TType::LParen)?;
    let params = create_param_lvars(tokenizer)?;
    skip(tokenizer, TType::RParen)?;
    skip(tokenizer, TType::LBrace)?;

    let mut locals = params.clone();

    let body = compound_stmt(tokenizer, &mut locals, globals)?;
    let stack_size = (locals.len() * 8) as u64 ;

    println!("create function: {}", func_name);

    Ok(new_func(func_name , params, locals, stack_size, body, return_type))
}


fn global_variable(tokenizer : &mut Tokenizer, base_type: &NodeType, globals: &mut Frame) -> Result<(), Box<dyn Error>> {
    let mut token = tokenizer.get();
    let mut first = true;

    while  token.ttype != TType::SemiColon {
        if !first {
            skip(tokenizer, TType::Comma)?;
        }
        first = false;

        let (var_name, ntype) = declarator(tokenizer, base_type)?;
        new_gvar(var_name, ntype, globals);
        token = tokenizer.get();
    }

    tokenizer.consume();
    Ok(())
}

// 先読みして、(か;で関数かグローバル変数かを判断する。
fn is_function(tokenizer: &mut Tokenizer) ->  Result<bool, Box<dyn Error>> {
    let mut token = tokenizer.get();
    let save = tokenizer.get_pos();

    while token.ttype != TType::EOF {
        match token.ttype {
            TType::LParen => {
                tokenizer.set_pos(save);
                return Ok(true);
            }
            TType::SemiColon => {
                tokenizer.set_pos(save);
                return Ok(false);
            }
            _ => {
                tokenizer.consume();
                token = tokenizer.get();
            }
        }
    }

    Err(Box::new(ParseError { err: format!("unexpeted EOF)") }))
}



fn new_func(name: String, params :Vec<Obj>, locals :Vec<Obj>, stack_size: u64, body :Ast, return_type: NodeType) -> Obj {
    Obj { name, ntype:return_type.clone(), params, locals, stack_size, body, return_type, is_local: false, is_func: true, offset: 0}
}


fn create_param_lvars(tokenizer :&mut Tokenizer) -> Result<Frame, Box<dyn Error>> {
    let mut locals:Frame = vec![];
    let globals_empty:Frame = vec![];
    let mut token = tokenizer.get();
    let mut i:i64 = 0;

    while token.ttype != TType::RParen {
        let tk = tokenizer.get();
        if i > 0 {
            skip(tokenizer, TType::Comma)?;
        }
        i = i + 1;
        let base_type = declspec(tokenizer)?;
        let (var_name, ntype) = declarator(tokenizer, &base_type)?;

        let search_result = find_lvar(&var_name, &locals, &globals_empty);
        match search_result {
            Ok(_) => {
                return Err(Box::new(ParseError { err: format!("duplicate parameter {:?}  )", tk) }));
            }
            Err(()) => {}
        }
        new_lvar(var_name, ntype, &mut locals)?;
        token = tokenizer.get();
    }
    Ok(locals)
}


fn new_lvar(name: String, ntype: NodeType, frame: &mut Frame) -> Result<Ast, Box<dyn Error>> {
    let offset = -(frame.len() as i64 +  1) * 8;
//    let new_lv = Obj {name: name.clone(), ntype: ntype.clone(), offset};
    let new_lv = Obj{
        name: name.clone(),
        ntype: ntype.clone(),
        params: vec![],
        locals: vec![],
        stack_size: 0,
        body: LocalVar {name: name.clone(), ntype: ntype.clone(), offset},
        return_type: ntype.clone(),
        is_local: true,
        is_func: false,
        offset: 0
    };


    frame.push(new_lv);
    node_variable(name, ntype, offset)
}

fn new_gvar(name :String, ntype :NodeType, globals: &mut Frame) {
    let new_gvar = Obj{
        name: name.clone(),
        ntype: ntype.clone(),
        params: vec![],
        locals: vec![],
        stack_size: 0,
        body: LocalVar {name: name.clone(), ntype: ntype.clone(), offset:0},
        return_type: ntype.clone(),
        is_local: false,
        is_func: false,
        offset: 0
    };
    globals.push(new_gvar);
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
    Ok(NodeType{kind: NodeTypeKind::Int, size: 8, len:0, base: None})
}

fn declarator(tokenizer : &mut Tokenizer, ntype: &NodeType) -> Result<(String,NodeType), Box<dyn Error>> {
    let mut token = tokenizer.get();
    let mut node_type = ntype.clone();

    while is_token_ast(&token) {
        node_type = pointer_to(&node_type);
        tokenizer.consume();
        token = tokenizer.get();
    }

    match token.ttype {
        TType::Identifier(s) => {
            tokenizer.consume();
            let suffix = type_suffix(tokenizer, &node_type)?;
            Ok((s, suffix))
        }
        _ => Err(Box::new(ParseError{err: format!("expected a variable name {:?}.", token)}))
    }
}

fn type_suffix(tokenizer : &mut Tokenizer, ntype: &NodeType) -> Result<NodeType, Box<dyn Error>> {

    let mut token = tokenizer.get();

    if token.ttype == TType::LParen {
        let func_type = NodeType{kind :NodeTypeKind::Func, size :0, len  : 0, base: None};
        return Ok(func_type);
    }
    // [ 配列のindex
    if token.ttype == TType::LBracket {
        let mut token_md = tokenizer.get();
        let mut dim_vec: Vec<NodeType> = vec![];
        while token_md.ttype == TType::LBracket {
            tokenizer.consume();
            token = tokenizer.get();
            let sz = get_number(&token)?;  //いったん個数を入れる
            tokenizer.consume();
            skip(tokenizer, TType::RBracket)?;
            dim_vec.push(NodeType { kind: NodeTypeKind::Array, size: sz as usize, len: sz as usize, base: Some(Box::new(ntype.clone())) });
            token_md = tokenizer.get();
        }

        let mut pre_ntype = Box::new(ntype.clone());
        let mut md_ntype = NodeType { kind: NodeTypeKind::Array, size: ntype.size, len: ntype.len, base: Some(Box::new(ntype.clone()))};
        let mut md_size = ntype.size;
        for n in dim_vec.iter().rev() {
            md_size = md_size * n.size;
            md_ntype = NodeType { kind: NodeTypeKind::Array, size: md_size, len: n.len ,base: Some(pre_ntype.clone())};

            pre_ntype = Box::new(md_ntype.clone());
        }
        return Ok(md_ntype);
    }

    // それ以外
    Ok(ntype.clone())
}

fn func_params(tokenizer : &mut Tokenizer, ntype: &NodeType) -> Result<NodeType, Box<dyn Error>> {
    let mut token = tokenizer.get();
    let mut first = true;

    let mut type_list: Vec<NodeType> = vec![];

    while token.ttype != TType::RParen {
        if !first {
            skip(tokenizer, TType::Comma)?;
            first = false;
        }
        let base_type = declspec(tokenizer)?;
        let (_, ty) = declarator(tokenizer, &base_type)?;
        type_list.push(ty);

        token = tokenizer.get();
    }


    //TODO　ここから

    Ok(ntype.clone())
}

fn pointer_to(base_type: &NodeType) -> NodeType {
    NodeType{kind: NodeTypeKind::Ptr, size:8, len:0, base: Some(Box::new(base_type.clone()))}
}

fn is_token_ast(token :&Token) -> bool {
    match &token.ttype {
        TType::Operator(s) => {
            s == "*"
        },
        _ => false
    }
}

fn get_number(token: &Token)-> Result<i64, Box<dyn Error>>  {
    match token.ttype {
        TType::Integer(n) => Ok(n),
        _ =>  Err(Box::new(ParseError{err: format!("expected a number {:?}.", token)})),
    }
}


fn new_node(op :ast::BinOpKind, l: Ast, r: Ast) -> Ast {
    let binop  = BinOp::new(op, Box::new(l),  Box::new(r));
    AstKind::BinOp(binop)
}

fn new_node_int(op :ast::BinOpKind, l: Ast, r: Ast) -> Ast {
    let mut binop  = BinOp::new(op, Box::new(l),  Box::new(r));
    let ntype = NodeType{kind: NodeTypeKind::Int, size:8, len:0, base: None};
    binop.set_node_type(ntype);
    AstKind::BinOp(binop)
}

fn new_node_ntype(op :ast::BinOpKind, l: Ast, r: Ast, ntype:NodeType) -> Ast {
    let mut binop  = BinOp::new(op, Box::new(l),  Box::new(r));
    binop.set_node_type(ntype);
    AstKind::BinOp(binop)
}

fn new_unary(op :ast::UniOpKind, l: Ast) -> Ast {
    let uniop = UniOp::new(op, Box::new(l));
    AstKind::UniOp(uniop)
}

fn new_block(body :Vec<Box<Ast>>) -> Ast {
    AstKind::Block { body }
}

fn new_if_else(cond :Ast, then :Ast, els :Ast) -> Ast {
    let cond_box = Box::new(cond);
    let then_box = Box::new(then);
    let els_box = Box::new(els);
    AstKind::If_ {cond: cond_box , then: then_box, els: els_box }
}


fn new_if(cond :Ast, then :Ast) -> Ast {
    let els = new_block(vec![]);
    new_if_else(cond, then, els)
}

fn new_for(init :Ast, cond :Ast, inc :Ast, then :Ast) -> Ast {
    let init_box = Box::new(init);
    let cond_box = Box::new(cond);
    let inc_box = Box::new(inc);
    let then_box = Box::new(then);
    AstKind::For {init: init_box, cond: cond_box, inc: inc_box, then: then_box}
}


fn new_funccall(funcname: String, args: Vec<Box<Ast>>) -> Ast {
    let ntype = NodeType{kind:NodeTypeKind::Int, size:8, len:0, base: None};
    AstKind::FunCall { funcname, ntype, args}
}


fn get_type(node :&Ast) -> Option<NodeType> {
    match node.clone()  {
        AstKind::BinOp(binop) => Some(binop.ntype.clone()),
        AstKind::UniOp(uniop) => Some(uniop.ntype.clone()),
        AstKind::Num {n : _, ntype} => Some(ntype.clone()),
        AstKind::FunCall {funcname:_, args: _, ntype} => Some(ntype.clone()),
        AstKind::LocalVar {name:_ , ntype, offset:_} => Some(ntype),
        _ => None
    }
}


fn new_add(l: &mut Ast, r: &mut Ast) -> Result<Ast, Box<dyn Error>>  {
    add_type(l)?;
    add_type(r)?;

    if is_integer(l) && is_integer(r) {
        let binop  = BinOp::new(BinOpKind::Add, Box::new(l.clone()),  Box::new(r.clone()));
        return Ok(AstKind::BinOp(binop));
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
    let obj_size = match &lhs {
        AstKind::LocalVar {name: _, ntype, offset: _ } => {
            if ntype.kind == NodeTypeKind::Array {
                match ntype.base.clone() {
                    Some(b) => b.size,
                    _ => return Err(Box::new(ParseError{err: format!("Array has no base {:?}", lhs)})),
                }
            } else {
                ntype.size
            }
        },
        AstKind::UniOp(uniop) => {
            if uniop.op == Deref {
                if uniop.ntype.kind == NodeTypeKind::Array {
                    match uniop.ntype.base.clone() {
                        Some(b) => b.size,
                        None => return Err(Box::new(ParseError { err: format!("deref + array is not basetype {:?} ", uniop) })),
                    }
                } else {
                    uniop.ntype.size
                }
            } else {
                match &*uniop.l {
                    AstKind::LocalVar { name: _, ntype, offset: _ } => array_element_size(&ntype)?,
                    _ => return Err(Box::new(ParseError { err: format!("lhs is not pointer {:?} ", lhs) }))
                }
            }
        }
        _ => return Err(Box::new(ParseError{err: format!("lhs is not pointer {:?} ", lhs)}))
    };

    let obj_size_node = node_number(obj_size as i64)?;
    let node_mul = new_node(BinOpKind::Mult, rhs.clone(), obj_size_node);
    let mut binop= match array_type(lhs)? {
        Some(ntype) => new_node_ntype(BinOpKind::Add, lhs.clone(),  node_mul, ntype),
        _ => new_node(BinOpKind::Add, lhs.clone(),  node_mul)
    };
    return Ok(binop);
}

// ASTがArrayなら型を返す。
// Arrayでない時はNoneを返す。
fn array_type(ast :&Ast) -> Result<Option<ast::NodeType>, Box<dyn Error>> {
    match ast {
        AstKind::LocalVar {name: _, ntype, offset: _ } => {
            if ntype.kind == NodeTypeKind::Array {
                Ok(Some(ntype.clone()))
            } else {
                Ok(None)
            }
        }
        _ => Ok(None),
    }
}

// ASTがArrayなら1次元減らした型を返す。
// Arrayでない時はNoneを返す。
fn array_deref(ast :&Ast) -> Result<Option<ast::NodeType>, Box<dyn Error>> {
    match ast {
        AstKind::LocalVar {name: _, ntype, offset: _ } => {
            if ntype.kind == NodeTypeKind::Array {
                 match ntype.base.clone() {
                     Some(b) => Ok(Some(*b)),
                     _ => return Err(Box::new(ParseError { err: format!("Array has no base {:?}", ast) })),
                 }
             } else {
                 Ok(None)
            }
        }
        _ => Ok(None),
    }
}

fn array_element_size(ntype: &NodeType) -> Result<usize, Box<dyn Error>> {
    let mut esize = ntype.size;
    let mut ntype_base = ntype.clone();
    loop {
        ntype_base =  match ntype_base.base {
            Some(b) => *b,
            None => return Ok(esize),
        };
        esize = ntype_base.size;
    }
}

fn new_sub(l: &mut Ast, r: &mut Ast) -> Result<Ast, Box<dyn Error>>  {

    add_type(l)?;
    add_type(r)?;

    if is_integer(l) && is_integer(r) {
        let binop  = BinOp::new(BinOpKind::Sub, Box::new(l.clone()),  Box::new(r.clone()));
        return Ok(AstKind::BinOp(binop));
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

    let obj_size = match &lhs {
        AstKind::LocalVar {name: _, ntype, offset: _ } => {
            ntype.size
        },
        AstKind::BinOp(binop) => {
            binop.ntype.size
        }
        AstKind::UniOp(uniop) => {
            uniop.ntype.size
        }
        _ => return Err(Box::new(ParseError{err: format!("lhs is not pointer {:?} ", lhs)}))
    };
    let obj_size_node = node_number(obj_size as i64)?;

    // ptr - ptr, which returns how many elements are between the two.
    if is_pointer(lhs) && is_pointer(rhs) {
        let sub_op = new_node_int(BinOpKind::Sub, lhs.clone(), rhs.clone());
        let binop = new_node_int(BinOpKind::Div, sub_op, obj_size_node);
        return Ok(binop);
    }

    //ltype2:pointer
    //rtype2:num

    let node_mul = new_node(BinOpKind::Mult, rhs.clone(), obj_size_node);
    let binop  = new_node(BinOpKind::Sub, lhs.clone(),  node_mul);
    return Ok(binop);
}

fn node_number(n: i64) -> Result<Ast, Box<dyn Error>> {
    Ok(AstKind::Num{n, ntype: NodeType{kind: NodeTypeKind::Int, size:8, len:0, base: None }})
}

fn node_variable(name: String, ntype:NodeType, offset :i64) ->  Result<Ast, Box<dyn Error>> {
    Ok(AstKind::LocalVar{name, ntype, offset})
}

fn skip(tokenizer: &mut Tokenizer, token :TType) -> Result<(), Box<dyn Error>> {
    let token_next = tokenizer.get();
    if token_next.ttype != token {
        return Err(Box::new(ParseError{err: format!("SKIP:token {:?} must be {:?} ", token_next, token)}));
    }
    tokenizer.consume();
    Ok(())
}
