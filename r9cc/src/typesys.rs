
use crate::ast::*;

pub fn is_integer(node :&Ast) -> bool {
    match &node.value {
        AstKind::Num{n:_, ntype} => {
            ntype.kind == NodeTypeKind::Int
        },
        _ => false
    }
}

fn add_type_for_body(body : &mut Vec<Box<Ast>>) {
    for i in 0..body.len() {
        add_type(&mut *body[i]);
    }
}

pub fn add_type(node :&mut Ast) {
    match &mut node.value {
        AstKind::Num{n:_, ntype:_} => (),
        AstKind::LocalVar{name: _, ntype: _, offset: _ } => (),
        AstKind::Block { body} => {
             add_type_for_body(body);
        },
        AstKind::If_ {cond, then , els } => {
            add_type(cond);
            add_type(then);
            add_type(els);
        },
        AstKind::For {init, cond, inc, then} => {
            add_type(init);
            add_type(cond);
            add_type(inc);
            add_type(then);
        },
        // BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
        // UniOp { op: UniOp, l: Box<Ast>},
        _ => {}
    }

    {}
}