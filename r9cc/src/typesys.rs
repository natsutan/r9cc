use std::error::Error;
use std::fmt;
use crate::ast::*;
use crate::generator::CodeGenError;

#[derive(Debug)]
pub struct TypeError {
    pub err: String,
}

impl Error for TypeError {}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type: error :{}", self.err)
    }
}


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

pub fn add_type(node :&mut Ast) -> Result<Option<NodeType>, Box<dyn Error>> {
    match &mut node.value {
        AstKind::Num{n:_, ntype} => {
            Ok(Some(ntype.clone()))
        },
        AstKind::LocalVar{name: _, ntype, offset: _ } => {
            Ok(Some(ntype.clone()))
        },
        AstKind::Block { body} => {
            add_type_for_body(body);
            Ok(None)
        },
        AstKind::If_ {cond, then , els } => {
            add_type(cond)?;
            add_type(then)?;
            add_type(els)?;
            Ok(None)
        },
        AstKind::For {init, cond, inc, then} => {
            add_type(init)?;
            add_type(cond)?;
            add_type(inc)?;
            add_type(then)?;
            Ok(None)
        },
        AstKind::BinOp(binop)  => {
            if binop.ntype.kind != NodeTypeKind::UnFixed {
                let ntype = binop.ntype.clone();
                return Ok(Some(ntype));
            }
            let ltype = add_type(&mut binop.l)?;
            add_type(&mut binop.r)?;

            match binop.op {
                BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mult | BinOpKind::Div | BinOpKind::Assign => {
                    match ltype {
                        Some(t) => {
                            binop.set_node_type(t.clone());
                            return Ok(Some(t.clone()));
                        }
                        None => {
                            return Err(Box::new(TypeError{err: format!("Binop node l has no type {:?}", binop)}));
                        }
                    }
                }
                _ => return Ok(None)
            }
        }
        //     // Add,
        //     // Sub,
        //     // Mult,
        //     // Div,
        //     // Eq,  // ==
        //     // Ne,  // !=
        //     // Lt,  // <
        //     // Le,  // >
        //     // Assign,
        //     None
        // },
        // // UniOp { op: UniOp, l: Box<Ast>},
        _ => Ok(None)
    }
}