use std::error::Error;
use std::fmt;
use crate::ast::*;

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
        AstKind::LocalVar {name: _, ntype, offset: _ }=> {
            ntype.kind == NodeTypeKind::Int
        },
        AstKind::BinOp(binop) => {
            binop.ntype.kind == NodeTypeKind::Int
        }
        AstKind::UniOp(uniop) => {
            uniop.ntype.kind == NodeTypeKind::Int
        }
        AstKind::FunCall {funcname:_, ntype, args:_} => {
            ntype.kind == NodeTypeKind::Int
        }
        _ => false
    }
}

pub fn is_pointer(node :&Ast) -> bool {
    match &node.value {
        AstKind::LocalVar {name: _, ntype, offset: _ }=> {
            ntype.kind == NodeTypeKind::Ptr
        },
        AstKind::BinOp(binop) => {
            binop.ntype.kind == NodeTypeKind::Ptr
        }
        AstKind::UniOp(uniop) => {
            uniop.ntype.kind == NodeTypeKind::Ptr
        }
        _ => false
    }
}

fn add_type_for_body(body : &mut Vec<Box<Ast>>) -> Result<Option<NodeType>, Box<dyn Error>> {
    for i in 0..body.len() {
        add_type(&mut *body[i])?;
    }
    Ok(None)
}

fn new_pointer(dst :&NodeType) -> Result<NodeType, Box<dyn Error>> {
    let dst_c = Box::new(dst.clone());
    Ok(NodeType{kind: NodeTypeKind::Ptr, size:1, base: Some(dst_c)})
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
            add_type_for_body(body)
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
                BinOpKind::Eq | BinOpKind::Ne | BinOpKind::Lt | BinOpKind::Le => {
                    let ntype = NodeType{kind: NodeTypeKind::Int, size:1, base: None};
                    binop.set_node_type(ntype.clone());
                    return Ok(Some(ntype));
                }
            }
        }
        AstKind::UniOp(uniop) => {
            let ltype = add_type(&mut uniop.l)?;

            match uniop.op {
                UniOpKind::Addr => {
                    let dst_type = match ltype {
                        Some(t) => t,
                        None => {
                            return Err(Box::new(TypeError { err: format!("Pointer has no type {:?}", ltype) }));
                        }
                    };

                    let ntype = new_pointer(&dst_type)?;
                    uniop.set_node_type(ntype.clone());
                    Ok(Some(ntype))
                },
                UniOpKind::Deref => {
                    let dst_type = match &ltype {
                        Some(t) => t,
                        None => {
                            return Err(Box::new(TypeError { err: format!("invalid pointer dereference {:?}", ltype) }));
                        }
                    };
                    match dst_type.kind {
                        NodeTypeKind::Ptr | NodeTypeKind::Array => {
                            let base_type = match &dst_type.base {
                                Some(b) => b,
                                None => {
                                    return Err(Box::new(TypeError { err: format!("invalid pointer dereference {:?}", ltype) }));
                                }
                            };
                            uniop.set_node_type(*base_type.clone());
                            let btype = *base_type.clone();
                            return Ok(Some(btype));

                        }
                        _ => return Err(Box::new(TypeError { err: format!("invalid pointer dereference {:?}", ltype) })),
                    }
                },
                _ => Ok(None)
            }
        },
        AstKind::FunCall {funcname:_, ntype, args:_} => {
            return Ok(Some(ntype.clone()));
        }
        _ => Ok(None)
    }
}