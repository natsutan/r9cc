use std::fmt;
use std::io::Write;
use std::path::Path;
use std::fs::File;
use crate::tokenizer::Token;



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub value :T,
    pub loc: Loc
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mult,
    Div,
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // >
    Assign,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinOp {
    pub op: BinOpKind,
    pub ntype: NodeType,
    pub l: Box<Ast>,
    pub r: Box<Ast>,
    pub loc: Loc
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniOp {
    pub op: UniOpKind,
    pub ntype: NodeType,
    pub l: Box<Ast>,
    pub loc: Loc
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    NdExprStmt,
    NdReturn,
    Addr,
    Deref,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeTypeKind {
    Int,
    Ptr,
    Func,
    Array,
    UnFixed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodeType {
    pub kind :NodeTypeKind,
    pub size :usize,
    pub base :Option<Box<NodeType>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num{n: i64, ntype :NodeType},
    LocalVar {name: String, ntype: NodeType, offset: i64 },
    BinOp(BinOp),
    UniOp(UniOp),
    Block { body: Vec<Box<Ast>>},
    If_ {cond: Box<Ast>, then: Box<Ast>, els : Box<Ast>},
    For {init: Box<Ast>, cond: Box<Ast>, inc: Box<Ast>, then: Box<Ast>},
    FunCall{funcname: String, args: Vec<Box<Ast>>, ntype: NodeType},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariable {
    pub name: String,
    pub ntype: NodeType,
    pub offset: i64,
}

pub struct Function {
    pub name: String,
    pub params: Vec<LocalVariable>,
    pub locals: Vec<LocalVariable>,
    pub stack_size: u64,
    pub body: Ast,
    pub return_type: NodeType,
}

impl BinOp {
    pub fn new(op:BinOpKind, l: Box<Ast>, r: Box<Ast>, token: &Token) -> BinOp {
        let loc = Loc{ 0: token.line_num, 1:token.pos };
        let ntype = NodeType{kind: NodeTypeKind::UnFixed, size:1, base: None};
        BinOp { op, ntype, l, r, loc }
    }

    pub fn set_node_type(&mut self, node_type :NodeType) {
        self.ntype = node_type.clone();
    }
}

impl UniOp {
    pub fn new(op:UniOpKind, l: Box<Ast>, token: &Token) -> UniOp {
        let loc = Loc{ 0: token.line_num, 1:token.pos };
        let ntype = NodeType{kind: NodeTypeKind::UnFixed, size:1, base: None};
        UniOp { op, ntype, l, loc }
    }

    pub fn set_node_type(&mut self, node_type :NodeType) {
        self.ntype = node_type.clone();
    }
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self.kind {
            NodeTypeKind::Int => "int",
            NodeTypeKind::Ptr => "ptr",
            NodeTypeKind::Array => "arr",
            NodeTypeKind::Func => "fun",
            NodeTypeKind::UnFixed => "",
        };
        write!(f, "{}", s)
    }
}


pub type Ast = Annot<AstKind>;
pub type Program = Vec<Function>;
pub type Frame = Vec<LocalVariable>;

pub fn write_dot(program: &Program, path :&Path) -> Result<(),  std::io::Error> {
    let mut file = File::create(path)?;

    writeln!(file, "digraph G {{")?;

    let mut cnt = 0;
    for node in program.iter() {
        cnt = write_function(&node, &mut file, cnt)?;
        cnt += 1;
    }


    writeln!(file, "}}")?;
    Ok(())
}

fn write_function(func :&Function, file: &mut File, cnt: u64) -> Result<u64, std::io::Error> {
    let self_node_name = node_name(cnt);
    writeln!(file, "{}", format!("{}[label={}, shape=\"box\"]", self_node_name, func.name))?;
    let body_node_name = node_name(cnt + 1);

    writeln!(file, "{}", format!("{} -> {}", self_node_name, body_node_name))?;
    let next_cnt = write_node(&func.body, file, cnt+1)?;

    return Ok(next_cnt)

}

pub fn node_name(cnt :u64) -> String {
    format!("node{}", cnt)
}

fn write_node(node :&Ast, file: &mut File, cnt: u64) -> Result<u64, std::io::Error> {
    let self_node_name = node_name(cnt);

    match &node.value {
        AstKind::Num{n, ntype:_n} => {
            writeln!(file, "{}", format!("{}[label={}]", self_node_name, n))?;
            return Ok(cnt)
        },
        AstKind::BinOp(binop) => {
            let left_cnt = write_node(&binop.l, file, cnt + 1)?;
            let right_cnt = write_node(&binop.r, file, left_cnt + 1)?;
            let op_str = match binop.op {
                BinOpKind::Add  => "+",
                BinOpKind::Sub  => "-",
                BinOpKind::Mult => "*",
                BinOpKind::Div  => "/",
                BinOpKind::Eq   => "==",
                BinOpKind::Ne   => "!=",
                BinOpKind::Lt   => "<",
                BinOpKind::Le   => "<=",
                BinOpKind::Assign => "=",
            };

            let left_node_name = node_name(cnt+1);
            let right_node_name = node_name(left_cnt + 1);

            writeln!(file, "{}", format!("{}[label=\"{}\n{}\"]", self_node_name, op_str, binop.ntype))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, left_node_name))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, right_node_name))?;

            return Ok(right_cnt)
        },
        AstKind::LocalVar{name, ntype, offset} => {
            writeln!(file, "{}", format!("{}[label=\"{}\n{}(size {})\"]", self_node_name, name, ntype, ntype.size))?;
            return Ok(cnt)
        }
        AstKind::UniOp(uniop)=> {
            let left_cnt = write_node(&uniop.l, file, cnt + 1)?;
            let left_node_name = node_name(cnt + 1);
            let op_str = match uniop.op {
                UniOpKind::NdExprStmt => "EXPR_STMT",
                UniOpKind::NdReturn => "RETURN",
                UniOpKind::Addr => "&",
                UniOpKind::Deref => "*p",
            };

            writeln!(file, "{}", format!("{}[label=\"{}\n{}\"]", self_node_name, op_str, uniop.ntype))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, left_node_name))?;
            return Ok(left_cnt)
        }
        AstKind::Block { body } => {
            writeln!(file, "{}", format!("{}[label=BLOCK]", self_node_name))?;
            let mut next_cnt = cnt;
            for node in body.iter() {
                let next_node_name = node_name(next_cnt+1);
                writeln!(file, "{}", format!("{} -> {}", self_node_name, next_node_name))?;
                next_cnt = write_node(node, file, next_cnt+1)?;
            }

            return Ok(next_cnt)
        }
        AstKind::If_ {cond, then, els} => {
            writeln!(file, "{}", format!("{}[label=IF]", self_node_name))?;
            let mut next_cnt = cnt;
            let cond_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, cond_node_name))?;
            next_cnt = write_node(cond, file ,next_cnt + 1)?;
            let then_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, then_node_name))?;
            next_cnt = write_node(then, file ,next_cnt + 1)?;
            let else_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, else_node_name))?;
            next_cnt = write_node(els, file ,next_cnt + 1)?;

            return Ok(next_cnt)
        }
        AstKind::For {init, cond, inc, then} => {
            writeln!(file, "{}", format!("{}[label=FOR]", self_node_name))?;
            let mut next_cnt = cnt;

            let init_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, init_node_name))?;
            next_cnt = write_node(init, file ,next_cnt + 1)?;

            let cond_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, cond_node_name))?;
            next_cnt = write_node(cond, file ,next_cnt + 1)?;

            let inc_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, inc_node_name))?;
            next_cnt = write_node(inc, file ,next_cnt + 1)?;

            let then_node_name = node_name(next_cnt+1);
            writeln!(file, "{}", format!("{} -> {}", self_node_name, then_node_name))?;
            next_cnt = write_node(then, file ,next_cnt + 1)?;

            return Ok(next_cnt)
        }
        AstKind::FunCall {funcname, args: _, ntype: _} => {
            writeln!(file, "{}", format!("{}[label=\"CALL\n{}\"]", self_node_name, funcname))?;
            return Ok(cnt)
        }

    }
}