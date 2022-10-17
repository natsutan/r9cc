use std::io::Write;
use std::path::Path;
use std::fs::File;
//use std::io::prelude::*;


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
pub type BinOp = Annot<BinOpKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    NdExprStmt,
    NdReturn,
}
pub type UniOp = Annot<UniOpKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BlockKind {
    NdBlock,
}
pub type Block = Annot<BlockKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IfKind {
    NdIf,
}
pub type If_ = Annot<IfKind>;



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(i64),
    LocalVar{name: String, offset: i64 },
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
    UniOp { op: UniOp, l: Box<Ast>},
    Block { body: Vec<Box<Ast>>},
    If_ {cond: Box<Ast>, then: Box<Ast>, els : Box<Ast>},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariable {
    pub name: String,
    pub offset: i64,
}



pub type Ast = Annot<AstKind>;
pub type Program = Vec<Ast>;
pub type Frame = Vec<LocalVariable>;

pub fn write_dot(program: &Program, path :&Path) -> Result<(),  std::io::Error> {
    let mut file = File::create(path)?;

    writeln!(file, "digraph G {{")?;

    let mut cnt = 0;
    for node in program.iter() {
        cnt = write_node(&node, &mut file, cnt)?;
        cnt += 1;
    }


    writeln!(file, "}}")?;
    Ok(())
}

pub fn node_name(cnt :u64) -> String {
    format!("node{}", cnt)
}

fn write_node(node :&Ast, file: &mut File, cnt: u64) -> Result<u64, std::io::Error> {
    let self_node_name = node_name(cnt);

    match &node.value {
        AstKind::Num(n) => {
            writeln!(file, "{}", format!("{}[label={}]", self_node_name, n))?;
            return Ok(cnt)
        },
        AstKind::BinOp{op, l, r} => {
            let left_cnt = write_node(&l, file, cnt + 1)?;
            let right_cnt = write_node(&r, file, left_cnt + 1)?;
            let op_str = match op.value {
                BinOpKind::Add  => "\"+\"",
                BinOpKind::Sub  => "\"-\"",
                BinOpKind::Mult => "\"*\"",
                BinOpKind::Div  => "\"/\"",
                BinOpKind::Eq   => "\"==\"",
                BinOpKind::Ne   => "\"!=\"",
                BinOpKind::Lt   => "\"<\"",
                BinOpKind::Le   => "\"<=\"",
                BinOpKind::Assign => "\"=\"",
            };

            let left_node_name = node_name(cnt+1);
            let right_node_name = node_name(left_cnt + 1);

            writeln!(file, "{}", format!("{}[label={}]", self_node_name, op_str))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, left_node_name))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, right_node_name))?;

            return Ok(right_cnt)
        },
        AstKind::LocalVar{name, offset} => {
            writeln!(file, "{}", format!("{}[label=\"{}\n{}\"]", self_node_name, name, offset))?;
            return Ok(cnt)
        }
        AstKind::UniOp {op, l} => {
            let left_cnt = write_node(&l, file, cnt + 1)?;
            let left_node_name = node_name(cnt + 1);
            let op_str = match op.value {
                UniOpKind::NdExprStmt => "\"EXPR_STMT\"",
                UniOpKind::NdReturn => "\"RETURN\"",
            };

            writeln!(file, "{}", format!("{}[label={}]", self_node_name, op_str))?;
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
    }
}