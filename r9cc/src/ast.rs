use std::io::Write;
use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

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
}
pub type BinOp = Annot<BinOpKind>;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(i64),
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
}

pub type Ast = Annot<AstKind>;

pub fn write_dot(ast: &Ast, path :&Path) -> Result<(),  std::io::Error> {
    let mut file = File::create(path)?;

    writeln!(file, "digraph G {{")?;

    write_node(ast, &mut file, 0)?;

    writeln!(file, "}}")?;
    Ok(())
}

pub fn node_name(cnt :u64) -> String {
    format!("node{}", cnt)
}

pub fn write_node(node :&Ast, file: &mut File, cnt: u64) -> Result<u64, std::io::Error> {
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
                BinOpKind::Add => "\"+\"",
                BinOpKind::Sub => "\"-\"",
                BinOpKind::Mult => "\"*\"",
                BinOpKind::Div => "\"/\"",
                _ => "write node Unknown OP",
            };

            let left_node_name = node_name(cnt+1);
            let right_node_name = node_name(left_cnt + 1);

            writeln!(file, "{}", format!("{}[label={}]", self_node_name, op_str))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, left_node_name))?;
            writeln!(file, "{}", format!("{} -> {}", self_node_name, right_node_name))?;

            return Ok(right_cnt)
        },
    }
}