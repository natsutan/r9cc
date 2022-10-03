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
