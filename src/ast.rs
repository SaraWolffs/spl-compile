use std::ops::Deref;

#[derive(Copy,Clone)]
pub(crate) struct Loc{
    pub line: u32,
    pub col: u16,
    pub len: u16,
}

impl Loc {
    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 0;
        self.len = 0;
    }

    pub fn advance(&mut self) {
        self.col += self.len;
        self.len = 0;
    }
}

pub(crate) type Located<T> = (T,Loc);

/*
impl<T> Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
*/

pub(crate) type SPL = Vec<Decl>;

pub(crate) enum Decl {
    Var(Option<Id>,Id,Exp),
    Fun(Id,Vec<Id>,Option<FunType>,Vec<Decl>,Vec<Stmt>)
}

pub(crate) enum Exp {
    Var(Id,Vec<Selector>),
    Call(Id,Vec<Exp>),
    Lit(LitVal),
    Tuple(Vec<Exp>),
    BinOp(Op,Box<Exp>,Box<Exp>),
    UnOp(Op,Box<Exp>)
}

pub(crate) enum Stmt {
    ITE(Exp,Vec<Stmt>,Vec<Stmt>),
    While(Exp,Vec<Stmt>),
    Assign(Id,Exp),
    Call(Id,Vec<Exp>),
    Ret(Option<Exp>)
}

pub(crate) type FunType = (Vec<Type>,Type);

pub(crate) enum Type {
    Lit(BType),
    Typename(Id),
    Tuple(Vec<Type>),
    List(Box<Type>)
}


/** Terminal symbols/tokens **/

pub(crate) type Id = Located<u32>; //TODO: replace String with u32 tracking number?

pub(crate) enum Selector {
    Hd, Tl, Fst, Snd
}

pub(crate) enum BType {
    IntT,
    BoolT,
    CharT,
    UnitT,
}

pub(crate) enum LitVal {
    Int(i64),
    Char(char),
    Bool(bool),
    Nil,
}

pub(crate) enum Op {
    And, Or, Not,
    Lt, Leq, Gt, Geq, Eq, Neq, 
    Plus, Minus, Mul, Div,
    Cons,
}
