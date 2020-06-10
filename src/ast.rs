#[derive(Copy,Clone,Debug,PartialEq)]
pub struct Loc{
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

pub type Located<T> = (T,Loc);

pub type SPL = Vec<Decl>;

pub enum Decl {
    Var(Option<Type>,Id,Exp),
    Fun(Id,Vec<Id>,Option<FunType>,Vec<Decl>,Vec<Stmt>)
}

pub enum Exp {
    Var(Id,Vec<Selector>),
    Call(Id,Vec<Exp>),
    Lit(LitVal),
    Tuple(Vec<Exp>),
    BinOp(Op,Box<Exp>,Box<Exp>),
    UnOp(Op,Box<Exp>)
}

pub enum Stmt {
    ITE(Exp,Vec<Stmt>,Vec<Stmt>),
    While(Exp,Vec<Stmt>),
    Assign(Id,Exp),
    Call(Id,Vec<Exp>),
    Ret(Option<Exp>)
}

pub type FunType = (Vec<Type>,Type);

pub enum Type {
    Lit(BType),
    Typename(Id),
    Tuple(Vec<Type>),
    List(Box<Type>)
}


/** Terminal symbols/tokens **/

pub type Id = Located<u32>; 

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Selector {
    Hd, Tl, Fst, Snd
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum BType {
    IntT,
    BoolT,
    CharT,
    UnitT,
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum LitVal {
    Int(i64),
    Char(char),
    Bool(bool),
    Nil,
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Op {
    And, Or, Not,
    Lt, Leq, Gt, Geq, Eq, Neq, 
    Plus, Minus, Mul, Div, Neg,
    Cons,
}
