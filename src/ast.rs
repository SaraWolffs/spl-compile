#[derive(Copy,Clone,Debug,PartialEq)]
pub struct Span {
    pub startline:u32,
    pub endline:u32,
    pub startcol:u16,
    pub endcol:u16,
}

pub type Spanned<T> = (T,Option<Span>);

pub type SPL = Vec<Decl>;


pub type Decl = Spanned<BareDecl>;
pub enum BareDecl {
    Var(Option<Type>,Id,Exp),
    Fun(Id,Vec<Id>,Option<FunType>,Vec<Decl>,Vec<Stmt>)
}

pub type Exp = Spanned<Typed<BareExp>>;
pub enum BareExp {
    Var(Id,Vec<Selector>),
    Call(Id,Vec<Exp>),
    Lit(LitVal),
    Tuple(Vec<Exp>),
    BinOp(Op,Box<Exp>,Box<Exp>),
    UnOp(Op,Box<Exp>),
}

pub type Stmt = Spanned<BareStmt>;
pub enum BareStmt {
    ITE(Exp,Vec<Stmt>,Vec<Stmt>),
    While(Exp,Vec<Stmt>),
    Assign(Id,Exp),
    Call(Id,Vec<Exp>),
    Ret(Option<Exp>),
}

pub type FunType = Spanned<BareFunType>;
pub type BareFunType = (Vec<Type>,Type);

pub type Type = Spanned<BareType>;
pub enum BareType {
    Lit(BType),
    Typename(Id),
    Tuple(Vec<Type>),
    List(Box<Type>),
}

pub type Typed<T> = (T,Option<Type>);


/** Terminal symbols/tokens **/
pub type Id = Spanned<BareId>;

pub type BareId = u32; 

pub type Selector = Spanned<BareSelector>;

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum BareSelector {
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

pub type Op = Spanned<BareOp>;

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum BareOp {
    And, Or, Not,
    Lt, Leq, Gt, Geq, Eq, Neq, 
    Plus, Minus, Mul, Div, Neg,
    Cons,
}
