#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Span {
    pub startline: u32,
    pub endline: u32,
    pub startcol: u16,
    pub endcol: u16,
}

impl From<crate::parser::Loc> for Span {
    fn from(loc: crate::parser::Loc) -> Self {
        Span {
            startline: loc.line,
            endline: loc.line,
            startcol: loc.col,
            endcol: loc.col + loc.len,
        }
    }
}

impl Span {
    pub(crate) fn new(startline: u32, endline: u32, startcol: u16, endcol: u16) -> Self {
        Self {
            startline,
            endline,
            startcol,
            endcol,
        }
    }
    pub(crate) fn hull(lhs: Self, rhs: Self) -> Self {
        use core::cmp::max;
        use core::cmp::min;
        use std::cmp::Ordering::*;
        Span {
            startline: min(lhs.startline, rhs.startline),
            endline: max(lhs.endline, rhs.endline),
            startcol: match lhs.startline.cmp(&rhs.startline) {
                Less => lhs.startcol,
                Equal => min(lhs.startcol, rhs.startcol),
                Greater => rhs.startcol,
            },
            endcol: match lhs.endline.cmp(&rhs.endline) {
                Greater => lhs.endcol,
                Equal => max(lhs.endcol, rhs.endcol),
                Less => rhs.endcol,
            },
        }
    }
}

pub type Spanned<T> = (T, Option<Span>);

pub type SPL = Vec<Decl>;

pub type Decl = Spanned<BareDecl>;
#[derive(Debug, PartialEq)]
pub enum BareDecl {
    Var(Option<Type>, Id, Exp),
    Fun(Id, Vec<Id>, Option<FunType>, Vec<Decl>, Vec<Stmt>),
}

pub type Exp = Spanned<Typed<BareExp>>;
#[derive(Debug, PartialEq)]
pub enum BareExp {
    Var(Id, Vec<Selector>),
    Call(Id, Vec<Exp>),
    Lit(LitVal),
    Tuple(Vec<Exp>),
    BinOp(Op, Box<Exp>, Box<Exp>),
    UnOp(Op, Box<Exp>),
}

pub type Stmt = Spanned<BareStmt>;
#[derive(Debug, PartialEq)]
pub enum BareStmt {
    ITE(Exp, Vec<Stmt>, Vec<Stmt>),
    While(Exp, Vec<Stmt>),
    Assign(Id, Exp),
    Call(Id, Vec<Exp>),
    Ret(Option<Exp>),
}

pub type FunType = Spanned<BareFunType>;
pub type BareFunType = (Vec<Type>, Type);

pub type Type = Spanned<BareType>;
#[derive(Debug, PartialEq)]
pub enum BareType {
    Lit(BType),
    Typename(Id),
    Tuple(Vec<Type>),
    List(Box<Type>),
}

pub type Typed<T> = (T, Option<Type>);

/** Terminal symbols/tokens **/
pub type Id = Spanned<BareId>;

pub type BareId = u32;

pub type Selector = Spanned<BareSelector>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BareSelector {
    Hd,
    Tl,
    Fst,
    Snd,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BType {
    IntT,
    BoolT,
    CharT,
    UnitT,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LitVal {
    Int(i64),
    Char(char),
    Bool(bool),
    Nil,
}

pub type Op = Spanned<BareOp>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BareOp {
    And,
    Or,
    Not,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    Plus,
    Minus,
    Mul,
    Div,
    Neg,
    Cons,
}

type Priority = u8;
impl BareOp {
    pub fn prio(self) -> Priority {
        use BareOp::*;
        match self {
            Cons => 15,
            Or => 20,
            And => 30,
            Eq => 40,
            Neq => 40,
            Not => 50,
            Lt => 60,
            Leq => 60,
            Gt => 60,
            Geq => 60,
            Plus => 70,
            Minus => 70,
            Neg => 85,
            // Exception: since -(a*b) == (-a)*b, we choose the one that facilitates
            // the greater amount of const folding, rather than the equivalent choice
            // which intuition suggests.
            Mul => 80,
            Div => 80,
        }
    }

    pub fn is_unary(self) -> bool {
        use BareOp::*;
        match self {
            Neg | Not => true,
            _ => false,
        }
    }

    pub fn right_precedes(self, other: Self) -> bool {
        self.is_unary()
            || self.prio() > other.prio()
            || (self.prio() == other.prio() && (self.prio() % 2 == 1))
    }

    #[allow(dead_code)]
    pub fn left_precedes(self, other: Self) -> bool {
        self.prio() > other.prio() || (self.prio() == other.prio() && (self.prio() % 2 == 0))
    }
}
