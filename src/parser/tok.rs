use crate::ast::{BType, BareId, BareOp, BareSelector, LitVal};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Loc {
    pub line: u32,
    pub col: u16,
    pub len: u16,
} // TODO: unify this with span to undo technical debt from premature optimization.

impl Loc {
    // TODO: guard against overflow
    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 0;
        self.len = 0;
    }

    pub fn advance(&mut self) {
        self.col += self.len;
        self.len = 0;
    }

    pub fn step(&mut self) {
        self.len += 1;
    }
}

pub type Located<T> = (T, Loc);

#[derive(Copy, Clone, Debug, PartialEq)]
pub(super) enum Misc {
    Var,
    If,
    Else,
    While,
    Return,
    Semicolon,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BrackOpen,
    BrackClose,
    Comma,
    Dot,
    Arrow,
    Assign,
    TypeColon,
}

#[must_use]
#[derive(Copy, Clone, Debug, PartialEq)]
pub(super) enum Token {
    IdTok(BareId),
    Selector(BareSelector),
    TypeTok(BType),
    Lit(LitVal),
    Op(BareOp),
    Marker(Misc),
}

pub(super) type LocTok = Located<Token>;

pub(super) trait TokAble {
    fn to_tok(self) -> Token;
    fn to_ltok(self, l: Loc) -> LocTok
    where
        Self: std::marker::Sized,
    {
        (self.to_tok(), l)
    }
}

impl TokAble for u32 {
    fn to_tok(self) -> Token {
        Token::IdTok(BareId(self))
    }
}

impl TokAble for BareSelector {
    fn to_tok(self) -> Token {
        Token::Selector(self)
    }
}

impl TokAble for BType {
    fn to_tok(self) -> Token {
        Token::TypeTok(self)
    }
}

impl TokAble for LitVal {
    fn to_tok(self) -> Token {
        Token::Lit(self)
    }
}

impl TokAble for BareOp {
    fn to_tok(self) -> Token {
        Token::Op(self)
    }
}

impl TokAble for Misc {
    fn to_tok(self) -> Token {
        Token::Marker(self)
    }
}
