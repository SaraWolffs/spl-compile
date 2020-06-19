
use crate::ast::{Selector,BType,LitVal,Op,Loc,Located};
#[derive(Copy,Clone,Debug,PartialEq)]
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

#[derive(Copy,Clone,Debug,PartialEq)]
pub(super) enum Token {
    IdTok(u32),
    Selector(Selector),
    TypeTok(BType),
    Lit(LitVal),
    Op(Op),
    Marker(Misc),
}

pub(super) type LocTok = Located<Token>;

pub(super) trait TokAble  {
    fn to_tok(self) -> Token;
    fn to_ltok(self,l:Loc) -> LocTok 
        where Self: std::marker::Sized {
        (self.to_tok(),l)
    }
}

impl TokAble for u32 {
    fn to_tok(self) -> Token {
        Token::IdTok(self)
    }
}

impl TokAble for Selector {
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

impl TokAble for Op {
    fn to_tok(self) -> Token {
        Token::Op(self)
    }
}

impl TokAble for Misc {
    fn to_tok(self) -> Token {
        Token::Marker(self)
    }
}

#[macro_export]
macro_rules! tokpat_to_str {
    (Marker(Var)) => { "'var'" };
    (TypeTok(IntT)) => { "'Int'" };
    (TypeTok(BoolT)) => { "'Bool'" };
    (TypeTok(CharT)) => { "'Char'" };
    (TypeTok(UnitT)) => { "'Void'" };
    (TypeTok($t:pat)) => { "base type" };
    (Marker(BracOpen)) => { "'{'" };
    (Marker(ParenOpen)) => { "'('" };

}

mod test {
    use super::*;
    use super::Token::*;
    #[test]
    fn typetokpat_to_str() {
        assert_eq!(tokpat_to_str!(TypeTok(_)), "base type");
        assert_eq!(tokpat_to_str!(TypeTok(CharT)), "'Char'");
    }
}
