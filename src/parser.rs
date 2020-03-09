
mod lex {
    use crate::ast::{Id,Selector,BType,LitVal,Op,Loc,Located};
    use core::iter::{Peekable};
    use core::str::Chars;
    //use core::slice::{Iter};

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
        LineComment,
        CommentOpen,
        CommentClose,
        Whitespace,
        TokFail(String),
    }

    pub(super) enum Token {
        Id(Id),
        Selector(Located<Selector>),
        Type(Located<BType>),
        Lit(Located<LitVal>),
        Op(Located<Op>),
        Marker(Located<Misc>),
    }
    pub(super) type TokStream = Vec<Token>;

    trait TokAble  {
        fn to_tok(self,l:Loc) -> Token;
    }
    
    impl TokAble for String {
        fn to_tok(self,l:Loc) -> Token {
            Token::Id((self,l))
        }
    }
    
    impl TokAble for Selector {
        fn to_tok(self,l:Loc) -> Token {
            Token::Selector((self,l))
        }
    }
    use crate::ast::Selector::*;

    impl TokAble for BType {
        fn to_tok(self,l:Loc) -> Token {
            Token::Type((self,l))
        }
    }
    use crate::ast::BType::*;

    impl TokAble for LitVal {
        fn to_tok(self,l:Loc) -> Token {
            Token::Lit((self,l))
        }
    }
    use crate::ast::LitVal::*;

    impl TokAble for Op {
        fn to_tok(self,l:Loc) -> Token {
            Token::Op((self,l))
        }
    }
    use crate::ast::Op::*;
    
    impl TokAble for Misc {
        fn to_tok(self,l:Loc) -> Token {
            Token::Marker((self,l))
        }
    }
    use Misc::*;

    fn step<T>( loc: &mut Loc, iter: &mut dyn Iterator<Item = T>,
    ) -> Option<T> {
        loc.len += 1;
        iter.next()
    }

    fn step_or<T>(loc: &mut Loc, iter: &mut dyn Iterator<Item = T>, errmsg: &str,
    ) -> Result<T,(String,Loc)> {
        loc.len += 1;
        iter.next().ok_or((errmsg.to_string(),loc.to_owned()))
    }
    
    pub(super) fn lex(input : &str) -> TokStream {
        use Token::*;
        let mut tokens : Vec<Token> = Vec::with_capacity(input.len()); // Absolute worst case
        let mut loclife = Loc{ line: 0, col: 0, len: 0 };
        let loc = &mut loclife;
        let mut iterlife = input.char_indices().peekable();
        let iter = &mut iterlife;
        loop {
            let tok = match step(loc,lin) {
                Some('+') => Plus.to_tok(*loc),
                Some(';') => Semicolon.to_tok(*loc),
                Some('(') => ParenOpen.to_tok(*loc),
                Some(')') => ParenClose.to_tok(*loc),
                Some('{') => BraceOpen.to_tok(*loc),
                Some('}') => BraceClose.to_tok(*loc),
                Some(']') => BrackClose.to_tok(*loc),
                Some(',') => Comma.to_tok(*loc),
                Some('.') => Dot.to_tok(*loc),
                Some('%') => Div.to_tok(*loc),
                Some('[') => match lin.peek() {
                    Some(']') => { step(loc,lin); Nil.to_tok(*loc) },
                    _ => BrackOpen.to_tok(*loc),
                },
                Some('<') => match lin.peek() {
                    Some('=') => { step(loc,lin); Leq.to_tok(*loc) },
                    _ => Lt.to_tok(*loc),
                },
                Some('>') => match lin.peek() {
                    Some('=') => { step(loc,lin); Geq.to_tok(*loc) },
                    _ => Gt.to_tok(*loc),
                },
                Some('!') => match lin.peek() {
                    Some('=') => { step(loc,lin); Neq.to_tok(*loc) },
                    _ => Not.to_tok(*loc),
                },
                Some('=') => match lin.peek() {
                    Some('=') => { step(loc,lin); Eq.to_tok(*loc) },
                    _ => Assign.to_tok(*loc),
                },
                Some('&') => match lin.peek() {
                    Some('&') => { step(loc,lin); And.to_tok(*loc) },
                    _ => TokFail("Found lone &".to_string()).to_tok(*loc),
                },
                Some('|') => match lin.peek() {
                    Some('|') => { step(loc,lin); Or.to_tok(*loc) },
                    _ => TokFail("Found lone |".to_string()).to_tok(*loc),
                },
                //               Some('\'') => match lin.
                //                    Char(step_or(loc,lin,"\' at end of line")?).to_tok(*loc),
                None => break,
                x => TokFail("Unrecognized character".to_string()).to_tok(*loc),
            };
            loc.advance();
        }
        tokens
    }
}
