
mod lex {
    use std::collections::HashMap;
    use std::str::{Chars,CharIndices};
    use crate::ast::{Id,Selector,BType,LitVal,Op,Loc,Located};
    use std::iter::{Peekable};
    use std::num::ParseIntError;
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
        TypeColon,
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
    
    impl TokAble for u32 {
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

    struct Lex<'s> {
        input : &'s str,
        loc : Loc,
        iter : Peekable<CharIndices<'s>>,
        wordtoks : HashMap<&'s str,Token>,
        vars : Vec<&'s str>,
    }

    impl Lex<'_>{
        fn step(&mut self) -> Option<(usize,char)> {
            self.loc.len += 1;
            self.iter.next()
        }

        fn step_or(&mut self, errmsg: &str) -> Option<Result<(usize,char),(String,Loc)>>{
            self.loc.len += 1;
            Some(self.iter.next().ok_or((errmsg.to_string(),self.loc.to_owned())))
        }

        fn ipeek(&mut self) -> Option<char>{
            Some(self.iter.peek()?.1)
        }

        fn step_ch(&mut self) -> Option<char> {
            self.loc.len += 1;
            Some(self.iter.next()?.1)
        }

        fn escape_char(&mut self) -> Option<Result<char,&'static str>> {
            Some(match self.step_ch()? {
                '\n' => Ok('\n'),
                '\\' => Ok('\\'),
                '\n' => Err("\'\\ at end of line"),
                _ => Err("Unrecognized escape sequence"),
            })
        }

        fn parse_int(&mut self, start : usize) -> Result<i64,ParseIntError> {
            let mut end = start;
            loop {
                match self.step() {
                    Some((npos,c)) => {end = npos; if !c.is_digit(10) {break}}
                    None => return self.input[start..].parse(),
                }
            }
            self.input[start..=end].parse()
        }

        fn parse_word(&mut self, start : usize) -> Token {
            0.to_tok(self.loc)
        }

        fn line_comment(&mut self) {
            self.iter.skip_while(|(_,c)| { !(c == '\n') });
            self.loc.next_line();
        }

        fn block_comment(&mut self) {
        }
    }

    macro_rules! fail {
        ( $reason : expr, $loc : expr ) => {return Some(Err(($reason.to_string(),$loc)))};
    }

    impl Iterator for Lex<'_> {
        type Item = Result<Token,(String,Loc)>;
        fn next(&mut self) -> Option<Self::Item> {
            self.loc.advance();
            let (pos,chr) = self.step()?;
            Some(Ok(match chr {
                '+' => Plus.to_tok(self.loc),
                ';' => Semicolon.to_tok(self.loc),
                '(' => ParenOpen.to_tok(self.loc),
                ')' => ParenClose.to_tok(self.loc),
                '{' => BraceOpen.to_tok(self.loc),
                '}' => BraceClose.to_tok(self.loc),
                ']' => BrackClose.to_tok(self.loc),
                ',' => Comma.to_tok(self.loc),
                '.' => Dot.to_tok(self.loc),
                '%' => Div.to_tok(self.loc),
                '*' => Mul.to_tok(self.loc),
                '[' => match self.ipeek() {
                    Some(']') => { self.step(); Nil.to_tok(self.loc) },
                    _ => BrackOpen.to_tok(self.loc),
                },
                '<' => match self.ipeek() {
                    Some('=') => { self.step(); Leq.to_tok(self.loc) },
                    _ => Lt.to_tok(self.loc),
                },
                '>' => match self.ipeek() {
                    Some('=') => { self.step(); Geq.to_tok(self.loc) },
                    _ => Gt.to_tok(self.loc),
                },
                '!' => match self.ipeek() {
                    Some('=') => { self.step(); Neq.to_tok(self.loc) },
                    _ => Not.to_tok(self.loc),
                },
                '=' => match self.ipeek() {
                    Some('=') => { self.step(); Eq.to_tok(self.loc) },
                    _ => Assign.to_tok(self.loc),
                },
                ':' => match self.ipeek() {
                    Some(':') => { self.step(); TypeColon.to_tok(self.loc) },
                    _ => Cons.to_tok(self.loc),
                },
                '&' => match self.ipeek() {
                    Some('&') => { self.step(); And.to_tok(self.loc) },
                    _ => fail!("Found lone &",self.loc),
                },
                '|' => match self.ipeek() {
                    Some('|') => { self.step(); Or.to_tok(self.loc) },
                    _ => fail!("Found lone |",self.loc),
                },
                '\'' => match self.step_ch() {
                    Some('\n') | None => fail!("\' at end of line",self.loc),
                    Some('\\') => match self.escape_char()? {
                        Ok(x) => Char(x).to_tok(self.loc),
                        Err(e) => fail!(e,self.loc),
                    },
                    Some(x) => Char(x).to_tok(self.loc),
                }
                '-' => match self.iter.peek().copied() {
                    Some((_,'>')) => Arrow.to_tok(self.loc),
                    Some((numpos,x)) => match x.is_digit(10) {
                        true => Int(-self.parse_int(numpos).unwrap()).to_tok(self.loc),
                        false => Minus.to_tok(self.loc),
                    },
                    _ => Minus.to_tok(self.loc),
                }
                '/' => match self.step_ch() {
                    Some('/') => { self.line_comment(); return self.next() },
                    Some('*') => { self.block_comment(); return self.next() },
                    _ => fail!("Found lone /",self.loc),

                }
                '\n' => { self.loc.next_line(); return self.next() },
                x => {
                    if x.is_alphabetic() { self.parse_word(pos) }
                    else if x.is_digit(10) { Int(self.parse_int(pos).unwrap()).to_tok(self.loc)}
                    else if x.is_whitespace() { return self.next() }
                    else { fail!("Unrecognized character",self.loc) }
                },
            }))
        }
    }


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
    /*
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
    */
}
