
mod lex {
    use std::collections::HashMap;
    use std::str::{Chars,CharIndices};
    use crate::ast::{Id,Selector,BType,LitVal,Op,Loc,Located};
    use std::iter::{Peekable};
    use std::num::ParseIntError;
    //use core::slice::{Iter};

    #[derive(Copy,Clone)]
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

    #[derive(Copy,Clone)]
    pub(super) enum Token {
        Id(u32),
        Selector(Selector),
        Type(BType),
        Lit(LitVal),
        Op(Op),
        Marker(Misc),
    }
    pub(super) type TokStream = Vec<Token>;
    pub(super) type LocTok = Located<Token>;

    trait TokAble  {
        fn to_tok(self) -> Token;
        fn to_ltok(self,l:Loc) -> LocTok 
            where Self: std::marker::Sized {
            (self.to_tok(),l)
        }

    }
    
    impl TokAble for u32 {
        fn to_tok(self) -> Token {
            Token::Id(self)
        }
    }
    
    impl TokAble for Selector {
        fn to_tok(self) -> Token {
            Token::Selector(self)
        }
    }
    use crate::ast::Selector::*;

    impl TokAble for BType {
        fn to_tok(self) -> Token {
            Token::Type(self)
        }
    }
    use crate::ast::BType::*;

    impl TokAble for LitVal {
        fn to_tok(self) -> Token {
            Token::Lit(self)
        }
    }
    use crate::ast::LitVal::*;

    impl TokAble for Op {
        fn to_tok(self) -> Token {
            Token::Op(self)
        }
    }
    use crate::ast::Op::*;
    
    impl TokAble for Misc {
        fn to_tok(self) -> Token {
            Token::Marker(self)
        }
    }
    use Misc::*;

    /*
    #[derive(Copy,Clone)]
    enum PreToken {
        PreId(u32),
        PreSel(Selector),
        PreLit(LitVal),
        PreTyp(BType),
        PreMisc(Misc),
    }
    use PreToken::*;

    impl TokAble for PreToken {
        fn to_tok(self) -> Token {
            match self {
                PreId(x) => x.to_tok(l),
                PreSel(x) => x.to_tok(l),
                PreLit(x) => x.to_tok(l),
                PreTyp(x) => x.to_tok(l),
                PreMisc(x) => x.to_tok(l),
            }
        }
    }
    */

    struct Lex<'s> {
        input : &'s str,
        loc : Loc,
        chars : Peekable<CharIndices<'s>>,
        wordtoks : HashMap<&'s str,Token>,
        names : Vec<&'s str>,
        vcount : u32,
    }

    impl Lex<'_>{
        pub(super) fn lex<'s>(source: &'s str) -> Lex<'s>{
            use crate::parser::lex::Token::*;
            let mut keywords = HashMap::with_capacity(256);
            keywords.insert("var",Marker(Var));
            keywords.insert("Void",Type(UnitT));
            keywords.insert("Int",Type(IntT));
            keywords.insert("Bool",Type(BoolT));
            keywords.insert("Char",Type(CharT));
            keywords.insert("if",Marker(If));
            keywords.insert("else",Marker(Else));
            keywords.insert("while",Marker(While));
            keywords.insert("return",Marker(Return));
            keywords.insert("hd",Selector(Hd));
            keywords.insert("tl",Selector(Tl));
            keywords.insert("fst",Selector(Fst));
            keywords.insert("snd",Selector(Snd));
            keywords.insert("False",Lit(Bool(false)));
            keywords.insert("True",Lit(Bool(true)));


            Lex{ input : source, 
                 loc : Loc { line : 0, col : 0, len : 0 },
                 chars : source.char_indices().peekable(),
                 wordtoks : keywords,
                 names : Vec::with_capacity(128),
                 vcount : 0,
            }

        }

        fn step(&mut self) -> Option<(usize,char)> {
            self.loc.len += 1;
            self.chars.next()
        }

        fn step_or(&mut self, errmsg: &str) -> Option<Result<(usize,char),(String,Loc)>>{
            self.loc.len += 1;
            Some(self.chars.next().ok_or((errmsg.to_string(),self.loc.to_owned())))
        }

        fn ipeek(&mut self) -> Option<char>{
            Some(self.chars.peek()?.1)
        }

        fn step_ch(&mut self) -> Option<char> {
            self.loc.len += 1;
            Some(self.chars.next()?.1)
        }

        fn escape_char(&mut self) -> Option<Result<char,&'static str>> {
            Some(match self.step_ch()? {
                'n' => Ok('\n'),
                '\\' => Ok('\\'),
                '\n' => Err("\'\\ at end of line"),
                _ => Err("Unrecognized escape sequence"),
            })
        }

        fn parse_int(&mut self, start : usize) -> Result<i64,ParseIntError> {
            loop {
                match self.step() {
                    Some((end,c)) => if !c.is_digit(10) {
                        return self.input[start..end].parse()},
                    None => return self.input[start..].parse(),
                }
            }
        }

        fn parse_word(&mut self, start : usize) -> Token {
            let word : &str = loop {
                match self.step() {
                    Some((end,c)) => if !c.is_alphanumeric() {
                        break &self.input[start..end]},
                    None => break &self.input[start..],
                }
            };
            *self.wordtoks.entry(word).or_insert({
                    self.names.push(word);
                    self.vcount += 1;
                    crate::parser::lex::Token::Id(self.vcount-1)
            })
        }

        fn line_comment(&mut self) {
            for (_,c) in &mut self.chars {
                    if c == '\n' {break};
            };
            /*
            loop {
                match self.chars.next() {
                    Some((_,c)) => if c == '\n' {break},
                    None => break,
                }
            }
            */
            self.loc.next_line();
        }

        fn rec_block_comment(&mut self) -> Option<()> {
            loop{
                match self.step_ch()? {
                    '*' => match self.ipeek() {
                        Some('/') => { self.step(); break Some(()); },
                        _ => ()
                    },
                    '/' => match self.ipeek() {
                        Some('*') => { self.step(); self.rec_block_comment()?; () },
                        _ => (),
                    },
                    '\n' => self.loc.next_line(),
                    _ => (),
                }
            }
        }

        fn block_comment(&mut self) -> Result<(),Loc> {
            let startloc = self.loc;
            self.rec_block_comment().ok_or(startloc)
        }
    }

    macro_rules! fail {
        ( $reason : expr, $loc : expr ) => {return Some(Err(($reason.to_string(),$loc)))};
    }

    impl Iterator for Lex<'_> {
        type Item = Result<LocTok,(String,Loc)>;
        fn next(&mut self) -> Option<Self::Item> {
            self.loc.advance();
            let (pos,chr) = self.step()?;
            Some(Ok(match chr {
                '+' => Plus.to_ltok(self.loc),
                ';' => Semicolon.to_ltok(self.loc),
                '(' => ParenOpen.to_ltok(self.loc),
                ')' => ParenClose.to_ltok(self.loc),
                '{' => BraceOpen.to_ltok(self.loc),
                '}' => BraceClose.to_ltok(self.loc),
                ']' => BrackClose.to_ltok(self.loc),
                ',' => Comma.to_ltok(self.loc),
                '.' => Dot.to_ltok(self.loc),
                '%' => Div.to_ltok(self.loc),
                '*' => Mul.to_ltok(self.loc),
                '[' => match self.ipeek() {
                    Some(']') => { self.step(); Nil.to_ltok(self.loc) },
                    _ => BrackOpen.to_ltok(self.loc),
                },
                '<' => match self.ipeek() {
                    Some('=') => { self.step(); Leq.to_ltok(self.loc) },
                    _ => Lt.to_ltok(self.loc),
                },
                '>' => match self.ipeek() {
                    Some('=') => { self.step(); Geq.to_ltok(self.loc) },
                    _ => Gt.to_ltok(self.loc),
                },
                '!' => match self.ipeek() {
                    Some('=') => { self.step(); Neq.to_ltok(self.loc) },
                    _ => Not.to_ltok(self.loc),
                },
                '=' => match self.ipeek() {
                    Some('=') => { self.step(); Eq.to_ltok(self.loc) },
                    _ => Assign.to_ltok(self.loc),
                },
                ':' => match self.ipeek() {
                    Some(':') => { self.step(); TypeColon.to_ltok(self.loc) },
                    _ => Cons.to_ltok(self.loc),
                },
                '&' => match self.ipeek() {
                    Some('&') => { self.step(); And.to_ltok(self.loc) },
                    _ => fail!("Found lone &",self.loc),
                },
                '|' => match self.ipeek() {
                    Some('|') => { self.step(); Or.to_ltok(self.loc) },
                    _ => fail!("Found lone |",self.loc),
                },
                '\'' => match self.step_ch() {
                    Some('\n') | None => fail!("\' at end of line",self.loc),
                    Some('\\') => match self.escape_char()? {
                        Ok(x) => Char(x).to_ltok(self.loc),
                        Err(e) => fail!(e,self.loc),
                    },
                    Some(x) => Char(x).to_ltok(self.loc),
                }
                '-' => match self.chars.peek().copied() {
                    Some((_,'>')) => Arrow.to_ltok(self.loc),
                    Some((numpos,x)) => match x.is_digit(10) {
                        true => Int(-self.parse_int(numpos).unwrap()).to_ltok(self.loc),
                        false => Minus.to_ltok(self.loc),
                    },
                    _ => Minus.to_ltok(self.loc),
                }
                '/' => match self.step_ch() {
                    Some('/') => { self.line_comment(); return self.next() },
                    Some('*') => match self.block_comment() {
                        Err(l) => fail!("Unclosed block comment started",l),
                        Ok(()) => return self.next(),
                    },
                    _ => fail!("Found lone /",self.loc),

                }
                '\n' => { self.loc.next_line(); return self.next() },
                x => {
                    if x.is_alphabetic() { (self.parse_word(pos),self.loc) }
                    else if x.is_digit(10) { Int(self.parse_int(pos).unwrap()).to_ltok(self.loc)}
                    else if x.is_whitespace() { return self.next() }
                    else { fail!("Unrecognized character",self.loc) }
                },
            }))
        }
    }


    fn step<T>( loc: &mut Loc, iterable: &mut dyn Iterator<Item = T>,
    ) -> Option<T> {
        loc.len += 1;
        iterable.next()
    }

    fn step_or<T>(loc: &mut Loc, iterable: &mut dyn Iterator<Item = T>, errmsg: &str,
    ) -> Result<T,(String,Loc)> {
        loc.len += 1;
        iterable.next().ok_or((errmsg.to_string(),loc.to_owned()))
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
                Some('+') => Plus.to_ltok(*loc),
                Some(';') => Semicolon.to_ltok(*loc),
                Some('(') => ParenOpen.to_ltok(*loc),
                Some(')') => ParenClose.to_ltok(*loc),
                Some('{') => BraceOpen.to_ltok(*loc),
                Some('}') => BraceClose.to_ltok(*loc),
                Some(']') => BrackClose.to_ltok(*loc),
                Some(',') => Comma.to_ltok(*loc),
                Some('.') => Dot.to_ltok(*loc),
                Some('%') => Div.to_ltok(*loc),
                Some('[') => match lin.peek() {
                    Some(']') => { step(loc,lin); Nil.to_ltok(*loc) },
                    _ => BrackOpen.to_ltok(*loc),
                },
                Some('<') => match lin.peek() {
                    Some('=') => { step(loc,lin); Leq.to_ltok(*loc) },
                    _ => Lt.to_ltok(*loc),
                },
                Some('>') => match lin.peek() {
                    Some('=') => { step(loc,lin); Geq.to_ltok(*loc) },
                    _ => Gt.to_ltok(*loc),
                },
                Some('!') => match lin.peek() {
                    Some('=') => { step(loc,lin); Neq.to_ltok(*loc) },
                    _ => Not.to_ltok(*loc),
                },
                Some('=') => match lin.peek() {
                    Some('=') => { step(loc,lin); Eq.to_ltok(*loc) },
                    _ => Assign.to_ltok(*loc),
                },
                Some('&') => match lin.peek() {
                    Some('&') => { step(loc,lin); And.to_ltok(*loc) },
                    _ => TokFail("Found lone &".to_string()).to_ltok(*loc),
                },
                Some('|') => match lin.peek() {
                    Some('|') => { step(loc,lin); Or.to_ltok(*loc) },
                    _ => TokFail("Found lone |".to_string()).to_ltok(*loc),
                },
                //               Some('\'') => match lin.
                //                    Char(step_or(loc,lin,"\' at end of line")?).to_ltok(*loc),
                None => break,
                x => TokFail("Unrecognized character".to_string()).to_ltok(*loc),
            };
            loc.advance();
        }
        tokens
    }
    */
}
