
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
        fn to_tok(self,l:Loc) -> Token {
            match self {
                PreId(x) => x.to_tok(l),
                PreSel(x) => x.to_tok(l),
                PreLit(x) => x.to_tok(l),
                PreTyp(x) => x.to_tok(l),
                PreMisc(x) => x.to_tok(l),
            }
        }
    }

    struct Lex<'s> {
        input : &'s str,
        loc : Loc,
        chars : Peekable<CharIndices<'s>>,
        wordtoks : HashMap<&'s str,PreToken>,
        names : Vec<&'s str>,
        vcount : u32,
    }

    impl Lex<'_>{
        pub(super) fn lex<'s>(source: &'s str) -> Lex<'s>{
            let mut keywords = HashMap::with_capacity(256);
            keywords.insert("var",PreMisc(Var));
            keywords.insert("Void",PreTyp(UnitT));
            keywords.insert("Int",PreTyp(IntT));
            keywords.insert("Bool",PreTyp(BoolT));
            keywords.insert("Char",PreTyp(CharT));
            keywords.insert("if",PreMisc(If));
            keywords.insert("else",PreMisc(Else));
            keywords.insert("while",PreMisc(While));
            keywords.insert("return",PreMisc(Return));
            keywords.insert("hd",PreSel(Hd));
            keywords.insert("tl",PreSel(Tl));
            keywords.insert("fst",PreSel(Fst));
            keywords.insert("snd",PreSel(Snd));
            keywords.insert("False",PreLit(Bool(false)));
            keywords.insert("True",PreLit(Bool(true)));


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
                '\n' => Ok('\n'),
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

        fn parse_word(&mut self, start : usize) -> PreToken {
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
                    PreId(self.vcount-1)
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
                '-' => match self.chars.peek().copied() {
                    Some((_,'>')) => Arrow.to_tok(self.loc),
                    Some((numpos,x)) => match x.is_digit(10) {
                        true => Int(-self.parse_int(numpos).unwrap()).to_tok(self.loc),
                        false => Minus.to_tok(self.loc),
                    },
                    _ => Minus.to_tok(self.loc),
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
                    if x.is_alphabetic() { self.parse_word(pos).to_tok(self.loc) }
                    else if x.is_digit(10) { Int(self.parse_int(pos).unwrap()).to_tok(self.loc)}
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
