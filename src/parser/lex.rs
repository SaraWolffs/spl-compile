use std::collections::HashMap;
use std::str::{CharIndices};
use crate::ast::{Selector,BType,LitVal,Op,Loc,Located};
use std::iter::{Peekable};
use std::num::ParseIntError;
//use core::slice::{Iter};


use super::tok::*;
use crate::ast::Selector::*;
use crate::ast::BType::*;
use crate::ast::LitVal::*;
use crate::ast::Op::*;
use Misc::*;


pub(super) struct Lex<'s> {
    input : &'s str,
    loc : Loc,
    chars : Peekable<CharIndices<'s>>,
    wordtoks : HashMap<&'s str,Token>,
    pub names : Vec<&'s str>,
    vcount : u32,
}


impl<'sub, 's : 'sub> Lex<'s>{
    pub fn lex(source: &'s str) -> Lex<'s>{ // TODO: remove loc.len+=, use loc.step()
        use super::tok::Token::*;
        let mut keywords = HashMap::with_capacity(256);
        keywords.insert("var",Marker(Var));
        keywords.insert("Void",TypeTok(UnitT));
        keywords.insert("Int",TypeTok(IntT));
        keywords.insert("Bool",TypeTok(BoolT));
        keywords.insert("Char",TypeTok(CharT));
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

    fn ipeek(&mut self) -> Option<char>{
        Some(self.chars.peek()?.1)
    }

    fn step_ch(&mut self) -> Option<char> {
        let ret = Some(self.chars.next()?.1);
        self.loc.len += 1;
        ret
    }

    fn escape_char(&mut self) -> Result<char,&'static str> {
        match self.step_ch() {
            Some('n') => Ok('\n'),
            Some('\\') => Ok('\\'),
            Some('\n') | None => Err("\'\\ at end of line"),
            _ => Err("Unrecognized escape sequence"),
        }
    }

    fn step_while(&mut self, start : usize, prop : fn(char) -> bool) -> &'sub str {
        assert!(prop(self.input[start..].chars().next().unwrap()));
        loop {
            match self.chars.peek() {
                Some((end,c)) => 
                    if prop(*c) {
                        self.step();
                    } else {
                        return &self.input[start..*end]
                    }
                None => return &self.input[start..]
            }
        }
    }

    fn parse_int(&mut self, start : usize) -> Result<i64,ParseIntError> {
        self.step_while(start, |c| {c.is_digit(10)}).parse()
    }

    fn parse_word(&mut self, start : usize) -> Token {
        let word = self.step_while(start,char::is_alphanumeric);
        let names = &mut self.names;
        let vcount = &mut self.vcount;
        let wordtoks = &mut self.wordtoks; // pacify the borrow checker
        *wordtoks.entry(word).or_insert_with(|| {
                names.push(word);
                *vcount += 1;
                crate::parser::lex::Token::IdTok(*vcount-1)
        })
    }
    fn parse_char(&mut self) -> Result<LocTok,&'static str> {
        Ok(match self.step_ch() {
            Some('\n') | None => return Err("\' at end of line"),
            Some('\\') => Char(self.escape_char()?).to_ltok(self.loc),
            Some(x) => Char(x).to_ltok(self.loc),
        })
    }

    fn line_comment(&mut self) {
        for (_,c) in &mut self.chars {
                if c == '\n' {break};
        };
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
            '\'' => match self.parse_char() {
                Ok(c) => c,
                Err(msg) => fail!(msg,self.loc),
            }
            '-' => match self.chars.peek().copied() {
                Some((_,'>')) => Arrow.to_ltok(self.loc),
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

#[cfg(test)]
mod tests {
    use super::*;
    fn tloc(line: u32, col: u16, len: u16) -> Loc {
        Loc{line: line, col: col, len: len}
    }

    #[test]
    fn test_sanity() {
        assert!(true);
    }
    // Metatesting: multiple ways to lex a single token
    #[test]
    fn lex_plus() {
        let mut toks = Lex::lex("+");
        assert_eq!(toks.next(),Some(Ok((Token::Op(Plus),tloc(0, 0, 1)))));
    }
    #[test]
    fn lex_comma() {
        let mut toks = Lex::lex(",");
        assert_eq!(toks.next().unwrap().unwrap().0,Token::Marker(Comma));
    }
    #[test]
    fn lex_dot() {
        let mut toks = Lex::lex(".").map(|x| x.unwrap().0);
        assert_eq!(toks.next().unwrap(),Token::Marker(Dot));
    }
    #[test]
    fn lex_num() {
        let tok = Lex::lex("37").map(|x| x.unwrap().0).next().unwrap();
        assert_eq!(tok,Token::Lit(Int(37)));
    }

    #[test]
    fn lex_twintok_leq() {
        let mut toks = Lex::lex("<=").map(|x| x.unwrap().0);
        assert_eq!(toks.next().unwrap(),Token::Op(Leq));
    }

    #[test]
    fn lex_prefix_minus() {
        let mut toks = Lex::lex("-").map(|x| x.unwrap().0);
        assert_eq!(toks.next().unwrap(),Token::Op(Minus));
    }

    #[test]
    fn lex_lone_slash() {
        let mut toks = Lex::lex("/");
        assert_eq!(toks.next().unwrap(),Err(("Found lone /".to_string(), tloc(0,0,1))));
    }

    #[test]
    fn lex_linecomment() {
        let mut toks = Lex::lex("// This should not lex as anything\n+");
        assert_eq!(toks.next().unwrap().unwrap(),(Token::Op(Plus), tloc(1,0,1)));
    }

    #[test]
    fn lex_char() {
        let mut toks = Lex::lex("'a");
        assert_eq!(toks.next().unwrap().unwrap(),(Token::Lit(Char('a')), tloc(0,0,2)));
    }


    // Change: lexing of negative numbers to be determined parser-stage.
    // Bug fixed: don't parse 1 past the end of the number.
    #[test]
    fn lex_negnum() {
        let mut toks = Lex::lex("-42").map(|x| x.unwrap());
        assert_eq!(toks.next().unwrap(),(Token::Op(Minus),tloc(0,0,1)));
        assert_eq!(toks.next().unwrap(),(Token::Lit(Int(42)),tloc(0,1,2)));
        assert_eq!(toks.next(),None);
    }

    #[test]
    fn lex_vars() {
        let mut lexer = Lex::lex("foo while b4r foo");
        let mut toks = lexer.by_ref().map(|x| x.unwrap());
        let footok = toks.next().unwrap();
        let foo = match footok.0 {
            Token::IdTok(x) => x,
            _ => panic!(),
        };
        assert_eq!(footok,(Token::IdTok(foo),tloc(0,0,3)));
        assert_eq!(toks.next().unwrap(),(Token::Marker(While),tloc(0,4,5)));
        assert_eq!(toks.next().unwrap(),(Token::IdTok(foo+1),tloc(0,10,3)));
        assert_eq!(toks.next().unwrap(),(Token::IdTok(foo),tloc(0,14,3)));
        assert_eq!(toks.next(),None);
        assert_eq!(lexer.names[foo as usize],"foo");
        assert_eq!(lexer.names[(foo+1) as usize],"b4r");
    }
        
}
