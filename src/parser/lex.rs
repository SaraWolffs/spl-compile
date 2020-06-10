use std::collections::HashMap;
use std::str::{CharIndices};
use crate::ast::{Selector,BType,LitVal,Op,Loc,Located};
use std::iter::{Peekable};
use std::num::ParseIntError;
//use core::slice::{Iter};

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

trait TokAble  {
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
use crate::ast::Selector::*;

impl TokAble for BType {
    fn to_tok(self) -> Token {
        Token::TypeTok(self)
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

pub(super) struct Lex<'s> {
    input : &'s str,
    loc : Loc,
    chars : Peekable<CharIndices<'s>>,
    wordtoks : HashMap<&'s str,Token>,
    names : Vec<&'s str>,
    vcount : u32,
}

impl Lex<'_>{
    pub fn lex<'s>(source: &'s str) -> Lex<'s>{
        use crate::parser::lex::Token::*;
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
            match self.chars.peek() {
                Some((end,c)) => if !c.is_digit(10) {
                    return self.input[start..*end].parse()} else {self.step();},
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
                crate::parser::lex::Token::IdTok(self.vcount-1)
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

    // Test: changed lexing of negative numbers to be determined parser-stage.
    #[test]
    fn lex_negnum() {
        let mut toks = Lex::lex("-42").map(|x| x.unwrap());
        assert_eq!(toks.next().unwrap(),(Token::Op(Minus),tloc(0,0,1)));
        assert_eq!(toks.next().unwrap(),(Token::Lit(Int(42)),tloc(0,1,2)));
        assert_eq!(toks.next(),None);
    }
        
}
