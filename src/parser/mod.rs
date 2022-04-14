mod lex;
mod shunting_yard;
mod tok;

use std::fmt;
use std::fmt::Display;

use crate::ast::BareDecl::*;
use crate::ast::Selector;
use crate::ast::Span;
use crate::ast::*;
use lex::Lex;
use lex::LexError;

//use crate::ast::BareExp::Var as EVar;
pub use tok::Loc;
use tok::Misc::*;
use tok::Token;
use tok::Token::Lit as LitTok;
//use tok::Token::Selector as SelectTok;
use tok::Token::*;

use self::tok::LocTok;

type TokStream<'s> = lex::Lex<'s>;

pub struct Parser<'s> {
    ts: TokStream<'s>,
}

macro_rules! fail {
    ( $reason : expr, $loc : expr ) => {
        return Err(ParseError($reason.to_string(), Some($loc)))
    };
    ( $reason : expr ) => {
        return Err(ParseError($reason.to_string(), None))
    };
}

macro_rules! ipe {
    ( $msg : expr ) => {
        panic!("Internal parser error: {}", $msg)
    };
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ParseError(String, Option<tok::Loc>);

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(loc) = self.1 {
            write!(f, "{} at {}", self.0, loc)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError(err.0, Some(err.1))
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

fn opthull(lhs: Option<Span>, rhs: Option<Span>) -> Option<Span> {
    if let Some(l) = lhs {
        if let Some(r) = rhs {
            Some(Span::hull(l, r))
        } else {
            lhs
        }
    } else {
        rhs
    }
}

fn hull(lhs: Span, rhs: Span) -> Span {
    Span::hull(lhs, rhs)
}

fn eof<T>(expected: String) -> ParseResult<T> {
    Err(ParseError(
        format!("EOF while looking for {}", expected),
        None,
    ))
}

fn unexpected<T>(found: tok::LocTok, expected: String) -> ParseResult<T> {
    Err(ParseError(
        format!(
            "Unexpected {:?} encountered while looking for {}",
            found.0, expected
        ),
        Some(found.1),
    ))
}

fn lexfail<T>(err: LexError) -> ParseResult<T> {
    Err(ParseError::from(err))
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            ts: Lex::lex(source),
        }
    }

    pub fn names(&self) -> &Vec<&'s str> {
        &self.ts.names
    }

    pub fn to_names(self) -> Vec<&'s str> {
        self.ts.names
    }

    fn nexttok(&mut self) -> Option<<Lex as Iterator>::Item> {
        self.ts.next()
    }

    fn peekloctok(&mut self) -> ParseResult<Option<&tok::LocTok>> {
        match self.ts.peek() {
            None => Ok(None),
            Some(Err(e)) => lexfail(e.to_owned()),
            Some(Ok(ref loctok)) => Ok(Some(loctok)),
        }
    }
    fn peektok(&mut self) -> ParseResult<Option<&tok::Token>> {
        match self.ts.peek() {
            None => Ok(None),
            Some(Err(e)) => lexfail(e.to_owned()),
            Some(Ok((ref tok, _))) => Ok(Some(tok)),
        }
    }

    fn trytok(&mut self) -> ParseResult<Option<tok::LocTok>> {
        self.ts.next().transpose().map_err(ParseError::from)
        //.map_err(|(msg, loc)| (msg, Some(loc)))
    }

    fn sometok(&mut self, expected: &str) -> ParseResult<tok::LocTok> {
        self.trytok()?
            .ok_or_else(|| ParseError(format!("EOF while looking for {}", expected), None))
    }

    fn unpeektok(&mut self, val: tok::LocTok) -> ParseResult<&tok::LocTok> {
        self.ts
            .unpeek(val)
            .map_err(|_| ipe!("Attempted lookahead beyond 1"))
    }

    fn backtrack<T>(&mut self, found: tok::LocTok, expected: String) -> ParseResult<T> {
        self.unpeektok(found)?;
        unexpected(found, expected)
    }

    fn consume(&mut self, tok: Token, expected: &str) -> ParseResult<tok::LocTok> {
        match self.peekloctok()? {
            None => eof(format!("{:?}", tok)),
            Some(&(found, loc)) => {
                if found == tok {
                    self.nexttok();
                    Ok((found, loc))
                } else {
                    unexpected((found, loc), expected.to_owned())
                }
            }
        }
    }

    fn discard(&mut self, tok: Token) -> ParseResult<Loc> {
        let expected = format!("{:?}", tok);
        let (_, loc): LocTok = self.consume(tok, &expected)?;
        Ok(loc)
    }

    fn many<T>(&mut self, single: fn(&mut Self) -> ParseResult<Option<T>>) -> ParseResult<Vec<T>> {
        let mut acc = Vec::new();
        loop {
            let next = single(self)?;
            if let Some(parsed) = next {
                acc.push(parsed);
            } else {
                break Ok(acc);
            }
        }
    }

    fn one<T>(
        &mut self,
        p: fn(&mut Self) -> ParseResult<Option<T>>,
        expected: &str,
    ) -> ParseResult<T> {
        if let Some(parsed) = p(self)? {
            Ok(parsed)
        } else {
            match self.peekloctok()? {
                None => eof(expected.to_owned()),
                Some(&(found, loc)) => unexpected((found, loc), expected.to_owned()),
            }
        }
    }

    fn parse_id(&mut self) -> ParseResult<Id> {
        match self.sometok("identifier")? {
            (IdTok(id), idloc) => Ok((id, Some(idloc.into()))),
            found => unexpected(found, "identifier".to_owned()),
        }
    }

    fn decl(&mut self) -> ParseResult<Option<Decl>> {
        match self.trytok()? {
            None => Ok(None),
            Some((Marker(Var), loc)) => {
                let (id, exp, loc2) = self.var_init()?;
                Ok(Some((
                    Global((None, id, exp)),
                    Some(hull(Span::from(loc), Span::from(loc2))),
                )))
            }
            Some((IdTok(id), loc)) => Ok(Some(
                self.fun_or_named_type_var_decl((id, Some(Span::from(loc))))?,
            )),
            Some((nonid, loc)) => match nonid {
                TypeTok(_) | Marker(ParenOpen) | Marker(BrackOpen) => {
                    self.unpeektok((nonid, loc))?;
                    let typ = self.non_id_type()?;
                    let (id, exp, loc2) = self.var_init()?;
                    Ok(Some((
                        Global((Some(typ), id, exp)),
                        Some(hull(Span::from(loc), Span::from(loc2))),
                    )))
                }
                _ => self.backtrack((nonid, loc), "type or identifier".to_string()),
            },
        }
    }

    fn fun_or_named_type_var_decl(&mut self, id: Id) -> ParseResult<Decl> {
        use crate::ast::BareType::*;
        match self.peektok()? {
            None => eof("'(' or identifier".to_owned()),
            Some(Marker(ParenOpen)) => self.fun_def(id),
            Some(IdTok(_)) => {
                let (varid, exp, loc2) = self.var_init()?;
                Ok((
                    Global((Some((Typename(id.0), id.1)), varid, exp)),
                    Some(hull(Span::from(id.1.unwrap()), Span::from(loc2))),
                ))
            }
            _ => unexpected(
                self.peekloctok().unwrap().unwrap().to_owned(),
                "'(' or identifier".to_owned(),
            ),
        }
    }

    fn var_init(&mut self) -> ParseResult<(Id, Exp, Loc)> {
        let id = self.parse_id()?;
        let _: LocTok = self.consume(Marker(Assign), "'='")?;
        let exp = self.exp()?;
        let (_, loc) = self.consume(Marker(Semicolon), "';'")?;
        Ok((id, exp, loc))
    }

    fn fun_def(&mut self, id: Id) -> ParseResult<Decl> {
        let (args, _) = self.tuplish(Parser::parse_id)?;
        let ftype = if let Some(Marker(TypeColon)) = self.peektok()? {
            let _: LocTok = self.consume(Marker(TypeColon), "'::'").unwrap();
            Some(self.fun_type()?)
        } else {
            None
        };
        let (body, bspan) = self.compound()?;
        Ok((Fun(id, args, ftype, body), opthull(id.1, Some(bspan))))
    }

    fn fun_type(&mut self) -> ParseResult<FunType> {
        let intypes = self.many(Self::typ)?;
        let (_, arrowloc) = self.consume(Marker(Arrow), "'->'")?;
        let startspan = if let Some((_, typloc)) = intypes.first() {
            typloc.to_owned()
        } else {
            Some(arrowloc.into())
        };
        let outtype @ (_, endspan) = self.one(Self::typ, "type")?;
        Ok(((intypes, outtype), opthull(startspan, endspan)))
    }

    fn typ(&mut self) -> ParseResult<Option<Type>> {
        use crate::ast::BareType::*;
        match self.peektok()? {
            Some(TypeTok(_)) | Some(Marker(BraceOpen)) | Some(Marker(BrackOpen)) => {
                Ok(Some(self.non_id_type()?))
            }
            Some(IdTok(_)) => {
                if let (IdTok(id), idloc) = self.sometok("identifier").unwrap() {
                    Ok(Some((Typename(id), Some(idloc.into()))))
                } else {
                    unreachable!();
                }
            }
            _ => Ok(None),
        }
    }

    fn non_id_type(&mut self) -> ParseResult<Type> {
        use crate::ast::BareType::*;
        if let Some((tok, loc)) = self.trytok()? {
            let span: Span = loc.into();
            match tok {
                TypeTok(t) => Ok((Lit(t), Some(span))), // inline b_type
                Marker(BraceOpen) => {
                    self.unpeektok((tok, loc))?;
                    let (tvec, tupspan) = self.tuplish(|p| p.one(Self::typ, "type"))?;
                    Ok((Tuple(tvec), Some(tupspan)))
                }
                Marker(BrackOpen) => {
                    let inner = self.one(Self::typ, "type")?;
                    let (_, endloc): LocTok = self.consume(Marker(BrackClose), "']'")?;
                    Ok((List(Box::new(inner)), Some(hull(span, endloc.into()))))
                }
                _ => unexpected((tok, loc), "non-identifier type".to_owned()),
            }
        } else {
            eof("type".to_owned())
        }
    }

    fn stmt(&mut self) -> ParseResult<Option<Stmt>> {
        use crate::parser::tok::Misc::While as WhileTok;
        use BareStmt::*;
        if let Some(loctok @ (tok, loc)) = self.trytok()? {
            let startspan = loc.into();
            match tok {
                Marker(If) => {
                    // parse if-then-else
                    let _: LocTok = self.consume(Marker(ParenOpen), "'('")?;
                    let cond = self.exp()?;
                    let _: LocTok = self.consume(Marker(ParenClose), "')'")?;
                    let (then, thenspan) = self.compound()?;
                    let (alt, endspan) = if let Some(Marker(Else)) = self.peektok()? {
                        self.nexttok();
                        self.compound()?
                    } else {
                        (Vec::new(), thenspan)
                    };
                    Ok(Some((ITE(cond, then, alt), Some(hull(startspan, endspan)))))
                }
                Marker(WhileTok) => {
                    // parse while
                    let _: LocTok = self.consume(Marker(ParenOpen), "'('")?;
                    let cond = self.exp()?;
                    let _: LocTok = self.consume(Marker(ParenClose), "')'")?;
                    let (body, endspan) = self.compound()?;
                    Ok(Some((While(cond, body), Some(hull(startspan, endspan)))))
                }
                IdTok(id) => Ok(Some(self.assign_init_or_call((id, Some(startspan)))?)),
                Marker(Var) => {
                    // parse untyped local var declaration
                    let (id, exp, endloc) = self.var_init()?;
                    Ok(Some((
                        Local((None, id, exp)),
                        Some(hull(startspan, endloc.into())),
                    )))
                }
                TypeTok(_) | Marker(BraceOpen) | Marker(BrackOpen) => {
                    self.unpeektok(loctok)?;
                    let t = self.non_id_type()?;
                    let (id, exp, endloc) = self.var_init()?;
                    Ok(Some((
                        Local((Some(t), id, exp)),
                        Some(hull(startspan, endloc.into())),
                    )))
                }
                Marker(Return) => {
                    // parse return
                    if let Some((Marker(Semicolon), endloc)) = self.peekloctok()? {
                        Ok(Some((
                            Ret(None),
                            Some(hull(startspan, endloc.to_owned().into())),
                        )))
                    } else {
                        let retval = self.exp()?;
                        let (_, endloc) = self.consume(Marker(Semicolon), ";")?;
                        Ok(Some((
                            Ret(Some(retval)),
                            Some(hull(startspan, endloc.into())),
                        )))
                    }
                }
                _ => {
                    // no statement found, caller decides if that's an issue.
                    self.unpeektok(loctok)?;
                    Ok(None)
                }
            }
        } else {
            // EOF. This is probably a problem for the caller, but not ours.
            Ok(None)
        }
    }

    fn assign_init_or_call(&mut self, id: Id) -> ParseResult<Stmt> {
        use tok::Misc::Assign as AssignTok;
        use BareStmt::*;
        if let Some(tok) = self.peektok()? {
            match tok {
                Marker(Dot) | Marker(AssignTok) => {
                    let (field, _) = self.field()?;
                    self.discard(Marker(AssignTok))?;
                    let exp = self.exp()?;
                    let endloc = self.discard(Marker(Semicolon))?;
                    Ok((Assign(id, field, exp), opthull(id.1, Some(endloc.into()))))
                }
                Marker(ParenOpen) => {
                    let (args, endspan) = self.tuplish(Self::exp)?;
                    Ok((Call(id, args), opthull(id.1, Some(endspan))))
                }
                IdTok(_) => {
                    let (vname, exp, endloc) = self.var_init()?;
                    Ok((
                        Local((Some((BareType::Typename(id.0), id.1)), vname, exp)),
                        opthull(id.1, Some(endloc.into())),
                    ))
                }
                _ => unexpected(
                    self.peekloctok().unwrap().unwrap().to_owned(),
                    "'.', '=', '(' or identifier".to_owned(),
                ),
            }
        } else {
            eof("'.', '=', '(', or identifier".to_owned())
        }
    }

    fn compound(&mut self) -> ParseResult<(Vec<Stmt>, Span)> {
        let startspan = self.discard(Marker(BraceOpen))?.into();
        let stmts = self.many(Self::stmt)?;
        let endspan = self.discard(Marker(BraceClose))?.into();
        Ok((stmts, hull(startspan, endspan)))
    }

    fn selector(&mut self) -> ParseResult<Option<Selector>> {
        if let Some((Marker(Dot), startloc)) = self.peekloctok()? {
            let startspan = startloc.to_owned().into();
            self.nexttok();
            let loctok = self.sometok("'hd', 'tl', 'fst', or 'snd'")?;
            if let (Selector(sel), endloc) = loctok {
                Ok(Some((sel, Some(hull(startspan, endloc.into())))))
            } else {
                unexpected(loctok, "'hd', 'tl', 'fst', or 'snd'".to_owned())
            }
        } else {
            Ok(None)
        }
    }

    fn field(&mut self) -> ParseResult<(Vec<Selector>, Option<Span>)> {
        let selectors = self.many(Self::selector)?;
        let startspan = selectors.first().map(|x| x.1).flatten();
        let endspan = selectors.last().map(|x| x.1).flatten();
        Ok((selectors, opthull(startspan, endspan)))
    }

    fn exp(&mut self) -> ParseResult<Exp> {
        shunting_yard::ShuntingYard::parse_exp(self)
    }

    fn atom(&mut self) -> ParseResult<Exp> {
        use BareExp::*;
        match self.trytok()? {
            None => eof("identifier, literal, or '('".to_string()),
            Some((tok, loc)) => match tok {
                IdTok(i) => self.field_or_call((i, Some(loc.into()))),
                LitTok(val) => Ok(((Lit(val), None), Some(loc.into()))),
                Marker(ParenOpen) => {
                    self.unpeektok((Marker(ParenOpen), loc))?;
                    let (coords, span) = self.tuplish(Self::exp)?;
                    if coords.len() == 1 {
                        Ok((coords.into_iter().next().unwrap().0, Some(span)))
                    } else {
                        Ok(((Tuple(coords), None), Some(span))) // unknown type, known location
                    }
                }
                x => unexpected((x, loc), "identifier, literal, or '('".to_string()),
            },
        }
    }

    fn field_or_call(&mut self, id: Id) -> ParseResult<Exp> {
        use BareExp::*;
        match self.peekloctok()? {
            None => Ok(((Var(id, Vec::new()), None), id.1)),
            Some((tok, _)) => match tok {
                Marker(ParenOpen) => {
                    let (args, end) = self.tuplish(Self::exp)?;
                    Ok(((Call(id, args), None), opthull(id.1, Some(end))))
                }
                _ => {
                    let (fld, end) = self.field()?;
                    Ok(((Var(id, fld), None), opthull(id.1, end)))
                }
            },
        }
    }

    fn tuplish<T>(
        &mut self,
        single: fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<(Vec<T>, Span)> {
        let span = {
            if let Some(Ok((Marker(ParenOpen), loc))) = self.nexttok() {
                loc.into()
            } else {
                panic!("Internal parser error: called tuplish without starting '('")
            }
        };
        let mut vec = Vec::new();
        if let Some(&(Marker(ParenClose), loc)) = self.peekloctok()? {
            return {
                self.nexttok();
                Ok((vec, hull(span, loc.into())))
            };
        }
        loop {
            vec.push(single(self)?);
            match self.nexttok() {
                None => fail!("EOF while parsing tuple"),
                Some(Err(e)) => break lexfail(e),
                Some(Ok((Marker(ParenClose), loc))) => break Ok((vec, hull(span, loc.into()))),
                Some(Ok((Marker(Comma), _))) => (),
                Some(Ok(loctok)) => {
                    self.unpeektok(loctok)?;
                    break unexpected(loctok, "',' or ')'".to_string());
                }
            }
        }
    }
}

impl<'s> Iterator for Parser<'s> {
    type Item = ParseResult<Decl>;
    fn next(&mut self) -> Option<Self::Item> {
        self.decl().transpose()
    }
}

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
    use super::lex::*;
    use super::*;
    fn tspan(startline: u32, endline: u32, startcol: u32, endcol: u32) -> Option<Span> {
        Some(Span::new(startline, endline, startcol, endcol))
    }

    #[test]
    fn test_sanity() {
        assert!(true);
    }

    #[test]
    fn parse_litint_atom() {
        use BareExp::*;
        use LitVal::*;
        let mut p = Parser::new("1");
        let correct = Ok(((Lit(Int(1)), None), tspan(0, 0, 0, 1)));
        let test = p.atom();
        assert_eq!(test, correct);
        assert_eq!(p.ts.next(), None);
    }

    #[test]
    fn parse_litint_exp() {
        use BareExp::*;
        use LitVal::*;
        let mut p = Parser::new("1");
        let correct = Ok(((Lit(Int(1)), None), tspan(0, 0, 0, 1)));
        let test = p.exp();
        assert_eq!(test, correct);
        assert_eq!(p.ts.next(), None);
    }

    #[test]
    fn parse_litpluslit() {
        use crate::ast::BareExp::*;
        use crate::ast::BareOp::*;
        use crate::ast::LitVal::*;
        let mut p = Parser::new("3+2");
        let correct = Ok((
            (
                BinOp(
                    (Plus, tspan(0, 0, 1, 2)),
                    Box::new(((Lit(Int(3)), None), tspan(0, 0, 0, 1))),
                    Box::new(((Lit(Int(2)), None), tspan(0, 0, 2, 3))),
                ),
                None,
            ),
            tspan(0, 0, 0, 3),
        ));
        let test = p.exp();
        assert_eq!(test, correct);
        assert_eq!(p.ts.next(), None);
    }

    #[test]
    fn paren_exp() {
        use BareExp::*;
        use LitVal::*;
        let mut p = Parser::new("(1)");
        let correct = Ok(((Lit(Int(1)), None), tspan(0, 0, 0, 3)));
        let test = p.exp();
        assert_eq!(test, correct);
        assert_eq!(p.ts.next(), None);
    }

    #[test]
    fn plus_after_mul() {
        use crate::ast::BareExp::*;
        use crate::ast::BareOp::*;
        use crate::ast::LitVal::*;
        let leftplus = Parser::new("1+2*3").exp();
        let rightplus = Parser::new("2*3+1").exp();
        match leftplus.unwrap() {
            ((BinOp((Plus, _), lhs, rhs), None), _) => match *rhs {
                ((BinOp((Mul, _), rlhs, rrhs), None), _) => {
                    assert_eq!(lhs.0, (Lit(Int(1)), None));
                    assert_eq!(rlhs.0, (Lit(Int(2)), None));
                    assert_eq!(rrhs.0, (Lit(Int(3)), None));
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
        match rightplus.unwrap() {
            ((BinOp((Plus, _), lhs, rhs), None), _) => match *lhs {
                ((BinOp((Mul, _), llhs, lrhs), None), _) => {
                    assert_eq!(rhs.0, (Lit(Int(1)), None));
                    assert_eq!(llhs.0, (Lit(Int(2)), None));
                    assert_eq!(lrhs.0, (Lit(Int(3)), None));
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn arith_parens() {
        use crate::ast::BareExp::*;
        use crate::ast::BareOp::*;
        use crate::ast::LitVal::*;
        let rightplus = Parser::new("1*(2+3)").exp();
        let leftplus = Parser::new("(2+3)*1").exp();
        match rightplus.unwrap() {
            ((BinOp((Mul, _), lhs, rhs), None), _) => match *rhs {
                ((BinOp((Plus, _), rlhs, rrhs), None), _) => {
                    assert_eq!(lhs.0, (Lit(Int(1)), None));
                    assert_eq!(rlhs.0, (Lit(Int(2)), None));
                    assert_eq!(rrhs.0, (Lit(Int(3)), None));
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
        match leftplus.unwrap() {
            ((BinOp((Mul, _), lhs, rhs), None), _) => match *lhs {
                ((BinOp((Plus, _), llhs, lrhs), None), _) => {
                    assert_eq!(rhs.0, (Lit(Int(1)), None));
                    assert_eq!(llhs.0, (Lit(Int(2)), None));
                    assert_eq!(lrhs.0, (Lit(Int(3)), None));
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn call_noargs() {
        use crate::ast::BareExp::*;
        let mut p = Parser::new("foo()");
        let test = p.exp();
        let foo = BareId(p.ts.names.iter().position(|&e| e == "foo").unwrap() as u32);
        let correct = Ok((
            (Call((foo, tspan(0, 0, 0, 3)), Vec::new()), None),
            tspan(0, 0, 0, 5),
        ));
        assert_eq!(test, correct);
        assert_eq!(p.ts.next(), None);
    }

    #[test]
    fn tuple_newline_exp() {
        use BareExp::*;
        use LitVal::*;
        let mut p = Parser::new("(1,\n2)");
        let correct = Ok((
            (
                Tuple(vec![
                    ((Lit(Int(1)), None), tspan(0, 0, 1, 2)),
                    ((Lit(Int(2)), None), tspan(1, 1, 0, 1)),
                ]),
                None,
            ),
            tspan(0, 1, 0, 2),
        ));
        let test = p.exp();
        assert_eq!(test, correct);
        assert_eq!(p.ts.next(), None);
    }
}
