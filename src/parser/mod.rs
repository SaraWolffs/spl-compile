mod lex;
mod shunting_yard;
mod tok;

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
struct ParseError(String, Option<tok::Loc>);

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError(err.0, Some(err.1))
    }
}

type ParseResult<T> = Result<T, ParseError>;

fn opthull(lhs: Option<Span>, rhs: Option<Span>) -> Option<Span> {
    Some(Span::hull(lhs?, rhs?))
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
    fn new(source: &'s str) -> Self {
        Self {
            ts: Lex::lex(source),
        }
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

    fn expect(&mut self, tok: Token, expected: &str) -> ParseResult<tok::LocTok> {
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

    fn many<T>(&mut self, one: fn(&mut Self) -> ParseResult<Option<T>>) -> ParseResult<Vec<T>> {
        let mut acc = Vec::new();
        loop {
            let next = one(self)?;
            if let Some(parsed) = next {
                acc.push(parsed);
            } else {
                break Ok(acc);
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
        use BareType::Typename;
        match self.peektok()? {
            None => eof("'(' or identifier".to_owned()),
            Some(Marker(ParenOpen)) => self.fun_def(id),
            Some(IdTok(_)) => {
                let (varid, exp, loc2) = self.var_init()?;
                Ok((
                    Global((Some((Typename(id), id.1)), varid, exp)),
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
        let _: LocTok = self.expect(Marker(Assign), "'='")?;
        let exp = self.exp()?;
        let (_, loc) = self.expect(Marker(Semicolon), "';'")?;
        Ok((id, exp, loc))
    }

    fn fun_def(&mut self, id: Id) -> ParseResult<Decl> {
        let (args, _) = self.tuplish(Parser::parse_id)?;
        let ftype = if let Some(Marker(TypeColon)) = self.peektok()? {
            let _: LocTok = self.expect(Marker(TypeColon), "'::'").unwrap();
            Some(self.fun_type()?)
        } else {
            None
        };
        let (body, bspan) = self.compound()?;
        Ok((Fun(id, args, ftype, body), opthull(id.1, Some(bspan))))
    }

    fn fun_type(&mut self) -> ParseResult<FunType> {
        todo!();
    }

    fn typ(&mut self) -> ParseResult<Option<Type>> {
        todo!();
    }

    fn non_id_type(&mut self) -> ParseResult<Type> {
        todo!();
    }

    fn b_type(&mut self) -> ParseResult<BType> {
        todo!();
    }

    fn stmt(&mut self) -> ParseResult<Option<Stmt>> {
        todo!();
    }

    fn assign_or_call(&mut self, id: Id) -> ParseResult<Stmt> {
        todo!();
    }

    fn compound(&mut self) -> ParseResult<(Vec<Stmt>, Span)> {
        todo!();
    }

    fn selector(&mut self) -> ParseResult<Selector> {
        todo!();
    }

    fn field(&mut self) -> ParseResult<(Vec<Selector>, Span)> {
        todo!()
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
                    Ok(((Var(id, fld), None), opthull(id.1, Some(end))))
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

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
    use super::lex::*;
    use super::*;
    fn tspan(startline: u32, endline: u32, startcol: u16, endcol: u16) -> Option<Span> {
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
