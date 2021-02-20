mod lex;
mod shunting_yard;
mod tok;

use crate::ast::Selector;
use crate::ast::Span;
use crate::ast::*;
use lex::Lex;

pub use tok::Loc;
use tok::Misc::*;
use tok::Token;
use tok::Token::Lit as LitTok;
use tok::Token::Selector as SelectTok;
use tok::Token::*;

type TokStream<'s> = lex::Lex<'s>;

pub struct Parser<'s> {
    ts: TokStream<'s>,
}

macro_rules! fail {
    ( $reason : expr, $loc : expr ) => {
        return Err(($reason.to_string(), Some($loc)));
    };
    ( $reason : expr ) => {
        return Err(($reason.to_string(), None));
    };
}

macro_rules! ipe {
    ( $msg : expr ) => {
        panic!(format!("Internal parser error: {}", $msg))
    };
}

type ParseError = (String, Option<tok::Loc>);

type ParseResult<T> = Result<T, ParseError>;

fn opthull(lhs: Option<Span>, rhs: Option<Span>) -> Option<Span> {
    Some(Span::hull(lhs?, rhs?))
}

fn hull(lhs: Span, rhs: Span) -> Span {
    Span::hull(lhs, rhs)
}

fn eof<T>(expected: String) -> ParseResult<T> {
    Err((format!("EOF while looking for {}", expected), None))
}

fn unexpected<T>(found: tok::LocTok, expected: String) -> ParseResult<T> {
    Err((
        format!(
            "Unexpected {:?} encountered while looking for {}",
            found.0, expected
        ),
        Some(found.1),
    ))
}

fn lexfail<T>(err: lex::LexError) -> ParseResult<T> {
    Err((err.0,Some(err.1)))
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

    fn peektok(&mut self) -> Option<&<Lex as Iterator>::Item> {
        self.ts.peek()
    }

    fn trytok(&mut self) -> ParseResult<Option<tok::LocTok>> {
        self.ts
            .next()
            .transpose()
            .map_err(|(msg, loc)| (msg, Some(loc)))
    }

    fn unpeektok(&mut self, val: tok::LocTok) -> ParseResult<&tok::LocTok> {
        self.ts
            .unpeek(val)
            .map_err(|v| ipe!("Attempted lookahead beyond 1"))
    }

    fn expect(&mut self, tok: Token) -> ParseResult<tok::LocTok> {
        match self.peektok() {
            None => eof(format!("{:?}",tok)),
            Some(Err((msg, loc))) => fail!(msg, *loc),
            Some(&Ok((found, loc))) => {
                if found == tok {
                    self.nexttok();
                    Ok((found, loc))
                } else {
                    unexpected((found, loc), format!("{:?}",tok))
                }
            }
        }
    }

    fn var_init(&mut self) -> ParseResult<(Id, Exp)> {
        todo!();
    }

    fn non_id_type(&mut self) -> ParseResult<Type> {
        todo!();
    }

    /*
    fn fun_or_named_type_var_decl(ts: &mut TokStream) -> ParseResult<Decl> {
        match ts.next(){
            None => fail!("Ran out of tokens while parsing declaration with named type"),
            Some(Ok((tok,loc))) => unimplemented!(), // parse
            Some(Err(e)) => unimplemented!() //unexpected!(),
        }
    }
    */

    fn field(&mut self) -> ParseResult<(Vec<Selector>, Span)> {
        todo!()
    }

    fn exp(&mut self) -> ParseResult<Exp> {
        shunting_yard::ShuntingYard::parse_exp(self)
    }

    fn atom(&mut self) -> ParseResult<Exp> {
        use BareExp::*;
        match self.peektok() {
            None => fail!("EOF while looking for identifier, literal, or '('"),
            Some(Err((msg, loc))) => fail!(msg, *loc),
            Some(&Ok((tok, loc))) => match tok {
                IdTok(i) => {
                    self.nexttok();
                    self.field_or_call((i, Some(loc.into())))
                }
                LitTok(val) => {
                    self.nexttok();
                    Ok(((Lit(val), None), Some(loc.into())))
                }
                Marker(ParenOpen) => {
                    let (coords, span) = self.tuplish(Self::exp)?;
                    if coords.len() == 1 {
                        Ok((coords.into_iter().next().unwrap().0, Some(span)))
                    } else {
                        Ok(((Tuple(coords), None), Some(span)))
                    }
                }
                x => unexpected((x, loc), "identifier, literal, or '('".to_string()),
            },
        }
    }

    fn field_or_call(&mut self, id: Id) -> ParseResult<Exp> {
        use BareExp::*;
        match self.peektok() {
            None => Ok(((Var(id, Vec::new()), None), id.1)),
            Some(Err((msg, loc))) => fail!(msg, *loc),
            Some(Ok((tok, _))) => match tok {
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
        if let Some(&Ok((Marker(ParenClose), loc))) = self.peektok() {
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
                    break unexpected(loctok, "',' or ')'".to_string())
                }
            }
        }
    }
}

#[cfg(test)]
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
        let foo = p.ts.names.iter().position(|&e| e == "foo").unwrap() as u32;
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
