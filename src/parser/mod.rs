mod lex;
#[macro_use]
mod tok;

use crate::ast::*;
use crate::ast::Selector;
use crate::ast::Span;
use lex::Lex;

pub use tok::Loc;
use tok::Token::*;
use tok::Misc::*;
use tok::Token::Lit as LitTok;
use tok::Token::Selector as SelectTok;


type TokStream<'s> = std::iter::Peekable<lex::Lex<'s>>;

pub struct Parser<'s> {
    ts : TokStream<'s>,
    lexer : lex::Lex<'s>,
}
    

macro_rules! unexpected {
    ( $found: expr, $loc : expr, $expected: expr ) => 
    {return Err((format!("Unexpected {:?} encountered while looking for {}",
            $found, $expected),Some($loc)))};
}

macro_rules! fail {
    ( $reason : expr, $loc : expr ) => {return Err(($reason.to_string(),Some($loc)))};
    ( $reason : expr ) => {return Err(($reason.to_string(),None))};
}

type ParseError = (String,Option<tok::Loc>);

type ParseResult<T> = Result<T,ParseError>;

fn opthull(lhs:Option<Span>,rhs:Option<Span>) -> Option<Span> {
    Some(Span::hull(lhs?,rhs?))
}

fn hull(lhs:Span,rhs:Span) -> Span {
    Span::hull(lhs,rhs)
}

impl<'s> Parser<'s> {
    fn nexttok(&mut self) -> Option<<Lex as Iterator>::Item> {
        self.ts.next()
    }

    fn peektok(&mut self) -> Option<&<Lex as Iterator>::Item> {
        self.ts.peek()
    }

    fn var_init(&mut self) -> ParseResult<(Id,Exp)> {
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

    fn field(&mut self) -> ParseResult<(Vec<Selector>,Span)> {
        todo!()
    }

    fn exp(&mut self) -> ParseResult<Exp> {
        self.shunting_yard()
    }

    fn atom(&mut self) -> ParseResult<Exp> {
        use BareExp::*;
        match(self.nexttok()) {
            None => fail!("EOF while looking for identifier, literal, or '('"),
            Some(Err((msg,loc))) => fail!(msg,loc),
            Some(Ok((tok,loc))) => match tok {
                IdTok(i) => self.field_or_call((i,Some(loc.into()))),
                LitTok(val) => Ok(((Lit(val), None), Some(loc.into()))),
                Marker(ParenOpen) => { 
                    let (coords, span) = self.tuplish(Self::exp)?;
                    if (coords.len() == 1) {
                        Ok((coords.into_iter().next().unwrap().0,Some(span)))
                    } else {
                        Ok(((Tuple(coords), None), Some(hull(loc.into(),span))))
                    }
                    }
                x => unexpected!(x,loc,"identifier, literal, or '('")
            }
        }
    }


    fn field_or_call(&mut self, id: Id) -> ParseResult<Exp> {
        use BareExp::*;
        match(self.peektok()) {
            None => Ok(((Var(id,Vec::new()),None), id.1)),
            Some(Err((msg,loc))) => fail!(msg,*loc),
            Some(Ok((tok,loc))) => match tok {
                Marker(ParenOpen) => {
                    self.nexttok();
                    let (args, end) = self.tuplish(Self::exp)?;
                    Ok(((Call(id,args), None), opthull(id.1,Some(end))))
                }
                x => {
                    let (fld, end) = self.field()?;
                    Ok(((Var(id,fld),None), opthull(id.1,Some(end))))
                }
            }
        }
    }

    fn tuplish<T>(&mut self, single: fn(&mut Self) -> ParseResult<T>) -> ParseResult<(Vec<T>,Span)> {
        todo!()
    }

    fn shunting_yard(&mut self) -> ParseResult<Exp> {
        let mut opstack = Vec::<tok::Token>::with_capacity(8);
        let mut outstack = Vec::<Exp>::with_capacity(8);
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::lex::*;
    #[test]
    fn test_sanity() {
        assert!(true);
    }
    
}
