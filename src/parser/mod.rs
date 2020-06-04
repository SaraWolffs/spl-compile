mod lex;

use crate::ast::*;
use lex::Token::*;
use lex::Misc::*;
type TokStream<'s> = std::iter::Peekable<lex::Lex<'s>>;

macro_rules! unexpected {
    ( $found: expr, $loc : expr, $expected: expr ) => 
    {return Err((format!("Unexpected {:?} encountered while looking for {}",
            $found, $expected),Some($loc)))};
}

macro_rules! fail {
    ( $reason : expr, $loc : expr ) => {return Err(($reason.to_string(),Some($loc)))};
    ( $reason : expr ) => {return Err(($reason.to_string(),None))};
}

type ParseError = (String,Option<crate::ast::Loc>);

type ParseResult<T> = Result<T,ParseError>;


pub fn spl_parse(source: &str) -> ParseResult<SPL> {
    let mut tokstream = lex::Lex::lex(source).peekable();
    let mut ast = Vec::new();
    let ts = &mut tokstream;
    while let Some(Ok((tok,loc))) = ts.peek(){
        ast.push(match tok {
            Marker(Var) => {ts.next(); let (v,e) = var_init(ts)?; Decl::Var(None, v, e)},
            TypeTok(_) | Marker(ParenOpen) |  Marker(BrackOpen) => {
                let typ = non_id_type(ts)?;
                let (v,e) = var_init(ts)?;
                Decl::Var(Some(typ),v,e)
            },
            IdTok(i) => fun_or_named_type_var_decl(ts)?,
            x => unexpected!(x,*loc,
                "'var', 'Int', 'Bool', 'Char', '(', '[', or identifier")
        }
        );

    }
    match tokstream.next() {
        None => Ok(ast),
        Some(Err((msg,loc))) => fail!(msg,loc),
        Some(Ok(_)) => panic!("`Some(Ok(_))` after `while let Some(Ok(_))`"),
    }
}

fn var_init(ts: &mut TokStream) -> ParseResult<(Id,Exp)> {
    unimplemented!();
}

fn non_id_type(ts: &mut TokStream) -> ParseResult<Type> {
    unimplemented!();
}

fn fun_or_named_type_var_decl(ts: &mut TokStream) -> ParseResult<Decl> {
    match ts.next(){
        None => fail!("Ran out of tokens while parsing declaration with named type"),
        Some(Ok((tok,loc))) => unimplemented!(), // parse
        Some(Err(e)) => unimplemented!() //unexpected!(),
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
