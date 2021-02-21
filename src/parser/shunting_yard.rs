use super::*;

macro_rules! fail {
    ( $reason : expr, $loc : expr ) => {
        return Err(ParseError($reason.to_string(), Some($loc)));
    };
    ( $reason : expr ) => {
        return Err(ParseError($reason.to_string(), None));
    };
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ShuntState {
    Expression,
    Operator,
    Done,
}

pub(super) struct ShuntingYard<'s, 'p> {
    state: ShuntState,
    parser: &'p mut Parser<'s>,
    opstack: Vec<tok::LocTok>, // Note: this is typed too loosely, to allow for
    // possible rewrites which have the shunting yard deal with tuples
    outstack: Vec<Exp>,
    lastloc: Option<tok::Loc>,
    lasttok: Option<tok::Token>,
}

impl<'s, 'p> ShuntingYard<'s, 'p> {
    pub(super) fn parse_exp(parser: &'p mut Parser<'s>) -> ParseResult<Exp> {
        Self::new(parser).run()
    }

    fn new(parser: &'p mut Parser<'s>) -> Self {
        ShuntingYard {
            state: ShuntState::Expression,
            parser,
            opstack: Vec::new(),
            outstack: Vec::with_capacity(1),
            lastloc: None,
            lasttok: None,
        }
    }

    fn step(&mut self) -> ParseResult<()> {
        use ShuntState::*;
        match self.state {
            Expression => self.shunt_atom()?,
            Operator => self.shunt_binop()?,
            Done => self.oppop()?,
        }
        Ok(())
    }

    fn run(&mut self) -> ParseResult<Exp> {
        while self.state != ShuntState::Done {
            self.step()?;
        }
        loop {
            if self.outstack.len() == 1 && self.opstack.is_empty() {
                break Ok(self.outstack.pop().unwrap());
            } else {
                self.step()?;
            }
        }
    }

    fn shunt_atom(&mut self) -> ParseResult<()> {
        use crate::ast::BareOp::*;
        use ShuntState::*;
        assert_eq!(self.state, Expression);
        match self.parser.peektok()? {
            Some(&(Op(Minus), loc)) => {
                self.opstack.push((Op(Neg), loc));
                self.parser.nexttok();
                self.state = Expression;
            }
            Some(&(Op(Not), loc)) => {
                self.opstack.push((Op(Not), loc));
                self.parser.nexttok();
                self.state = Expression;
            }
            _ => {
                self.outstack.push(self.parser.atom()?);
                self.state = Operator;
            }
        }
        Ok(())
    }

    fn shunt_binop(&mut self) -> ParseResult<()> {
        use crate::ast::BareOp::*;
        use ShuntState::*;
        assert_eq!(self.state, Operator);
        match self.parser.peektok()? {
            None => {
                self.lasttok = None;
                self.state = Done;
            }
            Some(&(Op(Not), loc)) => fail!("Expected binary operator, found '!'", loc),
            Some(&(Op(op), loc)) => {
                self.oppush(op, loc)?;
                self.parser.nexttok();
                self.state = Expression;
            }
            Some(&(tok, loc)) => {
                self.lastloc = Some(loc);
                self.lasttok = Some(tok);
                self.state = Done;
            }
        }
        Ok(())
    }

    fn exppop(&mut self) -> ParseResult<Exp> {
        self.outstack.pop().ok_or_else(|| {
            ParseError(
                "Internal parser error: Popped from empty expression stack".to_string(),
                self.lastloc,
            )
        })
    }

    fn oppeek(&mut self) -> Option<tok::LocTok> {
        self.opstack.iter().rev().next().map(ToOwned::to_owned)
    }

    fn can_push(&mut self, op: crate::ast::BareOp, loc: Loc) -> ParseResult<bool> {
        Ok(match self.oppeek() {
            None => true,
            Some((Op(stacked), _)) => op.right_precedes(stacked),
            _ => {
                return Err(ParseError(
                    "Internal parser error: non-operator on operator stack".to_string(),
                    self.lastloc.or(Some(loc)),
                ))
            }
        })
    }

    fn opapply(&mut self, op: crate::ast::BareOp, loc: Loc) -> ParseResult<()> {
        use crate::ast::BareExp::*;
        if op.is_unary() {
            let arg1 = self.exppop()?;
            let span = opthull(Some(loc.into()), arg1.1);
            let result = ((UnOp((op, Some(loc.into())), Box::new(arg1)), None), span);
            self.outstack.push(result)
        } else {
            let arg2 = self.exppop()?; // remember, the stack is back-to-front
            let arg1 = self.exppop()?;
            let span = opthull(arg1.1, arg2.1);
            let result = (
                (
                    BinOp((op, Some(loc.into())), Box::new(arg1), Box::new(arg2)),
                    None,
                ),
                span,
            );
            self.outstack.push(result)
        }
        Ok(())
    }

    fn oppop(&mut self) -> ParseResult<()> {
        if let (Op(popped), loc) = self.opstack.pop().ok_or_else(|| {
            ParseError(
                "Internal parser error: Popped from empty operator stack".to_string(),
                self.lastloc,
            )
        })? {
            self.opapply(popped, loc)
        } else {
            Err(ParseError(
                "Internal parser error: non-operator on operator stack".to_string(),
                self.lastloc,
            ))
        }
    }

    fn oppush(&mut self, op: crate::ast::BareOp, loc: Loc) -> ParseResult<()> {
        while !self.can_push(op, loc)? {
            self.oppop()?
        }
        self.opstack.push((Op(op), loc));
        Ok(())
    }
}
