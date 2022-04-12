use core::fmt;

use crate::ast::{BType, BareOp, BareSelector, LitVal, Spanned};

impl fmt::Display for BareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BareOp::And => "&&",
                BareOp::Or => "||",
                BareOp::Not => "!",
                BareOp::Lt => "<",
                BareOp::Leq => "<=",
                BareOp::Gt => ">",
                BareOp::Geq => ">=",
                BareOp::Eq => "==",
                BareOp::Neq => "!=",
                BareOp::Plus => "+",
                BareOp::Minus => "-",
                BareOp::Mul => "*",
                BareOp::Div => "/",
                BareOp::Neg => "-",
                BareOp::Cons => ":",
            }
        )
    }
}

impl fmt::Display for LitVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitVal::Int(v) => write!(f, "{v}"),
            LitVal::Char(c) => write!(f, "'{c}"),
            LitVal::Bool(b) => write!(f, "{b}"),
            LitVal::Nil => write!(f, "[]"),
        }
    }
}

impl fmt::Display for BType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BType::IntT => "Int",
                BType::BoolT => "Bool",
                BType::CharT => "Char",
                BType::UnitT => "Void",
            }
        )
    }
}

impl fmt::Display for BareSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BareSelector::Hd => "hd",
                BareSelector::Tl => "tl",
                BareSelector::Fst => "fst",
                BareSelector::Snd => "snd",
            }
        )
    }
}

trait InternedShow {
    fn show(&self, names: &Vec<&str>) -> String;
}

impl<T> InternedShow for Spanned<T>
where
    T: InternedShow,
{
    fn show(&self, names: &Vec<&str>) -> String {
        self.0.show(names)
    }
}

impl InternedShow for BareOp {
    fn show(&self, _: &Vec<&str>) -> String {
        format!("{self}")
    }
}
impl InternedShow for BareSelector {
    fn show(&self, _: &Vec<&str>) -> String {
        format!("{self}")
    }
}
impl InternedShow for BType {
    fn show(&self, _: &Vec<&str>) -> String {
        format!("{self}")
    }
}
impl InternedShow for LitVal {
    fn show(&self, _: &Vec<&str>) -> String {
        format!("{self}")
    }
}
