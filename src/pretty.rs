use core::fmt;

use crate::ast::*;

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

pub struct ShowConfig<'s> {
    names: Vec<&'s str>,
    lntab: String,
    tabwidth: usize,
}

impl<'s> ShowConfig<'s> {
    pub fn new(names: Vec<&'s str>) -> Self {
        let tabwidth = 4;
        Self {
            names: names,
            lntab: itertools::join(
                std::iter::once("\n").chain(std::iter::once(" ").cycle().take(tabwidth)),
                "",
            ),
            tabwidth: tabwidth,
        }
    }
}

pub trait InternedShow {
    fn show(&self, cfg: &ShowConfig) -> String;
}

impl<T> InternedShow for Spanned<T>
where
    T: InternedShow,
{
    fn show(&self, cfg: &ShowConfig) -> String {
        self.0.show(cfg)
    }
}

impl<T> InternedShow for Typed<T>
where
    T: InternedShow,
{
    fn show(&self, cfg: &ShowConfig) -> String {
        self.0.show(cfg)
    }
}

impl InternedShow for Vec<Stmt> {
    fn show(&self, cfg: &ShowConfig) -> String {
        itertools::join(self.iter().map(|x| x.show(cfg)), "\n")
    }
}

impl InternedShow for BareOp {
    fn show(&self, _: &ShowConfig) -> String {
        format!("{self}")
    }
}
impl InternedShow for BareSelector {
    fn show(&self, _: &ShowConfig) -> String {
        format!("{self}")
    }
}
impl InternedShow for BType {
    fn show(&self, _: &ShowConfig) -> String {
        format!("{self}")
    }
}
impl InternedShow for LitVal {
    fn show(&self, _: &ShowConfig) -> String {
        format!("{self}")
    }
}
impl InternedShow for BareId {
    fn show(&self, cfg: &ShowConfig) -> String {
        cfg.names[self.0 as usize].to_owned()
    }
}

impl InternedShow for BareType {
    fn show(&self, cfg: &ShowConfig) -> String {
        match self {
            BareType::Lit(t) => t.show(cfg),
            BareType::Typename(id) => id.show(cfg),
            BareType::Tuple(v) => {
                format!("({})", itertools::join(v.iter().map(|x| x.show(cfg)), ", "))
            }
            BareType::List(t) => format!("[{}]", t.show(cfg)),
            BareType::Typevar(n) => format!("t{n}"),
        }
    }
}

impl InternedShow for BareExp {
    fn show(&self, cfg: &ShowConfig) -> String {
        match self {
            BareExp::Var(id, field) => format!(
                "{}{}",
                id.show(cfg),
                itertools::join(field.iter().map(|x| format!(".{}", x.show(cfg))), "")
            ),
            BareExp::Call(id, args) => {
                format!(
                    "{}({})",
                    id.show(cfg),
                    itertools::join(args.iter().map(|x| x.show(cfg)), ", ")
                )
            }
            BareExp::Lit(val) => val.show(cfg),
            BareExp::Tuple(v) => {
                format!("({})", itertools::join(v.iter().map(|x| x.show(cfg)), ", "))
            }
            BareExp::BinOp(o, l, r) => smart_parenthesise(o, l, r, cfg),
            BareExp::UnOp(o, e) => smart_paren_un(o, e, cfg),
        }
    }
}

impl InternedShow for VarDecl {
    fn show(&self, cfg: &ShowConfig) -> String {
        let (t, id, exp) = self;
        format!(
            "{} {} = {};",
            if let Some(t) = t {
                t.show(cfg)
            } else {
                "var".to_owned()
            },
            id.show(cfg),
            exp.show(cfg)
        )
    }
}

impl InternedShow for BareStmt {
    fn show(&self, cfg: &ShowConfig) -> String {
        match self {
            BareStmt::ITE(cond, then, els) => {
                if els.is_empty() {
                    format!(
                        "if ({}) {{\n{}\n}}",
                        cond.show(cfg),
                        indent(then.show(cfg), cfg)
                    )
                } else {
                    format!(
                        "if ({}) {{\n{}\n}} else {{\n{}\n}}",
                        cond.show(cfg),
                        indent(then.show(cfg), cfg),
                        indent(els.show(cfg), cfg)
                    )
                }
            }
            BareStmt::While(cond, then) => format!(
                "while ({}) {{\n{}\n}}",
                cond.show(cfg),
                indent(then.show(cfg), cfg)
            ),
            BareStmt::Assign(id, field, exp) => format!(
                "{}{} = {};",
                id.show(cfg),
                itertools::join(field.iter().map(|x| format!(".{}", x.show(cfg))), ""),
                exp.show(cfg),
            ),
            BareStmt::Call(id, args) => format!(
                "{}({});",
                id.show(cfg),
                itertools::join(args.iter().map(|x| x.show(cfg)), ", ")
            ),
            BareStmt::Ret(val) => {
                if let Some(exp) = val {
                    format!("return {};", exp.show(cfg))
                } else {
                    "return;".to_owned()
                }
            }
            BareStmt::Local(vdec) => vdec.show(cfg),
        }
    }
}

impl InternedShow for BareDecl {
    fn show(&self, cfg: &ShowConfig) -> String {
        match self {
            BareDecl::Global(vdec) => vdec.show(cfg),
            BareDecl::Fun(id, args, ftype, body) => format!(
                "{}({}){} {{\n{}\n}}",
                id.show(cfg),
                itertools::join(args.iter().map(|x| x.show(cfg)), ", "),
                if let Some(ftype) = ftype {
                    ftype.show(cfg)
                } else {
                    "".to_owned()
                },
                indent(body.show(cfg), cfg)
            ),
        }
    }
}

impl InternedShow for BareFunType {
    fn show(&self, cfg: &ShowConfig) -> String {
        let (argtypes, rettype) = self;
        format!(
            " :: {} -> {}",
            itertools::join(argtypes.iter().map(|x| x.show(cfg)), " "),
            rettype.show(cfg)
        )
    }
}

impl InternedShow for SPL {
    fn show(&self, cfg: &ShowConfig) -> String {
        itertools::join(self.iter().map(|x| x.show(cfg)), "\n\n")
    }
}

fn smart_paren_un(o: &Op, e: &Exp, cfg: &ShowConfig) -> String {
    // TODO: make this not emit superfluous parentheses
    format!("{}({})", o.show(cfg), e.show(cfg))
}

fn smart_parenthesise(o: &Op, l: &Exp, r: &Exp, cfg: &ShowConfig) -> String {
    // TODO: make this not emit superfluous parentheses
    format!("({}) {} ({})", l.show(cfg), o.show(cfg), r.show(cfg))
}

fn indent(s: String, cfg: &ShowConfig) -> String {
    // itertools::join(std::iter::once::<&str>(&cfg.tab).chain(s.lines()), &cfg.tab)
    format!(
        "{}{}",
        &cfg.lntab[1..],
        (str::replace(&s, "\n", &cfg.lntab))
    )
}

// fn commasep<T>(v: T, cfg: &ShowConfig) -> String
// where
//     T: Iterator,
//     T::Item: InternedShow,
// {
//     v.map(|x| x.show(cfg)).collect::<Vec<_>>().join(", ")
// }
