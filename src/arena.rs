use std::fmt::{Display, Formatter};

use bumpalo::Bump;
use yap::{one_of, types::WithContext, IntoTokens, Tokens};

use crate::AbstrMode;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Label<'a>(&'a str);

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type<'a> {
    Top,
    Int,
    Arr(&'a Type<'a>, &'a Type<'a>),
    And(&'a Type<'a>, &'a Type<'a>),
    Rcd(Label<'a>, &'a Type<'a>),
}

#[derive(Debug, Clone, Copy)]
pub enum Expr<'a> {
    Top,
    Int(i64),
    Query,
    Abstr(AbstrMode, &'a Expr<'a>),
    Close {
        env: &'a Expr<'a>,
        expr: &'a Expr<'a>,
    },
    Apply(&'a Expr<'a>, &'a Expr<'a>),
    Merge(&'a Expr<'a>, &'a Expr<'a>),
    Annot(&'a Expr<'a>, &'a Type<'a>),
    Label(Label<'a>, &'a Expr<'a>),
    Fetch(&'a Expr<'a>, Label<'a>),
}

fn maybe_parse_expr_binop<'a>(
    lhs: &'a Expr<'a>,
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Expr<'a>> {
    let bump: &Bump = *t.context();
    fn can_begin_expr(c: char) -> bool {
        c.is_ascii_digit() || c == '{' || c == '?' || c == '^'
    }
    match t.peek() {
        None => Some(lhs),
        Some(x) => match x {
            '>' => {
                t.next();
                let rhs = parse_expr(t)?;
                Some(&*bump.alloc(Expr::Close {
                    env: lhs,
                    expr: rhs,
                }))
            }
            ',' => {
                t.next();
                let rhs = parse_expr(t)?;
                Some(&*bump.alloc(Expr::Merge(lhs, rhs)))
            }
            ':' => {
                t.next();
                let ty = parse_type(t)?;
                Some(&*bump.alloc(Expr::Annot(lhs, ty)))
            }
            '.' => {
                t.next();
                let l = parse_label(t)?;
                Some(&*bump.alloc(Expr::Fetch(lhs, l)))
            }
            x if can_begin_expr(x) => {
                let rhs = parse_expr(t)?;
                Some(&*bump.alloc(Expr::Apply(lhs, rhs)))
            }
            _ => Some(lhs),
        },
    }
}

pub fn parse_expr<'a>(
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Expr<'a>> {
    let e = one_of!(t;
        parse_int(t),
        parse_top(t),
        parse_query(t),
        parse_braced_expr(t),
    )?;
    // println!("parsed expr: {:?}", e);
    maybe_parse_expr_binop(e, t)
}

fn parse_braced_expr<'a>(
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Expr<'a>> {
    let bump = *t.context();
    t.token('{').then_some(())?;
    match t.peek()? {
        x if x.is_ascii_alphabetic() => {
            let l = parse_label(t)?;
            t.token('=').then_some(())?;
            let e = parse_expr(t)?;
            t.token('}').then_some(())?;
            Some(&*bump.alloc(Expr::Label(l, e)))
        }
        _ => {
            let e = parse_expr(t)?;
            t.token('}').then_some(())?;
            let mode = one_of!(t;
                t.token('@').then(|| AbstrMode::AbstrGeneral),
                t.token('*').then(|| AbstrMode::AbstrLabHide),
            )?;
            Some(&*bump.alloc(Expr::Abstr(mode, e)))
        }
    }
}

fn parse_top<'a>(t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>) -> Option<&'a Expr<'a>> {
    let bump: &Bump = *t.context();
    t.token('^').then(|| &*bump.alloc(Expr::Top))
}

fn parse_int<'a>(t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>) -> Option<&'a Expr<'a>> {
    let bump: &Bump = *t.context();
    let mut i = 0;
    let mut found_digits = false;
    loop {
        match t.peek() {
            Some(c) if c.is_digit(10) => {
                found_digits = true;
                i *= 10;
                i += c.to_digit(10).unwrap() as i64;
                t.next();
            }
            _ => break,
        }
    }
    if found_digits {
        //println!("int: {:?}", i);
        Some(&*bump.alloc(Expr::Int(i)))
    } else {
        None
    }
}

fn parse_label<'a>(t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>) -> Option<Label<'a>> {
    let bump: &Bump = *t.context();
    let s = bumpalo::collections::String::from_iter_in(
        t.take_while(|c| c.is_ascii_alphabetic()).as_iter(),
        bump,
    );
    if s.len() > 0 {
        // println!("label: {:?}", s);
        Some(Label(s.into_bump_str()))
    } else {
        None
    }
}

fn parse_query<'a>(
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Expr<'a>> {
    let bump: &Bump = *t.context();
    t.token('?').then(|| &*bump.alloc(Expr::Query))
}

fn parse_type<'a>(t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>) -> Option<&'a Type<'a>> {
    let ty = one_of!(t;
        parse_top_type(t),
        parse_int_type(t),
        parse_rcd_type(t),
    )?;
    maybe_parse_ty_binop(ty, t)
}

fn parse_top_type<'a>(
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Type<'a>> {
    let bump: &Bump = *t.context();
    t.token('^').then(|| &*bump.alloc(Type::Top))
}

fn parse_int_type<'a>(
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Type<'a>> {
    let bump: &Bump = *t.context();
    t.tokens("Int".chars()).then(|| &*bump.alloc(Type::Int))
}

fn maybe_parse_ty_binop<'a>(
    lhs: &'a Type<'a>,
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Type<'a>> {
    let bump: &Bump = *t.context();
    match t.peek() {
        None => Some(lhs),
        Some(x) => match x {
            '&' => {
                t.next();
                let rhs = parse_type(t)?;
                Some(&*bump.alloc(Type::And(lhs, rhs)))
            }
            '-' => {
                t.next();
                t.token('>').then_some(())?;
                let rhs = parse_type(t)?;
                Some(&*bump.alloc(Type::Arr(lhs, rhs)))
            }
            _ => Some(lhs),
        },
    }
}

fn parse_rcd_type<'a>(
    t: &mut WithContext<impl Tokens<Item = char>, &'a Bump>,
) -> Option<&'a Type<'a>> {
    let bump: &Bump = *t.context();
    t.token('{').then_some(())?;
    let l = parse_label(t)?;
    t.token(':').then_some(())?;
    let ty = parse_type(t)?;
    t.token('}').then_some(())?;
    Some(&*bump.alloc(Type::Rcd(l, ty)))
}

impl Display for Label<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Top => write!(f, "^"),
            Type::Int => write!(f, "Int"),
            Type::Arr(ty1, ty2) => write!(f, "{}->{}", ty1, ty2),
            Type::And(ty1, ty2) => write!(f, "{}&{}", ty1, ty2),
            Type::Rcd(l, ty) => write!(f, "{{{}:{}}}", l, ty),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Top => write!(f, "^"),
            Expr::Int(i) => write!(f, "{}", i),
            Expr::Query => write!(f, "?"),
            Expr::Abstr(mode, e) => write!(f, "{{{}}}{}", e, mode),
            Expr::Close { env, expr } => write!(f, "{}>{}", env, expr),
            Expr::Apply(e1, e2) => write!(f, "{}{}", e1, e2),
            Expr::Merge(e1, e2) => write!(f, "{},{}", e1, e2),
            Expr::Annot(e, ty) => write!(f, "{}:{}", e, ty),
            Expr::Label(l, e) => write!(f, "{{{}={}}}", l, e),
            Expr::Fetch(e, l) => write!(f, "{}.{}", e, l),
        }
    }
}

impl Display for AbstrMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AbstrMode::AbstrGeneral => write!(f, "@"),
            AbstrMode::AbstrLabHide => write!(f, "*"),
        }
    }
}

#[test]
fn test_parse() {
    let bump = Bump::new();

    fn test_round_trip<'a>(bump: &'a Bump, s: &'a str) {
        println!("testing: {}", s);
        let mut t = s.into_tokens().with_context(bump);
        let e = parse_expr(&mut t).unwrap();
        println!("parsed: {:?}", e);
        println!("format: {}", e);
        assert_eq!(s, format!("{}", e));
    }

    for s in ["1", "?", "?.x", "{x=1}", "{x=1}.x"] {
        test_round_trip(&bump, s);
    }
}
