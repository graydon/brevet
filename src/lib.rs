use arbitrary::Arbitrary;
use std::sync::Arc;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Arbitrary)]
pub struct Label(Arc<String>);

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Arbitrary)]
pub enum Type {
    Top,
    Int,
    Arr(Arc<Type>, Arc<Type>),
    And(Arc<Type>, Arc<Type>),
    Rcd(Label, Arc<Type>),
}

impl Type {
    pub fn is_arr(&self) -> bool {
        if let Type::Arr(_, _) = self {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Arbitrary)]
pub enum AbstrMode {
    AbstrLabHide,
    AbstrGeneral,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Arbitrary)]
pub enum Expr {
    Top,
    Int(i64),
    Query,
    Abstr(AbstrMode, Arc<Expr>),
    Close { env: Arc<Expr>, expr: Arc<Expr> },
    Apply(Arc<Expr>, Arc<Expr>),
    Merge(Arc<Expr>, Arc<Expr>),
    Annot(Arc<Expr>, Arc<Type>),
    Label(Label, Arc<Expr>),
    Fetch(Arc<Expr>, Label),
}

impl Expr {
    pub fn merge(&self, other: &Expr) -> Expr {
        Expr::Merge(Arc::new(self.clone()), Arc::new(other.clone()))
    }
    pub fn is_abstr(&self) -> bool {
        if let Expr::Abstr(_, _) = self {
            true
        } else {
            false
        }
    }
    pub fn is_arr_annot_abstr(&self) -> bool {
        if let Expr::Annot(expr, ty) = self {
            ty.is_arr() && expr.is_abstr()
        } else {
            false
        }
    }
    pub fn is_closure(&self) -> bool {
        if let Expr::Close { env, expr } = self {
            env.is_val() && expr.is_arr_annot_abstr()
        } else {
            false
        }
    }
    pub fn is_closure_subtype_of(&self, c: &Type, d: &Type) -> bool {
        if self.is_closure() {
            let Expr::Close { env: _, expr } = self else {
                return false;
            };
            let Expr::Annot(_, ty) = &**expr else {
                return false;
            };
            let Type::Arr(a, b) = &**ty else { return false };
            subty(c, a) && subty(b, d)
        } else {
            false
        }
    }
    pub fn is_val(&self) -> bool {
        match self {
            Expr::Top | Expr::Int(_) => true,
            Expr::Merge(a, b) => a.is_val() && b.is_val(),
            Expr::Label(_, e) => e.is_val(),
            _ => self.is_closure(),
        }
    }
    pub fn is_rec(&self, lab: &Label) -> bool {
        if let Expr::Label(el, e) = self {
            el == lab && e.is_val()
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Arbitrary)]
pub enum Error {
    General(String),
    NonValEnv,
    NonValEval,
    StuckEval,
    StuckCast,
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Error::General(value)
    }
}

pub fn subty(a: &Type, b: &Type) -> bool {
    match (a, b) {
        // S-Z
        (Type::Int, Type::Int) => true,
        // S-TOP
        (_, Type::Top) => true,
        // S-ARR
        (Type::Arr(a1, a2), Type::Arr(b1, b2)) => subty(b1, a1) && subty(a2, b2),
        // S-ANDL
        // S-ANDR
        (Type::And(a1, a2), a3) => subty(a1, a3) || subty(a2, a3),
        // S-AND
        (a1, Type::And(a2, a3)) => subty(a1, a2) && subty(a1, a3),
        // S-RCD
        (Type::Rcd(l1, e1), Type::Rcd(l2, e2)) => l1 == l2 && subty(e1, e2),
        _ => false,
    }
}

pub fn ordinary(x: &Type) -> bool {
    match x {
        // O-INT
        Type::Int => true,
        // O-ARROW
        Type::Arr(_, b) => ordinary(b),
        // O-RCD
        Type::Rcd(_, b) => ordinary(b),
        _ => false,
    }
}

pub fn toplike(x: &Type) -> bool {
    match x {
        // TL-TOP
        Type::Top => true,
        // TL-ARR
        Type::And(a, b) => toplike(a) && toplike(b),
        // TL-ARR
        Type::Arr(_, b) => toplike(b),
        // TL-RCD
        Type::Rcd(_, b) => toplike(b),
        _ => false,
    }
}

// "Common Ordinary Super Types"
pub fn cost(a: &Type, b: &Type) -> bool {
    match (a, b) {
        // COST-INT
        (Type::Int, Type::Int) => true,
        // COST-ANDL
        // COST-ANDR
        (Type::And(a, b), c) => cost(a, c) || cost(b, c),
        // COST-RANDL
        // COST-RANDR
        (a, Type::And(b, c)) => cost(a, b) || cost(a, c),
        // COST-ARR
        (Type::Arr(_a, b), Type::Arr(_c, d)) => cost(b, d),
        // COST-RCD
        (Type::Rcd(_, x), Type::Rcd(_, y)) => cost(x, y),
        _ => false,
    }
}

// Algorithmic disjointness
pub fn disj(a: &Type, b: &Type) -> bool {
    !cost(a, b)
}

// Value generator for top-like types
pub fn valgen(ty: &Type) -> Result<Expr, Error> {
    match ty {
        Type::Top => Ok(Expr::Top),
        Type::Int => Err(Error::StuckCast),
        Type::Arr(_, b) => {
            let expr = Arc::new(valgen(b)?);
            let abstr_expr = Arc::new(Expr::Abstr(AbstrMode::AbstrGeneral, expr));
            let env = Arc::new(Expr::Top);
            let expr = Arc::new(Expr::Annot(abstr_expr, Arc::new(ty.clone())));
            Ok(Expr::Close { env, expr })
        }
        Type::And(a, b) => Ok(Expr::Merge(Arc::new(valgen(a)?), Arc::new(valgen(b)?))),
        Type::Rcd(lab, ty) => Ok(Expr::Label(lab.clone(), Arc::new(valgen(ty)?))),
    }
}

pub fn cast(val: &Expr, ty: &Type) -> Result<Expr, Error> {
    match (val, ty) {
        // CASTING-INT
        (Expr::Int(_), Type::Int) => Ok(val.clone()),
        // CASTING-TOP
        (_, Type::Top) => Ok(val.clone()),
        (Expr::Close { env, expr }, Type::Arr(c, d)) if val.is_closure_subtype_of(c, d) => {
            let Expr::Annot(e, cloty) = &**expr else {
                return Err(Error::StuckCast);
            };
            let Type::Arr(a, _) = &**cloty else {
                return Err(Error::StuckCast);
            };
            if toplike(d) {
                // CASTING-ARROWTL
                valgen(ty)
            } else {
                // CASTING-ARROW
                // TODO: modes
                let new_ty = Arc::new(Type::Arr(a.clone(), d.clone()));
                let new_annot_expr = Arc::new(Expr::Annot(e.clone(), new_ty));
                Ok(Expr::Close {
                    env: env.clone(),
                    expr: new_annot_expr,
                })
            }
        }
        // CASTING-MERGEVL and CASTING-MERGEVL
        (Expr::Merge(a, b), _) if ordinary(ty) => {
            if let Ok(c) = cast(a, ty) {
                Ok(c)
            } else if let Ok(c) = cast(b, ty) {
                Ok(c)
            } else {
                Err(Error::StuckCast)
            }
        }
        // CASTING-AND
        (_, Type::And(a, b)) => Ok(Expr::Merge(Arc::new(cast(val, a)?), Arc::new(cast(val, b)?))),
        // CASTING-RCD
        (Expr::Label(elab, e), Type::Rcd(tlab, t)) if elab == tlab => {
            Ok(Expr::Label(elab.clone(), Arc::new(cast(e, t)?)))
        }
        _ => Err(Error::StuckCast),
    }
}

pub fn step(env: &Expr, expr: &Expr) -> Result<Expr, Error> {
    if !env.is_val() {
        return Err(Error::NonValEnv);
    }
    if expr.is_val() {
        return Ok(expr.clone());
    }
    match expr {
        // STEP-CTX
        Expr::Query => Ok(env.clone()),

        // STEP-ANNOV
        Expr::Annot(v, t) if v.is_val() => cast(v, t),

        // STEP-MERGER
        Expr::Merge(v, e) if v.is_val() => {
            let env_with_v = env.merge(v);
            let ev = step(&env_with_v, e)?;
            Ok(v.merge(&ev))
        }

        // STEP-CLOSURE
        e if e.is_arr_annot_abstr() => Ok(Expr::Close {
            env: Arc::new(env.clone()),
            expr: Arc::new(e.clone()),
        }),

        // STEP-BOX
        Expr::Close {
            env: env1,
            expr: expr1,
        } if env1.is_val() && !expr.is_closure() => step(env1, expr1),

        // STEP-BOXV
        Expr::Close {
            env: env1,
            expr: expr1,
        } if env1.is_val() && expr1.is_val() => Ok((**expr1).clone()),

        // STEP-BETA
        Expr::Apply(clo, arg) if clo.is_closure() && arg.is_val() => {
            let Expr::Close {
                env: env1,
                expr: expr1,
            } = &**clo
            else {
                return Err(Error::StuckEval);
            };
            let Expr::Annot(abstr, ty) = &**expr1 else {
                return Err(Error::StuckEval);
            };
            let Expr::Abstr(_mode, body) = &**abstr else {
                return Err(Error::StuckEval);
            };
            let Type::Arr(_, b) = &**ty else {
                return Err(Error::StuckEval);
            };
            // TODO: modes
            let new_env = env1.merge(arg);
            let new_annot = Expr::Annot(body.clone(), b.clone());
            Ok(Expr::Close {
                env: Arc::new(new_env),
                expr: Arc::new(new_annot),
            })
        }

        // STEP-PROJV
        Expr::Fetch(e, lab) if e.is_rec(lab) => {
            let Expr::Label(_, el) = &**e else {
                return Err(Error::StuckEval);
            };
            Ok((**el).clone())
        }

        // STEP-EVAL
        Expr::Annot(e, t) => Ok(Expr::Annot(Arc::new(step(env, e)?), t.clone())),
        Expr::Merge(a, b) => Ok(Expr::Merge(Arc::new(step(env, a)?), b.clone())),
        Expr::Label(lab, e) => Ok(Expr::Label(lab.clone(), Arc::new(step(env, e)?))),
        Expr::Fetch(e, lab) => Ok(Expr::Fetch(Arc::new(step(env, e)?), lab.clone())),
        Expr::Apply(clo, arg) if !clo.is_val() => {
            Ok(Expr::Apply(Arc::new(step(env, clo)?), arg.clone()))
        }
        Expr::Apply(clo, arg) => Ok(Expr::Apply(clo.clone(), Arc::new(step(env, arg)?))),
        Expr::Close { env: env1, expr } => Ok(Expr::Close {
            env: Arc::new(step(env, env1)?),
            expr: expr.clone(),
        }),

        _ => Err(Error::StuckEval),
    }
}
