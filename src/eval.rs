use std::collections::HashSet;

use derive_more::Display;

use crate::{codebase::*, parse::*, value::*};

#[derive(Debug, Display, Clone)]
pub enum EvalError {
    #[display(fmt = "Attempted to perform arithmetic on {} value", _0)]
    Math(Type),
    #[display(fmt = "Recursive value detected: {:?}", _0)]
    RecursiveValue(String),
    #[display(fmt = "Unknown value \"{}\"", _0)]
    UnknownValue(String),
}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn eval(cb: &Codebase, ident: &str, index: usize) -> EvalResult<Value> {
    let mut visited = HashSet::new();
    eval_rec(ident, index, cb, &mut visited)
}

type Visited = HashSet<(String, usize)>;

pub fn eval_rec(
    ident: &str,
    index: usize,
    cb: &Codebase,
    visited: &mut Visited,
) -> EvalResult<Value> {
    let key = (ident.into(), index);
    if visited.contains(&key) {
        return Err(EvalError::RecursiveValue(ident.to_string()));
    }
    visited.insert(key);
    Ok(if let Some(Evald { expr, res }) = cb.get(ident) {
        if let Some(Ok(val)) = res {
            val.clone()
        } else {
            expr.eval(cb, visited)?
        }
    } else {
        return Err(EvalError::UnknownValue(ident.into()));
    })
}

impl ExprOr {
    fn eval(&self, cb: &Codebase, visited: &mut Visited) -> EvalResult<Value> {
        let mut val = self.left.data.eval(cb, visited)?;
        for right in &self.rights {
            val = if val.is_truth() {
                val
            } else {
                right.expr.data.eval(cb, visited)?
            };
        }
        Ok(val)
    }
}

impl ExprAnd {
    fn eval(&self, cb: &Codebase, visited: &mut Visited) -> EvalResult<Value> {
        let mut val = self.left.data.eval(cb, visited)?;
        for right in &self.rights {
            val = if val.is_truth() {
                right.expr.data.eval(cb, visited)?
            } else {
                val
            };
        }
        Ok(val)
    }
}

impl ExprCmp {
    fn eval(&self, cb: &Codebase, visited: &mut Visited) -> EvalResult<Value> {
        let mut val = self.left.data.eval(cb, visited)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, visited)?;
            val = Value::Bool(match op {
                OpCmp::Is => val == right,
                OpCmp::Isnt => val != right,
                op => match (val, right) {
                    (Value::Num(a), Value::Num(b)) => match op {
                        OpCmp::Less => a < b,
                        OpCmp::Greater => a > b,
                        OpCmp::LessOrEqual => a <= b,
                        OpCmp::GreaterOrEqual => a >= b,
                        _ => unreachable!(),
                    },
                    (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
                },
            });
        }
        Ok(val)
    }
}

impl ExprAS {
    fn eval(&self, cb: &Codebase, visited: &mut Visited) -> EvalResult<Value> {
        let mut val = self.left.data.eval(cb, visited)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, visited)?;
            val = match (val, right) {
                (Value::Num(a), Value::Num(b)) => Value::Num(match op {
                    OpAS::Add => a + b,
                    OpAS::Sub => a - b,
                }),
                (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
            };
        }
        Ok(val)
    }
}

impl ExprMDR {
    fn eval(&self, cb: &Codebase, visited: &mut Visited) -> EvalResult<Value> {
        let mut val = self.left.data.eval(cb, visited)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, visited)?;
            val = match (val, right) {
                (Value::Num(a), Value::Num(b)) => Value::Num(match op {
                    OpMDR::Mul => a * b,
                    OpMDR::Div => a / b,
                    OpMDR::Rem => a % b,
                }),
                (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
            };
        }
        Ok(val)
    }
}

impl Term {
    fn eval(&self, cb: &Codebase, visited: &mut Visited) -> EvalResult<Value> {
        Ok(match self {
            Term::Expr(expr) => expr.eval(cb, visited)?,
            Term::Bool(b) => Value::Bool(*b),
            Term::Num(n) => Value::Num(*n),
            Term::Nil => Value::Nil,
            Term::Ident(ident) => {
                let index = cb.stack_size(ident).saturating_sub(1);
                eval_rec(ident, index, cb, visited)?
            }
            Term::String(s) => Value::String(s.clone()),
            Term::Function(f) => Value::Function(f.clone()),
        })
    }
}
