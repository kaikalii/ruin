use std::collections::HashSet;

use derive_more::Display;

use crate::{codebase::*, parse::*, value::*};

#[derive(Debug, Display)]
pub enum EvalError {
    #[display(fmt = "Attempted to perform arithmetic on {} value", _0)]
    Math(Type),
    #[display(fmt = "Recursive value detected: {:?}", _0)]
    RecursiveValue(String),
    #[display(fmt = "Unknown type")]
    UnknownType,
}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn eval(cb: &Codebase, ident: &str) -> EvalResult<Value> {
    let mut visited = HashSet::new();
    eval_rec(ident, cb, &mut visited)
}

pub fn eval_rec(ident: &str, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
    if visited.contains(ident) {
        return Err(EvalError::RecursiveValue(ident.into()));
    }
    visited.insert(ident.into());
    Ok(if let Some(Evald { expr, res }) = cb.vals.get(ident) {
        if let Some(Ok(val)) = res {
            val.clone()
        } else {
            expr.eval(cb, visited)?
        }
    } else {
        return Err(EvalError::UnknownType);
    })
}

impl ExprOr {
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
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
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
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
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
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
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
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
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
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
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
        Ok(match self {
            Term::Expr(expr) => expr.eval(cb, visited)?,
            Term::Bool(b) => Value::Bool(*b),
            Term::Num(n) => Value::Num(*n),
            Term::Nil => Value::Nil,
            Term::Ident(ident) => eval_rec(ident, cb, visited)?,
        })
    }
}
