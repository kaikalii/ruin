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
        if let Some(right) = &self.right {
            let left = self.left.data.eval(cb, visited)?;
            if left.is_truth() {
                Ok(left)
            } else {
                right.expr.data.eval(cb, visited)
            }
        } else {
            self.left.data.eval(cb, visited)
        }
    }
}

impl ExprAnd {
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
        if let Some(right) = &self.right {
            let left = self.left.data.eval(cb, visited)?;
            if left.is_truth() {
                right.expr.data.eval(cb, visited)
            } else {
                Ok(left)
            }
        } else {
            self.left.data.eval(cb, visited)
        }
    }
}

impl ExprCmp {
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
        if let Some(right) = &self.right {
            let op = right.op.data;
            let left = self.left.data.eval(cb, visited)?;
            let right = right.expr.data.eval(cb, visited)?;
            Ok(Value::Bool(match op {
                OpCmp::Is => left == right,
                OpCmp::Isnt => left != right,
                op => match (left, right) {
                    (Value::Num(a), Value::Num(b)) => match op {
                        OpCmp::Less => a < b,
                        OpCmp::Greater => a > b,
                        OpCmp::LessOrEqual => a <= b,
                        OpCmp::GreaterOrEqual => a >= b,
                        _ => unreachable!(),
                    },
                    (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
                },
            }))
        } else {
            self.left.data.eval(cb, visited)
        }
    }
}

impl ExprAS {
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
        if let Some(right) = &self.right {
            let op = right.op.data;
            let left = self.left.data.eval(cb, visited)?;
            let right = right.expr.data.eval(cb, visited)?;
            match (left, right) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(match op {
                    OpAS::Add => a + b,
                    OpAS::Sub => a - b,
                })),
                (Value::Num(_), val) | (val, _) => Err(EvalError::Math(val.ty())),
            }
        } else {
            self.left.data.eval(cb, visited)
        }
    }
}

impl ExprMDR {
    fn eval(&self, cb: &Codebase, visited: &mut HashSet<String>) -> EvalResult<Value> {
        if let Some(right) = &self.right {
            let op = right.op.data;
            let left = self.left.data.eval(cb, visited)?;
            let right = right.expr.data.eval(cb, visited)?;
            match (left, right) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(match op {
                    OpMDR::Mul => a * b,
                    OpMDR::Div => a / b,
                    OpMDR::Rem => a % b,
                })),
                (Value::Num(_), val) | (val, _) => Err(EvalError::Math(val.ty())),
            }
        } else {
            self.left.data.eval(cb, visited)
        }
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
