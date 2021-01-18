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
    #[display(fmt = "Attempted to call {}, a {} value", expr, ty)]
    CallNonFunction { expr: String, ty: Type },
}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn eval(cb: &mut Codebase, ident: &str) -> EvalResult<Value> {
    let mut visited = HashSet::new();
    eval_rec(ident, cb, &mut visited)
}

type Visited = HashSet<String>;

pub fn eval_rec(ident: &str, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
    if visited.contains(ident) {
        return Err(EvalError::RecursiveValue(ident.to_string()));
    }
    visited.insert(ident.into());
    Ok(if let Some(Evald { expr, res }) = cb.get(ident) {
        if let Some(Ok(val)) = res {
            val.clone()
        } else if let Some(expr) = expr {
            expr.clone().eval(cb, visited)?
        } else {
            panic!("Invalid evald configuration")
        }
    } else {
        return Err(EvalError::UnknownValue(ident.into()));
    })
}

impl ExprOr {
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
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
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
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
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
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
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
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
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
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

impl ExprCall {
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
        let term = self.term.eval(cb, visited)?;
        Ok(if let Some(args) = &self.args {
            let function = if let Value::Function(f) = &term {
                f
            } else {
                return Err(EvalError::CallNonFunction {
                    expr: self.term.to_string(),
                    ty: term.ty(),
                });
            };
            let mut arg_vals = Vec::with_capacity(args.len());
            for arg in args {
                arg_vals.push(arg.eval(cb, visited)?);
            }
            let mut function_cb = Codebase::default();
            for (name, val) in function.args.iter().zip(arg_vals) {
                function_cb.insert_val(name.clone(), val);
            }
            cb.push(function_cb);
            let mut visited = Visited::new();
            let ret = function.body.eval(cb, &mut visited)?;
            cb.pop();
            ret
        } else {
            term
        })
    }
}

impl Term {
    fn eval(&self, cb: &mut Codebase, visited: &mut Visited) -> EvalResult<Value> {
        Ok(match self {
            Term::Expr(expr) => expr.eval(cb, visited)?,
            Term::Bool(b) => Value::Bool(*b),
            Term::Num(n) => Value::Num(*n),
            Term::Nil => Value::Nil,
            Term::Ident(ident) => eval_rec(ident, cb, visited)?,
            Term::String(s) => Value::String(s.clone()),
            Term::Function(f) => Value::Function(f.clone()),
        })
    }
}
