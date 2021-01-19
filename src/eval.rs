use std::iter::repeat;

use derive_more::Display;

use crate::{codebase::*, parse::*, value::*};

#[derive(Debug, Display, Clone)]
pub enum EvalError {
    #[display(fmt = "Attempted to perform arithmetic on {} value", _0)]
    Math(Type),
    #[display(fmt = "Recursive value detected: {:?}", _0)]
    RecursiveValue(Path),
    #[display(fmt = "Unknown value \"{}\"", _0)]
    UnknownValue(Path),
    #[display(fmt = "Attempted to call {}, a {} value", expr, ty)]
    CallNonFunction { expr: String, ty: Type },
    #[display(fmt = "Cannot assign member of {}, a {} value", expr, ty)]
    CantAssign { expr: String, ty: Type },
}

pub type EvalResult = Result<Value, EvalError>;

pub fn eval(cb: &mut Codebase, path: &Path) -> EvalResult {
    eval_rec(path, cb, path)
}

pub fn eval_rec(path: &Path, cb: &mut Codebase, caller: &Path) -> EvalResult {
    Ok(if let Some(Evald { expr, res }) = cb.get(path) {
        if let Some(Ok(val)) = res {
            val.clone()
        } else if let Some(expr) = expr {
            expr.clone().eval(cb, caller)?
        } else {
            panic!("Invalid evald configuration")
        }
    } else {
        return Err(EvalError::UnknownValue(path.clone()));
    })
}

impl ExprOr {
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let mut val = self.left.data.eval(cb, caller)?;
        for right in &self.rights {
            val = if val.is_truth() {
                val
            } else {
                right.expr.data.eval(cb, caller)?
            };
        }
        Ok(val)
    }
}

impl ExprAnd {
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let mut val = self.left.data.eval(cb, caller)?;
        for right in &self.rights {
            val = if val.is_truth() {
                right.expr.data.eval(cb, caller)?
            } else {
                val
            };
        }
        Ok(val)
    }
}

impl ExprCmp {
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let mut val = self.left.data.eval(cb, caller)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, caller)?;
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
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let mut val = self.left.data.eval(cb, caller)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, caller)?;
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
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let mut val = self.left.data.eval(cb, caller)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, caller)?;
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
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let fexpr = self.fexpr.eval(cb, caller)?;
        if let Some(args) = &self.args {
            let function = if let Value::Function(f) = &fexpr {
                f
            } else {
                return Err(EvalError::CallNonFunction {
                    expr: self.fexpr.to_string(),
                    ty: fexpr.ty(),
                });
            };
            let mut arg_vals = Vec::with_capacity(args.len());
            for arg in args {
                arg_vals.push(arg.eval(cb, caller)?);
            }

            let mut function_cb = Codebase::default();
            for (name, val) in function
                .args
                .iter()
                .zip(arg_vals.into_iter().chain(repeat(Value::Nil)))
            {
                function_cb.insert_val(name.into(), val);
            }
            for (ident, expr) in &function.env {
                function_cb.insert(ident.into(), expr.clone())
            }
            cb.push(function_cb);
            let ret = function.body.eval(cb, caller);
            cb.pop();
            ret
        } else {
            Ok(fexpr)
        }
    }
}

impl ExprNot {
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        let mut val = self.expr.eval(cb, caller)?;
        for _ in 0..self.count {
            val = Value::Bool(!val.is_truth());
        }
        Ok(val)
    }
}

impl Term {
    pub fn eval(&self, cb: &mut Codebase, caller: &Path) -> EvalResult {
        Ok(match self {
            Term::Expr(expr) => expr.eval(cb, caller)?,
            Term::Bool(b) => Value::Bool(*b),
            Term::Num(n) => Value::Num(*n),
            Term::Nil => Value::Nil,
            Term::Ident(ident) => {
                if caller.name.as_ref().map_or(false, |name| name == ident) {
                    return Err(EvalError::RecursiveValue(ident.into()));
                } else {
                    eval_rec(&ident.into(), cb, caller)?
                }
            }
            Term::String(s) => Value::String(s.clone()),
            Term::Function(f) => Value::Function(f.clone().into()),
        })
    }
}
