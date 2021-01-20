use std::{iter::repeat, rc::Rc};

use derive_more::Display;
use rpds::Vector;

use crate::{codebase::*, parse::*, value::*};

#[derive(Debug, Display, Clone)]
pub enum EvalError {
    #[display(fmt = "Attempted to perform arithmetic on {} value", _0)]
    Math(Type),
    #[display(fmt = "Recursive value detected: {}", _0)]
    RecursiveValue(Path),
    #[display(fmt = "Unknown value \"{}\"", _0)]
    UnknownValue(Path),
    #[display(fmt = "Attempted to call {}, a {} value", expr, ty)]
    CallNonFunction { expr: String, ty: Type },
    #[display(fmt = "Cannot assign member of {}, a {} value", expr, ty)]
    CantAssign { expr: String, ty: Type },
}

pub type EvalResult = Result<Value, EvalError>;

pub type Callers<'a> = Vector<&'a Path>;

pub fn eval(cb: &Rc<Codebase>, path: &Path) -> Value {
    eval_rec(path, cb, &Vector::new().push_back(path)).into()
}

pub fn eval_rec(path: &Path, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
    if let Some(val) = cb.get(path).cloned() {
        if let Value::Expression { val: None, expr } = val {
            expr.eval(cb, callers)
        } else {
            Ok(val.as_evaluated().clone())
        }
    } else {
        Err(EvalError::UnknownValue(path.clone()))
    }
}

impl ExprOr {
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let mut val = self.left.data.eval(cb, callers)?;
        for right in &self.rights {
            val = if val.is_truth() {
                val
            } else {
                right.expr.data.eval(cb, callers)?
            };
        }
        Ok(val)
    }
}

impl ExprAnd {
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let mut val = self.left.data.eval(cb, callers)?;
        for right in &self.rights {
            val = if val.is_truth() {
                right.expr.data.eval(cb, callers)?
            } else {
                val
            };
        }
        Ok(val)
    }
}

impl ExprCmp {
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let mut val = self.left.data.eval(cb, callers)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, callers)?;
            val = Value::Bool(match op {
                OpCmp::Is => val == right,
                OpCmp::Isnt => val != right,
                op => match (val, right) {
                    (Value::Error(e), _) | (_, Value::Error(e)) => return Ok(Value::Error(e)),
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
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let mut val = self.left.data.eval(cb, callers)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, callers)?;
            val = match (val, right) {
                (Value::Error(e), _) | (_, Value::Error(e)) => return Ok(Value::Error(e)),
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
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let mut val = self.left.data.eval(cb, callers)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(cb, callers)?;
            val = match (val, right) {
                (Value::Error(e), _) | (_, Value::Error(e)) => return Ok(Value::Error(e)),
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
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let fexpr = self.fexpr.eval(cb, callers)?;
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
                arg_vals.push(arg.eval(cb, callers)?);
            }

            let mut function_cb = Codebase::from_parent(cb.clone());
            for (name, val) in function
                .args
                .iter()
                .zip(arg_vals.into_iter().chain(repeat(Value::Nil)))
            {
                function_cb.as_mut().insert(name.into(), val);
            }
            for (ident, expr) in &function.env {
                function_cb.as_mut().insert(ident.into(), expr.clone())
            }
            function.body.eval(&function_cb, callers)
        } else {
            Ok(fexpr)
        }
    }
}

impl ExprNot {
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        let mut val = self.expr.eval(cb, callers)?;
        for _ in 0..self.count {
            val = Value::Bool(!val.is_truth());
        }
        Ok(val)
    }
}

impl Term {
    pub fn eval(&self, cb: &Rc<Codebase>, callers: &Callers) -> EvalResult {
        Ok(match self {
            Term::Expr(expr) => expr.eval(cb, callers)?,
            Term::Bool(b) => Value::Bool(*b),
            Term::Num(n) => Value::Num(*n),
            Term::Nil => Value::Nil,
            Term::Ident(ident) => {
                if callers
                    .iter()
                    .filter_map(|path| path.name.as_ref())
                    .any(|name| name == ident)
                {
                    return Err(EvalError::RecursiveValue(ident.into()));
                } else {
                    let path = Path::from(ident);
                    let val: Value = eval_rec(&path, cb, &callers.push_back(&path))?;
                    val
                }
            }
            Term::String(s) => Value::String(s.clone()),
            Term::Function(f) => Value::Function(f.clone().into()),
        })
    }
}
