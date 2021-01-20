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

#[derive(Clone, Copy)]
pub struct EvalState<'a> {
    pub cb: &'a Rc<Codebase>,
    pub callers: &'a Callers<'a>,
}

impl<'a> EvalState<'a> {
    pub fn new(cb: &'a Rc<Codebase>, callers: &'a Callers<'a>) -> Self {
        EvalState { cb, callers }
    }
}

pub fn eval(cb: &Rc<Codebase>, path: &Path) -> Value {
    eval_rec(path, EvalState::new(cb, &Vector::new().push_back(path))).into()
}

pub fn eval_rec(path: &Path, state: EvalState) -> EvalResult {
    if let Some(val) = state.cb.get(path).cloned() {
        if let Value::Expression { val: None, expr } = val {
            expr.eval(state)
        } else {
            Ok(val.as_evald().clone())
        }
    } else {
        Err(EvalError::UnknownValue(path.clone()))
    }
}

impl ExprOr {
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let mut val = self.left.data.eval(state)?;
        for right in &self.rights {
            val = if val.is_truth() {
                val
            } else {
                right.expr.data.eval(state)?
            };
        }
        Ok(val)
    }
}

impl ExprAnd {
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let mut val = self.left.data.eval(state)?;
        for right in &self.rights {
            val = if val.is_truth() {
                right.expr.data.eval(state)?
            } else {
                val
            };
        }
        Ok(val)
    }
}

impl ExprCmp {
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let mut val = self.left.data.eval(state)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(state)?;
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
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let mut val = self.left.data.eval(state)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(state)?;
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
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let mut val = self.left.data.eval(state)?;
        for right in &self.rights {
            let op = right.op.data;
            let right = right.expr.data.eval(state)?;
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
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let fexpr = self.fexpr.eval(state)?;
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
                arg_vals.push(arg.eval(state)?);
            }

            let mut function_cb = Codebase::from_parent(state.cb.clone());
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
            function
                .body
                .eval(EvalState::new(&function_cb, state.callers))
        } else {
            Ok(fexpr)
        }
    }
}

impl ExprNot {
    pub fn eval(&self, state: EvalState) -> EvalResult {
        let mut val = self.expr.eval(state)?;
        for _ in 0..self.count {
            val = Value::Bool(!val.is_truth());
        }
        Ok(val)
    }
}

impl Term {
    pub fn eval(&self, state: EvalState) -> EvalResult {
        Ok(match self {
            Term::Expr(expr) => expr.eval(state)?,
            Term::Bool(b) => Value::Bool(*b),
            Term::Num(n) => Value::Num(*n),
            Term::Nil => Value::Nil,
            Term::Ident(ident) => {
                if state
                    .callers
                    .iter()
                    .filter_map(|path| path.name.as_ref())
                    .any(|name| name == ident)
                {
                    return Err(EvalError::RecursiveValue(ident.into()));
                } else {
                    let path = Path::from(ident);
                    let val: Value = eval_rec(
                        &path,
                        EvalState::new(state.cb, &state.callers.push_back(&path)),
                    )?;
                    val
                }
            }
            Term::String(s) => Value::String(s.clone()),
            Term::Function(f) => Value::Function(f.clone().into()),
        })
    }
}
