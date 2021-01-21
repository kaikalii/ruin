use std::{iter::repeat, ops::Index, sync::Arc};

use derive_more::Display;
use rpds::VectorSync;

use crate::{codebase::*, parse::*, value::*};

#[derive(Debug, Display, Clone)]
pub enum EvalError {
    #[display(fmt = "Attempted to perform arithmetic on {} value", _0)]
    Math(Type),
    #[display(fmt = "Recursive value detected: {}", _0)]
    RecursiveValue(Path),
    #[display(fmt = "Attempted to call {}, a {} value", expr, ty)]
    CallNonFunction { expr: String, ty: Type },
    #[display(fmt = "Cannot assign member of {}, a {} value", expr, ty)]
    CantAssign { expr: String, ty: Type },
    #[display(fmt = "Expected {} but found {}", expected, found)]
    TypeMismatch { expected: Type, found: Type },
}

pub type EvalResult<T = ()> = Result<T, EvalError>;

pub type Callers = VectorSync<Path>;

#[derive(Clone)]
pub struct EvalState {
    pub cb: Arc<Codebase>,
    pub callers: Callers,
}

impl EvalState {
    pub fn new(cb: Arc<Codebase>, callers: Callers) -> Self {
        EvalState { cb, callers }
    }
}

impl<P> Index<P> for EvalState
where
    P: Into<Path>,
{
    type Output = Value;
    fn index(&self, path: P) -> &Self::Output {
        &self.cb[path]
    }
}

pub fn eval_path(cb: &Arc<Codebase>, path: &Path) -> Value {
    let mut instrs = Instrs::new();
    let res = eval_rec(
        path,
        EvalState::new(cb.clone(), Callers::default().push_back(path.clone())),
        &mut instrs,
    );
    if let Err(e) = res {
        return Err(e).into();
    }
    Stack::default().run(&instrs)
}

pub enum Pushed {
    Value(Value),
    Delayed(Instrs),
}

impl Pushed {
    #[track_caller]
    pub fn unwrap_value(self) -> Value {
        if let Pushed::Value(val) = self {
            val
        } else {
            panic!("Instr is not Value")
        }
    }
    #[track_caller]
    pub fn unwrap_delayed(self) -> Instrs {
        if let Pushed::Delayed(instrs) = self {
            instrs
        } else {
            panic!("Instr is not Delayed")
        }
    }
}

#[derive(Clone)]
pub enum Instr {
    Push(Value),
    Delayed(Instrs),
    Or,
    And,
    Cmp(OpCmp),
    AS(OpAS),
    MDR(OpMDR),
    Not,
    Call(usize),
    Builtin(BuiltinFunction),
}

impl From<Value> for Instr {
    fn from(val: Value) -> Self {
        Instr::Push(val)
    }
}

impl From<Instrs> for Instr {
    fn from(instrs: Instrs) -> Self {
        Instr::Delayed(instrs)
    }
}

pub type Instrs = Vec<Instr>;

#[derive(Default)]
pub struct Stack(Vec<Pushed>);

impl Stack {
    pub fn pop(&mut self) -> Value {
        self.0.pop().expect("Stack is empty").unwrap_value()
    }
    pub fn pop2(&mut self) -> (Value, Value) {
        (self.pop(), self.pop())
    }
    pub fn pop_delayed(&mut self) -> Instrs {
        self.0.pop().expect("Stack is empty").unwrap_delayed()
    }
    pub fn push(&mut self, val: Value) {
        self.0.push(Pushed::Value(val));
    }
    pub fn push_delayed(&mut self, instrs: Instrs) {
        self.0.push(Pushed::Delayed(instrs));
    }
    pub fn execute(&mut self, instrs: &[Instr]) -> EvalResult {
        for instr in instrs {
            instr.execute(self)?;
        }
        Ok(())
    }
    pub fn run(&mut self, instrs: &[Instr]) -> Value {
        if let Err(e) = self.execute(instrs) {
            return Err(e).into();
        }
        self.pop()
    }
}

impl Instr {
    pub fn execute(&self, stack: &mut Stack) -> EvalResult {
        match self {
            Instr::Push(val) => stack.push(val.clone()),
            Instr::Delayed(instrs) => stack.push_delayed(instrs.clone()),
            Instr::Or => {
                let left = stack.pop();
                let right = stack.pop_delayed();
                if left.is_truth() {
                    stack.push(left);
                } else {
                    stack.execute(&right)?;
                }
            }
            Instr::And => {
                let left = stack.pop();
                let right = stack.pop_delayed();
                if left.is_truth() {
                    stack.execute(&right)?;
                } else {
                    stack.push(left);
                }
            }
            Instr::Cmp(op) => {
                let (left, right) = stack.pop2();
                let val = match op {
                    OpCmp::Is => Value::Bool(left == right),
                    OpCmp::Isnt => Value::Bool(left != right),
                    op => match (left, right) {
                        (Value::Error(e), _) | (_, Value::Error(e)) => Value::Error(e),
                        (Value::Num(a), Value::Num(b)) => Value::Bool(match op {
                            OpCmp::Less => a < b,
                            OpCmp::Greater => a > b,
                            OpCmp::LessOrEqual => a <= b,
                            OpCmp::GreaterOrEqual => a >= b,
                            _ => unreachable!(),
                        }),
                        (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
                    },
                };
                stack.push(val);
            }
            Instr::AS(op) => {
                let (left, right) = stack.pop2();
                let val = match (left, right) {
                    (Value::Error(e), _) | (_, Value::Error(e)) => Value::Error(e),
                    (Value::Num(a), Value::Num(b)) => Value::Num(match op {
                        OpAS::Add => a + b,
                        OpAS::Sub => a - b,
                    }),
                    (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
                };
                stack.push(val);
            }
            Instr::MDR(op) => {
                let (left, right) = stack.pop2();
                let val = match (left, right) {
                    (Value::Error(e), _) | (_, Value::Error(e)) => Value::Error(e),
                    (Value::Num(a), Value::Num(b)) => Value::Num(match op {
                        OpMDR::Mul => a * b,
                        OpMDR::Div => a / b,
                        OpMDR::Rem => a % b,
                    }),
                    (Value::Num(_), val) | (val, _) => return Err(EvalError::Math(val.ty())),
                };
                stack.push(val);
            }
            Instr::Not => {
                let val = stack.pop();
                stack.push(Value::Bool(!val.is_truth()));
            }
            Instr::Builtin(f) => f(stack)?,
            Instr::Call(arg_count) => {
                let fval = stack.pop();
                let mut args = Vec::with_capacity(*arg_count);
                for _ in 0..*arg_count {
                    args.push(stack.pop());
                }
                args.reverse();
                let mut finstrs = Instrs::new();
                compile_function(&fval, &fval, args, state, &mut finstrs)?;
            }
        }
        Ok(())
    }
}

fn eval_rec(path: &Path, state: EvalState, instrs: &mut Instrs) -> EvalResult {
    if let Some(val) = state.cb.get(path).cloned() {
        if let Value::Expression { val, expr } = val {
            if let Some(val) = val {
                instrs.push((*val).into());
            } else {
                instrs.push(expr.eval(state).into());
            }
        } else {
            instrs.push(val.into());
        }
    }
    Ok(())
}

pub trait Evalable {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult;
    fn eval(&self, state: EvalState) -> Value {
        let mut instrs = Instrs::new();
        match self.compile(state, &mut instrs) {
            Ok(()) => {
                let mut stack = Stack::default();
                if let Err(e) = stack.execute(&instrs) {
                    return Err(e).into();
                }
                stack.pop()
            }
            Err(e) => Err(e).into(),
        }
    }
}

impl Evalable for ExprOr {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        self.left.data.compile(state.clone(), instrs)?;
        for right in &self.rights {
            let mut delayed = Instrs::new();
            right.expr.data.compile(state.clone(), &mut delayed)?;
            instrs.push(delayed.into());
            instrs.push(Instr::Or);
        }
        Ok(())
    }
}

impl Evalable for ExprAnd {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        self.left.data.compile(state.clone(), instrs)?;
        for right in &self.rights {
            let mut delayed = Instrs::new();
            right.expr.data.compile(state.clone(), &mut delayed)?;
            instrs.push(delayed.into());
            instrs.push(Instr::And);
        }
        Ok(())
    }
}

impl Evalable for ExprCmp {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        self.left.data.compile(state.clone(), instrs)?;
        for right in &self.rights {
            right.expr.data.compile(state.clone(), instrs)?;
            instrs.push(Instr::Cmp(right.op.data));
        }
        Ok(())
    }
}

impl Evalable for ExprAS {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        self.left.data.compile(state.clone(), instrs)?;
        for right in &self.rights {
            right.expr.data.compile(state.clone(), instrs)?;
            instrs.push(Instr::AS(right.op.data));
        }
        Ok(())
    }
}

impl Evalable for ExprMDR {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        self.left.data.compile(state.clone(), instrs)?;
        for right in &self.rights {
            right.expr.data.compile(state.clone(), instrs)?;
            instrs.push(Instr::MDR(right.op.data));
        }
        Ok(())
    }
}

impl Evalable for ExprCall {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        match self {
            ExprCall::Regular { term, args } => {
                let mut arg_count = None;
                if let Some(args) = args {
                    for arg in args {
                        arg.compile(state.clone(), instrs)?;
                    }
                    arg_count = Some(args.len());
                }
                if let Some(arg_count) = arg_count {
                    instrs.push(Instr::Call(arg_count));
                }
                term.compile(state, instrs)?;
            }
            ExprCall::Method { first, calls } => {
                first.compile(state.clone(), instrs)?;
                for call in calls {
                    for arg in &call.args {
                        arg.compile(state.clone(), instrs)?;
                    }
                    call.term.compile(state.clone(), instrs)?;
                    instrs.push(Instr::Call(call.args.len() + 1));
                }
            }
        }
        Ok(())
    }
}

pub fn compile_function<T>(
    context: &T,
    fexpr: &Value,
    args: Vec<Value>,
    state: EvalState,
    instrs: &mut Instrs,
) -> EvalResult
where
    T: std::fmt::Display,
{
    let function = if let Value::Function(f) = fexpr.as_evald() {
        f.clone()
    } else {
        return Err(EvalError::CallNonFunction {
            expr: context.to_string(),
            ty: fexpr.ty(),
        });
    };

    let mut function_cb = Codebase::from_parent(state.cb.clone());
    for (name, val) in function
        .args
        .iter()
        .zip(args.into_iter().chain(repeat(Value::Nil)))
    {
        function_cb.as_mut().insert(name.into(), val);
    }
    for (ident, expr) in &function.env {
        function_cb.as_mut().insert(ident.into(), expr.clone())
    }
    function
        .body
        .compile(EvalState::new(function_cb, state.callers), instrs)
}

impl Evalable for ExprNot {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        self.expr.compile(state, instrs)?;
        for _ in 0..self.count {
            instrs.push(Instr::Not);
        }
        Ok(())
    }
}

impl Evalable for Term {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        match self {
            Term::Expr(expr) => expr.compile(state, instrs)?,
            Term::Bool(b) => instrs.push(Value::Bool(*b).into()),
            Term::Num(n) => instrs.push(Value::Num(*n).into()),
            Term::Nil => instrs.push(Value::Nil.into()),
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
                    let sub_callers = state.callers.push_back(path.clone());
                    eval_rec(&path, EvalState::new(state.cb, sub_callers), instrs)?;
                }
            }
            Term::String(s) => instrs.push(Value::String(s.clone()).into()),
            Term::Function(f) => instrs.push(Value::Function(f.clone().into()).into()),
        }
        Ok(())
    }
}

impl FunctionBody {
    fn compile(&self, state: EvalState, instrs: &mut Instrs) -> EvalResult {
        match self {
            FunctionBody::Expr {
                expr,
                instrs: finstrs,
            } => expr.compile(state, instrs)?,
            FunctionBody::Builtin(f) => instrs.push(Instr::Builtin(f.clone())),
        }
        Ok(())
    }
}
