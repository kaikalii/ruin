use std::{
    ops::Index,
    sync::{Arc, Mutex, MutexGuard},
};

use colored::Colorize;
use derive_more::Display;

use crate::{ast::*, codebase::*, value::*};

#[derive(Debug, Display, Clone)]
pub enum EvalError {
    #[display(
        fmt = "Attempted to perform arithmetic on {} value",
        "_0.to_string().underline()"
    )]
    Math(Type),
    #[display(fmt = "Recursive value detected: {}", _0)]
    RecursiveValue(String),
    #[display(
        fmt = "Attempted to call {}, a {} value",
        expr,
        "ty.to_string().underline()"
    )]
    CallNonFunction {
        expr: String,
        ty: Type,
    },
    #[display(
        fmt = "Cannot assign member of {}, a {} value",
        expr,
        "ty.to_string().underline()"
    )]
    _CantAssign {
        expr: String,
        ty: Type,
    },
    #[display(fmt = "Expected {} but found {}", expected, found)]
    TypeMismatch {
        expected: Type,
        found: Type,
    },
    #[display(fmt = "Unknown value: {:?}", _0)]
    UnknownValue(String),
    #[display(fmt = "Expected argument")]
    ExpectedArg,
    Value(String),
}

pub type EvalResult<T = ()> = Result<T, EvalError>;

pub type Callers = Vec<String>;

pub type ArgNameStack = Vec<Vec<String>>;
pub type ArgValStack = Vec<Vec<Value>>;

pub type ArgIndex = (usize, usize);

fn stack_arg(arg_stack: &[Vec<String>], ident: &str) -> EvalResult<ArgIndex> {
    for (i, args) in arg_stack.iter().enumerate().rev() {
        for (j, arg) in args.iter().enumerate().rev() {
            if ident == arg {
                return Ok((i, j));
            }
        }
    }
    Err(EvalError::UnknownValue(ident.into()))
}

#[derive(Clone)]
pub struct CompileState {
    pub cb: Arc<Codebase>,
    pub callers: Callers,
    pub args: ArgNameStack,
}

impl CompileState {
    pub fn new(cb: Arc<Codebase>) -> Self {
        CompileState {
            cb,
            callers: Callers::new(),
            args: ArgNameStack::default(),
        }
    }
}

impl<P> Index<P> for CompileState
where
    P: Into<String>,
{
    type Output = Value;
    fn index(&self, ident: P) -> &Self::Output {
        &self.cb[ident]
    }
}

pub fn eval_ident(cb: &Arc<Codebase>, ident: &str, seq: bool) -> Value {
    let mut instrs = Instrs::new();
    let mut state = CompileState::new(cb.clone());
    if let Err(e) = compile_ident(ident, &mut state, &mut instrs) {
        return e.into();
    }
    Stack::new().seq(seq).run(&instrs)
}

pub fn eval_function(function: Value, args: Vec<Value>) -> Value {
    let mut stack = Stack::new();
    let len = args.len();
    for val in args.into_iter().chain(Some(function)) {
        stack.push(val);
    }
    execute(&[Instr::Call(len)], &mut stack)
        .and_then(|_| stack.pop())
        .unwrap_or_else(Into::into)
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

#[derive(Debug, Display, Clone)]
pub enum Instr {
    #[display(fmt = "push {}", _0)]
    Push(Value),
    #[display(
        fmt = "Delayed({})",
        r#"_0.iter().map(|i| format!("{}, ", i)).collect::<String>()"#
    )]
    Delayed(Instrs),
    #[display(fmt = "Arg({}, {})", "_0.0", "_0.1")]
    Arg(ArgIndex),
    Or,
    And,
    #[display(fmt = "op {}", _0)]
    Cmp(OpCmp),
    #[display(fmt = "op {}", _0)]
    AS(OpAS),
    #[display(fmt = "op {}", _0)]
    MDR(OpMDR),
    Not,
    #[display(fmt = "call with {} arg(s)", _0)]
    Call(usize),
    PopArgs,
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

pub struct Stack {
    vals: Vec<Pushed>,
    args: ArgValStack,
    free_seq: bool,
}

impl Default for Stack {
    fn default() -> Self {
        Stack {
            vals: Vec::new(),
            args: ArgValStack::new(),
            free_seq: false,
        }
    }
}

impl Stack {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn seq(self, seq: bool) -> Self {
        Stack {
            free_seq: seq,
            ..self
        }
    }
    #[track_caller]
    pub fn pop(&mut self) -> EvalResult<Value> {
        self.vals
            .pop()
            .ok_or(EvalError::ExpectedArg)
            .map(Pushed::unwrap_value)
    }
    #[track_caller]
    pub fn pop2(&mut self) -> EvalResult<(Value, Value)> {
        let second = self.pop()?;
        Ok((self.pop()?, second))
    }
    pub fn pop_seq(&mut self) -> EvalResult<Value> {
        if let Some(Pushed::Value(Value::Seq)) = self.vals.last() {
            self.pop()
        } else if self.free_seq {
            self.free_seq = false;
            Ok(Value::Seq)
        } else {
            Err(EvalError::TypeMismatch {
                expected: Type::Seq,
                found: self.pop()?.ty(),
            })
        }
    }
    #[track_caller]
    pub fn pop_delayed(&mut self) -> EvalResult<Instrs> {
        self.vals
            .pop()
            .ok_or(EvalError::ExpectedArg)
            .map(Pushed::unwrap_delayed)
    }
    pub fn push(&mut self, val: Value) {
        self.vals.push(Pushed::Value(val));
    }
    pub fn push_delayed(&mut self, instrs: Instrs) {
        self.vals.push(Pushed::Delayed(instrs));
    }
    pub fn arg(&self, (i, j): ArgIndex) -> Value {
        self.args
            .get(self.args.len() - 1 - i)
            .and_then(|args| args.get(j))
            .cloned()
            .unwrap_or(Value::Nil)
    }
    pub fn run(&mut self, instrs: &[Instr]) -> Value {
        if let Err(e) = execute(instrs, self) {
            return e.into();
        }
        self.pop().unwrap_or_else(Into::into)
    }
}

enum InstrSource<'a> {
    Borrowed(&'a [Instr]),
    Owned(Vec<Instr>),
    Shared(Arc<Mutex<Vec<Instr>>>),
}

impl<'a> InstrSource<'a> {
    fn borrow(&self) -> InstrSourceRef {
        match self {
            InstrSource::Borrowed(slice) => InstrSourceRef::Borrowed(*slice),
            InstrSource::Owned(vec) => InstrSourceRef::Borrowed(vec.as_ref()),
            InstrSource::Shared(mutex) => InstrSourceRef::Shared(mutex.lock().unwrap()),
        }
    }
}

enum InstrSourceRef<'a> {
    Borrowed(&'a [Instr]),
    Shared(MutexGuard<'a, Vec<Instr>>),
}

impl<'a> AsRef<[Instr]> for InstrSourceRef<'a> {
    fn as_ref(&self) -> &[Instr] {
        match self {
            InstrSourceRef::Borrowed(slice) => *slice,
            InstrSourceRef::Shared(guard) => guard.as_ref(),
        }
    }
}

pub fn execute(instrs: &[Instr], stack: &mut Stack) -> EvalResult {
    let mut call_stack: Vec<(InstrSource, usize)> = vec![(InstrSource::Borrowed(instrs), 0)];
    while let Some((instrs, i)) = call_stack.last_mut() {
        let source_ref = instrs.borrow();
        if let Some(instr) = source_ref.as_ref().get(*i) {
            *i += 1;
            match instr {
                Instr::Push(val) => stack.push(val.clone()),
                Instr::Delayed(instrs) => stack.push_delayed(instrs.clone()),
                Instr::Arg(index) => stack.push(stack.arg(*index)),
                Instr::Or => {
                    let right = stack.pop_delayed()?;
                    let left = stack.pop()?;
                    if left.is_truth() {
                        stack.push(left);
                    } else {
                        drop(source_ref);
                        call_stack.push((InstrSource::Owned(right), 0));
                    }
                }
                Instr::And => {
                    let right = stack.pop_delayed()?;
                    let left = stack.pop()?;
                    if left.is_truth() {
                        drop(source_ref);
                        call_stack.push((InstrSource::Owned(right), 0));
                    } else {
                        stack.push(left);
                    }
                }
                Instr::Cmp(op) => {
                    let (left, right) = stack.pop2()?;
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
                            (Value::Num(_), val) | (val, _) => {
                                return Err(EvalError::Math(val.ty()))
                            }
                        },
                    };
                    stack.push(val);
                }
                Instr::AS(op) => {
                    let (left, right) = stack.pop2()?;
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
                    let (left, right) = stack.pop2()?;
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
                    let val = stack.pop()?;
                    stack.push(Value::Bool(!val.is_truth()));
                }
                Instr::Call(arg_count) => {
                    let fval = stack.pop()?;
                    let function = match fval {
                        Value::Function(function) => function,
                        Value::Error(e) => return Err(EvalError::Value(e)),
                        val => {
                            return Err(EvalError::CallNonFunction {
                                expr: val.to_string(),
                                ty: val.ty(),
                            })
                        }
                    };
                    match &function.body {
                        FunctionBody::Builtin(f) => f(stack)?,
                        FunctionBody::Expr { instrs, .. } => {
                            let mut args = Vec::with_capacity(*arg_count);
                            for _ in 0..*arg_count {
                                args.push(stack.pop()?);
                            }
                            args.reverse();
                            stack.args.push(args);
                            drop(source_ref);
                            call_stack.push((InstrSource::Owned(vec![Instr::PopArgs]), 0));
                            call_stack.push((InstrSource::Shared(instrs.clone()), 0));
                        }
                    }
                }
                Instr::PopArgs => {
                    stack.args.pop();
                }
            }
        } else {
            drop(source_ref);
            call_stack.pop();
        }
    }
    Ok(())
}

pub fn compile_ident(ident: &str, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
    match stack_arg(&state.args, ident) {
        Ok(index) => instrs.push(Instr::Arg(index)),
        Err(e) => {
            if state.callers.iter().any(|name| name == ident) {
                if state.cb.declared_functions.contains(ident) {
                    let fval = state
                        .cb
                        .get(ident)
                        .expect("Declared function not in codebase");
                    instrs.push(fval.clone().into());
                } else {
                    return Err(EvalError::RecursiveValue(ident.into()));
                }
            } else if let Some(val) = state.cb.get(ident).cloned() {
                if let Value::Expression { val, expr } = val {
                    if let Some(val) = val {
                        instrs.push((*val).into());
                    } else {
                        state.callers.push(ident.into());
                        let val = expr.eval(state, false).ok()?;
                        state.callers.pop();
                        instrs.push(val.into());
                    }
                } else {
                    instrs.push(val.into());
                }
            } else {
                return Err(e);
            }
        }
    }
    Ok(())
}

pub trait Evalable {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult;
    fn eval(&self, state: &mut CompileState, seq: bool) -> Value {
        let mut instrs = Instrs::new();
        match self.compile(state, &mut instrs) {
            Ok(()) => {
                let mut stack = Stack::new().seq(seq);
                if let Err(e) = execute(&instrs, &mut stack) {
                    return e.into();
                }
                stack.pop().unwrap_or_else(Into::into)
            }
            Err(e) => e.into(),
        }
    }
}

impl Evalable for ExprOr {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        self.left.compile(state, instrs)?;
        for right in &self.rights {
            let mut delayed = Instrs::new();
            right.expr.compile(state, &mut delayed)?;
            instrs.push(delayed.into());
            instrs.push(Instr::Or);
        }
        Ok(())
    }
}

impl Evalable for ExprAnd {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        self.left.compile(state, instrs)?;
        for right in &self.rights {
            let mut delayed = Instrs::new();
            right.expr.compile(state, &mut delayed)?;
            instrs.push(delayed.into());
            instrs.push(Instr::And);
        }
        Ok(())
    }
}

impl Evalable for ExprCmp {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        self.left.compile(state, instrs)?;
        for right in &self.rights {
            right.expr.compile(state, instrs)?;
            instrs.push(Instr::Cmp(right.op));
        }
        Ok(())
    }
}

impl Evalable for ExprAS {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        self.left.compile(state, instrs)?;
        for right in &self.rights {
            right.expr.compile(state, instrs)?;
            instrs.push(Instr::AS(right.op));
        }
        Ok(())
    }
}

impl Evalable for ExprMDR {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        self.left.compile(state, instrs)?;
        for right in &self.rights {
            right.expr.compile(state, instrs)?;
            instrs.push(Instr::MDR(right.op));
        }
        Ok(())
    }
}

impl Evalable for ExprCall {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        match self {
            ExprCall::Regular { term, args } => {
                let mut arg_count = None;
                if let Some(args) = args {
                    for arg in args {
                        arg.compile(state, instrs)?;
                    }
                    arg_count = Some(args.len());
                }
                term.compile(state, instrs)?;
                if let Some(arg_count) = arg_count {
                    instrs.push(Instr::Call(arg_count));
                }
            }
            ExprCall::Method { first, calls } => {
                first.compile(state, instrs)?;
                for call in calls {
                    for arg in &call.args {
                        arg.compile(state, instrs)?;
                    }
                    call.term.compile(state, instrs)?;
                    instrs.push(Instr::Call(call.args.len() + 1));
                }
            }
        }
        Ok(())
    }
}

impl Evalable for ExprNot {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        self.expr.compile(state, instrs)?;
        for _ in 0..self.count {
            instrs.push(Instr::Not);
        }
        Ok(())
    }
}

impl Evalable for Term {
    fn compile(&self, state: &mut CompileState, instrs: &mut Instrs) -> EvalResult {
        match self {
            Term::Expr(expr) => expr.compile(state, instrs)?,
            Term::Bool(b) => instrs.push(Value::Bool(*b).into()),
            Term::Num(n) => instrs.push(Value::Num(*n).into()),
            Term::Nil => instrs.push(Value::Nil.into()),
            Term::Ident(ident) => compile_ident(ident, state, instrs)?,
            Term::String(s) => instrs.push(Value::String(s.clone()).into()),
            Term::Function(function) => compile_function(function, state, Some(instrs))?,
        }
        Ok(())
    }
}

pub fn compile_function(
    function: &Function,
    state: &mut CompileState,
    instrs: Option<&mut Instrs>,
) -> EvalResult {
    // Get the body and compiled instructions
    let (body, function_instrs) = if let FunctionBody::Expr { expr, instrs } = &function.body {
        (expr, instrs)
    } else {
        panic!("Built-in function in term")
    };
    // Create a codebase to be used for body compilation
    let mut function_cb = Codebase::from_parent(state.cb.clone());
    // Insert values from the function's environment into the new codebase
    for (ident, val) in &function.env {
        function_cb.as_mut().insert(ident.into(), val.clone())
    }
    // Push the function's args to the state's arg stack
    state.args.push(function.args.idents.clone());
    // Access the function's instructions
    let mut function_instrs = function_instrs.lock().unwrap();
    function_instrs.clear();
    // Compile the function body
    body.compile(state, function_instrs.as_mut())?;
    // Pop the function's args
    state.args.pop();
    // Add the function as a push instruction
    if let Some(instrs) = instrs {
        instrs.push(Value::Function((*function).clone().into()).into());
    }
    Ok(())
}
