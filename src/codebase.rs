use std::rc::Rc;

use colored::Colorize;
use indexmap::IndexMap;

use crate::{eval::*, parse::*, value::Value};

#[derive(Debug, Clone, Default)]
pub struct Codebase {
    pub parent: Option<Rc<Self>>,
    pub vals: IndexMap<String, Evald>,
}

impl Codebase {
    pub fn push(&mut self, mut child: Self) {
        std::mem::swap(self, &mut child);
        self.parent = Some(Rc::new(child));
    }
    pub fn pop(&mut self) -> Self {
        let mut parent = Rc::try_unwrap(self.parent.take().unwrap()).unwrap();
        std::mem::swap(self, &mut parent);
        parent
    }
    pub fn get(&self, ident: &str) -> Option<&Evald> {
        self.vals
            .get(ident)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(ident)))
    }
    pub fn insert(&mut self, ident: String, expr: Expression) {
        // Unassign results that depend on the ident
        self.unassign_results(&ident);
        // Insert
        self.vals.remove(&ident);
        self.vals.insert(
            ident,
            Evald {
                expr: Some(expr),
                res: None,
            },
        );
    }
    pub fn insert_val(&mut self, ident: String, val: Value) {
        self.vals.insert(
            ident,
            Evald {
                expr: None,
                res: Some(Ok(val)),
            },
        );
    }
    pub fn print(&self, n: usize) {
        println!();
        if n < self.vals.len() {
            println!("...");
        }
        for (ident, evald) in self.vals.iter().rev().take(n).rev() {
            println!("{} = {}", ident.to_string().bold(), evald.format())
        }
        println!();
    }
    pub fn evaled_count(&self) -> usize {
        self.vals
            .values()
            .filter(|val| val.res.as_ref().map_or(false, |res| res.is_ok()))
            .count()
    }
    pub fn unassign_results(&mut self, ident: &str) {
        let mut idents = vec![ident.to_owned()];
        while !idents.is_empty() {
            for ident in idents.drain(..).collect::<Vec<_>>() {
                for (id, evald) in &mut self.vals {
                    if let Some(expr) = &evald.expr {
                        if expr.contains_ident(&ident) {
                            evald.res = None;
                            idents.push(id.clone());
                        }
                    }
                }
            }
        }
    }
    pub fn eval_all(&mut self) {
        // Get initial count and idents
        let mut count = self.evaled_count();
        let idents: Vec<_> = self.vals.keys().cloned().collect();
        // Loop until the number of success counts does not change
        loop {
            for ident in &idents {
                let res = eval(self, ident);
                self.vals.get_mut(ident).unwrap().res = Some(res);
            }
            let new_count = self.evaled_count();
            if new_count == count {
                break;
            }
            count = new_count;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Evald {
    pub expr: Option<Expression>,
    pub res: Option<EvalResult<Value>>,
}

impl Evald {
    pub fn format(&self) -> String {
        let expr = self
            .expr
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_default();
        match &self.res {
            Some(Ok(val)) => {
                let val = val.to_string();
                if val == expr {
                    val
                } else {
                    format!("{} = {}", expr, val)
                }
            }
            Some(Err(EvalError::UnknownValue(_))) => expr,
            Some(Err(e)) => format!("{} = {}", expr, e.to_string().red()),
            None => expr,
        }
    }
}
