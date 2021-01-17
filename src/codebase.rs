use std::cmp::Ordering;

use colored::Colorize;
use indexmap::IndexMap;

use crate::{eval::*, parse::*, value::Value};

#[derive(Debug, Clone, Default)]
pub struct Codebase {
    pub vals: IndexMap<String, Vec<Evald>>,
}

impl Codebase {
    pub fn get(&self, ident: &str) -> Option<&Evald> {
        self.vals.get(ident).and_then(|stack| stack.last())
    }
    pub fn get_mut(&mut self, ident: &str) -> Option<&mut Evald> {
        self.vals.get_mut(ident).and_then(|stack| stack.last_mut())
    }
    pub fn stack_size(&self, ident: &str) -> usize {
        self.vals.get(ident).map(|stack| stack.len()).unwrap_or(0)
    }
    pub fn push(&mut self, ident: String, expr: Expression) {
        // Unassign results that depend on the ident
        self.unassign_results(&ident);
        // Insert
        let mut stack = self.vals.remove(&ident).unwrap_or_default();
        stack.push(Evald { expr, res: None });
        self.vals.insert(ident, stack);
    }
    pub fn pop(&mut self, ident: &str) -> Option<Evald> {
        if let Some(stack) = self.vals.get_mut(ident) {
            stack.pop()
        } else {
            None
        }
    }
    pub fn print(&self, n: usize) {
        println!();
        if n < self.vals.len() {
            println!("...");
        }
        for (ident, evald) in self.vals.iter().rev().take(n).rev() {
            println!(
                "{} = {}",
                ident.to_string().bold(),
                evald.last().unwrap().format()
            )
        }
        println!();
    }
    pub fn evaled_count(&self) -> usize {
        self.vals
            .values()
            .flatten()
            .filter(|val| val.res.as_ref().map_or(false, |res| res.is_ok()))
            .count()
    }
    pub fn unassign_results(&mut self, ident: &str) {
        let mut idents = vec![ident.to_owned()];
        while !idents.is_empty() {
            for ident in idents.drain(..).collect::<Vec<_>>() {
                for (id, stack) in &mut self.vals {
                    for evald in stack {
                        if evald.expr.contains_ident(&ident) {
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
                for index in 0..self.stack_size(ident) {
                    let res = eval(self, ident, index);
                    self.vals.get_mut(ident).unwrap()[index].res = Some(res);
                }
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
    pub expr: Expression,
    pub res: Option<EvalResult<Value>>,
}

impl PartialEq for Evald {
    fn eq(&self, other: &Self) -> bool {
        self.expr.eq(&other.expr)
    }
}

impl Eq for Evald {}

impl PartialOrd for Evald {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.expr.partial_cmp(&other.expr)
    }
}

impl Ord for Evald {
    fn cmp(&self, other: &Self) -> Ordering {
        self.expr.cmp(&other.expr)
    }
}

impl Evald {
    pub fn format(&self) -> String {
        match &self.res {
            Some(Ok(val)) => {
                let val = val.to_string();
                let expr = self.expr.to_string();
                if val == expr {
                    val
                } else {
                    format!("{} = {}", self.expr, val)
                }
            }
            Some(Err(EvalError::UnknownValue(_))) => format!("{}", self.expr),
            Some(Err(e)) => format!("{} = {}", self.expr, e.to_string().red()),
            None => format!("{}", self.expr),
        }
    }
}
