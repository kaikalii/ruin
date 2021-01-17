use colored::Colorize;
use indexmap::IndexMap;

use crate::{eval::*, parse::*, value::Value};

pub struct Evald {
    pub expr: Expression,
    pub res: Option<EvalResult<Value>>,
}

impl Evald {
    pub fn format(&self) -> String {
        match &self.res {
            Some(Ok(_)) if self.expr.terms() == 1 => format!("{}", self.expr),
            Some(Ok(val)) => format!("{} = {}", self.expr, val),
            Some(Err(e)) => format!("{} = {}", self.expr, e.to_string().red()),
            None => format!("{}", self.expr),
        }
    }
}

#[derive(Default)]
pub struct Codebase {
    pub vals: IndexMap<String, Evald>,
}

impl Codebase {
    pub fn assign(&mut self, ass: Assignment) {
        // Unassign results that depend on the ident
        for evald in self.vals.values_mut() {
            if evald.expr.contains_ident(&ass.ident) {
                evald.res = None;
            }
        }
        // Insert
        self.vals.remove(&ass.ident);
        self.vals.insert(
            ass.ident,
            Evald {
                expr: ass.expr,
                res: None,
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
