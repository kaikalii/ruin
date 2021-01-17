use std::cmp::Ordering;

use colored::Colorize;
use indexmap::IndexMap;

use crate::{eval::*, parse::*, value::Value};

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

#[derive(Debug, Clone, Default)]
pub struct Env {
    pub vals: IndexMap<String, Evald>,
}

impl Env {
    pub fn assign(&mut self, ass: Assignment) {
        // Unassign results that depend on the ident
        self.unassign_results(&ass.ident);
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
    pub fn unassign_results(&mut self, ident: &str) {
        let mut idents = vec![ident.to_owned()];
        while !idents.is_empty() {
            for ident in idents.drain(..).collect::<Vec<_>>() {
                for (id, evald) in &mut self.vals {
                    if evald.expr.contains_ident(&ident) {
                        evald.res = None;
                        idents.push(id.clone());
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
