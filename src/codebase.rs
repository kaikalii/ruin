use std::rc::Rc;

use colored::Colorize;
use indexmap::IndexMap;

use crate::{eval::*, parse::*, value::Value};

#[derive(Debug, Clone, Default)]
pub struct Codebase {
    pub parent: Option<Rc<Self>>,
    pub vals: IndexMap<Path, Evald>,
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
    pub fn get(&self, path: &Path) -> Option<&Evald> {
        self.vals
            .get(path)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(path)))
    }
    pub fn insert(&mut self, path: Path, expr: Expression) {
        // Unassign results that depend on the path
        self.unassign_results(&path);
        // Insert
        self.vals.remove(&path);
        self.vals.insert(
            path,
            Evald {
                expr: Some(expr),
                res: None,
            },
        );
    }
    pub fn insert_val(&mut self, path: Path, val: Value) {
        self.vals.insert(
            path,
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
        for (path, evald) in self.vals.iter().rev().take(n).rev() {
            println!("{} = {}", path.to_string().bold(), evald.format())
        }
        println!();
    }
    pub fn evaled_count(&self) -> usize {
        self.vals
            .values()
            .filter(|val| val.res.as_ref().map_or(false, |res| res.is_ok()))
            .count()
    }
    pub fn unassign_results(&mut self, path: &Path) {
        let mut paths = vec![path.to_owned()];
        while !paths.is_empty() {
            for child in paths.drain(..).collect::<Vec<_>>() {
                for (id, evald) in &mut self.vals {
                    if &child != path {
                        if let Some(expr) = &evald.expr {
                            if expr.contains_ident(path.name.as_ref().unwrap()) {
                                evald.res = None;
                                paths.push(id.clone());
                            }
                        }
                    }
                }
            }
        }
    }
    pub fn eval_all(&mut self) {
        // Get initial count and paths
        let mut count = self.evaled_count();
        let paths: Vec<_> = self.vals.keys().cloned().collect();
        // Loop until the number of success counts does not change
        loop {
            for path in &paths {
                self.eval_path(path);
            }
            let new_count = self.evaled_count();
            if new_count == count {
                break;
            }
            count = new_count;
        }
    }
    fn eval_path(&mut self, path: &Path) {
        let res = eval(self, path);
        if let Some(evald) = self.vals.get_mut(path) {
            evald.res = Some(res);
        }
        if !path.disam.is_empty() {
            if let Some(parent) = path.parent() {
                self.eval_path(&parent);
                if let Some(evald) = self.vals.get(path) {
                    if let Some(expr) = evald.expr.clone() {
                        if let Some(evald) = self.vals.get_mut(&parent) {
                            if let Some(res) = &mut evald.res {
                                match res {
                                    Ok(Value::Function(function)) => {
                                        let function = Rc::make_mut(function);
                                        function.env =
                                            function.env.insert(path.name.clone().unwrap(), expr);
                                    }
                                    Ok(val) => {
                                        let ty = val.ty();
                                        let expr = evald
                                            .expr
                                            .as_ref()
                                            .map(ToString::to_string)
                                            .unwrap_or_else(|| val.to_string());
                                        self.vals.get_mut(path).unwrap().res =
                                            Some(Err(EvalError::CantAssign { expr, ty }));
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Evald {
    pub expr: Option<Expression>,
    pub res: Option<EvalResult>,
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
