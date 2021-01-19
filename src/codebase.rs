use std::rc::Rc;

use colored::Colorize;
use indexmap::IndexMap;

use crate::{eval::*, parse::*, value::*};

#[derive(Debug, Clone, Default)]
pub struct Codebase {
    pub parent: Option<Rc<Self>>,
    pub vals: IndexMap<Path, Value>,
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
    pub fn get(&self, path: &Path) -> Option<&Value> {
        self.vals
            .get(path)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(path)))
    }
    pub fn insert<V>(&mut self, path: Path, val: V)
    where
        V: Into<Value>,
    {
        // Unassign results that depend on the path
        self.unassign_results(&path);
        // Insert
        self.vals.remove(&path);
        self.vals.insert(path, val.into());
    }
    pub fn print(&self, n: usize) {
        println!();
        if n < self.vals.len() {
            println!("...");
        }
        for (path, val) in self.vals.iter().rev().take(n).rev() {
            println!("{} = {}", path.to_string().bold(), val)
        }
        println!();
    }
    pub fn evaled_count(&self) -> usize {
        self.vals
            .values()
            .filter(|val| !val.is_err() && val.is_evald())
            .count()
    }
    pub fn unassign_results(&mut self, path: &Path) {
        let mut paths = vec![path.to_owned()];
        while !paths.is_empty() {
            for child in paths.drain(..).collect::<Vec<_>>() {
                for (id, val) in &mut self.vals {
                    let ident = path.name.as_ref().unwrap();
                    if val.contains_ident(ident) {
                        val.reset();
                        if &child != path {
                            paths.push(id.clone());
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
        let evald = eval(self, path);
        if let Some(val) = self.vals.get_mut(path) {
            if let Value::Expression { val, .. } = val {
                *val = Some(evald.into());
            }
        }
        if !path.disam.is_empty() {
            if let Some(parent) = path.parent() {
                self.eval_path(&parent);
                if let Some(child_val) = self.vals.get(path).cloned() {
                    if let Some(parent_val) = self.vals.get_mut(&parent) {
                        match parent_val {
                            Value::Function(function) => {
                                let function = Rc::make_mut(function);
                                function.env =
                                    function.env.insert(path.name.clone().unwrap(), child_val);
                            }

                            Value::Table(table) => {
                                *table = table
                                    .insert(Key::String(path.name.clone().unwrap()), child_val);
                            }
                            parent_val => {
                                let ty = parent_val.ty();
                                let expr = parent_val.to_string();
                                *self.vals.get_mut(path).unwrap() =
                                    Err(EvalError::CantAssign { expr, ty }).into();
                            }
                        }
                    }
                }
            }
        }
    }
}
