use std::{collections::HashSet, ops::Index, sync::Arc};

use colored::Colorize;
use indexmap::IndexMap;

use crate::{ast::*, compile::*, value::*};

#[derive(Debug, Clone, Default)]
pub struct Codebase {
    pub parent: Option<Arc<Self>>,
    pub vals: IndexMap<String, Value>,
    pub declared_functions: HashSet<String>,
}

impl Codebase {
    #[track_caller]
    #[allow(clippy::wrong_self_convention, clippy::needless_lifetimes)]
    pub fn as_mut<'a>(self: &'a mut Arc<Self>) -> &'a mut Self {
        Arc::get_mut(self).expect("Codebase is cloned")
    }
    pub fn from_parent(parent: Arc<Self>) -> Arc<Self> {
        Codebase {
            parent: Some(parent),
            ..Default::default()
        }
        .into()
    }
    pub fn get(&self, ident: &str) -> Option<&Value> {
        self.vals
            .get(ident)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(ident)))
    }
    pub fn insert<V>(&mut self, ident: String, val: V)
    where
        V: Into<Value>,
    {
        // Unassign results that depend on the ident
        self.unassign_results(&ident);
        // Insert
        self.vals.remove(&ident);
        self.vals.insert(ident, val.into());
    }
    pub fn insert_function_decl(&mut self, decl: FunctionDecl) {
        self.declared_functions.insert(decl.ident.clone());
        self.insert(decl.ident, decl.function);
    }
    pub fn print(&self, n: usize) {
        println!();
        if n < self.vals.len() {
            println!("...");
        }
        for (ident, val) in self.vals.iter().rev().take(n).rev() {
            println!("{} = {}", ident.to_string().bold(), val)
        }
        println!();
    }
    pub fn evaled_count(&self) -> usize {
        self.vals
            .values()
            .filter(|val| !val.is_err() && val.is_evald())
            .count()
    }
    pub fn unassign_results(&mut self, ident: &str) {
        let mut idents = vec![ident.to_owned()];
        while !idents.is_empty() {
            for child in idents.drain(..).collect::<Vec<_>>() {
                for (id, val) in &mut self.vals {
                    if val.contains_ident(ident) {
                        val.reset();
                        if child != ident {
                            idents.push(id.clone());
                        }
                    }
                }
            }
        }
    }
    pub fn eval_all(self: &mut Arc<Self>) {
        // Get initial count and idents
        let mut count = self.evaled_count();
        let idents: Vec<_> = self.vals.keys().cloned().collect();
        // Loop until the number of success counts does not change
        loop {
            for ident in &idents {
                self.eval_ident(ident);
            }
            let new_count = self.evaled_count();
            if new_count == count {
                break;
            }
            count = new_count;
        }
    }
    fn eval_ident(self: &mut Arc<Self>, ident: &str) {
        if let Value::Function(function) = &self.vals[ident] {
            if let FunctionBody::Expr { instrs, .. } = &function.body {
                if instrs.lock().unwrap().is_none() {
                    let _ = compile_function(function, &mut CompileState::new(self.clone()), None);
                }
            }
        }
        let evald = eval_ident(self, ident, false);
        if let Some(val) = self.as_mut().vals.get_mut(ident) {
            if let Value::Expression { val, .. } = val {
                *val = Some(evald.into());
            }
        }
    }
}

impl<P> Index<P> for Codebase
where
    P: Into<String>,
{
    type Output = Value;
    fn index(&self, ident: P) -> &Self::Output {
        let ident = ident.into();
        self.vals
            .get(&ident)
            .unwrap_or_else(|| panic!("Unknown val: {}", ident))
    }
}
