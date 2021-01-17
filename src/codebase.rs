use indexmap::IndexMap;

use crate::{
    parse::{Assignment, Expression},
    types::TypeSet,
};

#[derive(Default)]
pub struct Codebase {
    pub assignments: IndexMap<String, Expression>,
}

impl Codebase {
    pub fn assign(&mut self, ass: Assignment) {
        self.assignments.remove(&ass.ident);
        self.assignments.insert(ass.ident, ass.expr);
    }
    pub fn print(&self, n: usize) {
        println!();
        if n < self.assignments.len() {
            println!("...");
        }
        for (ident, expr) in self.assignments.iter().rev().take(n).rev() {
            println!("{} = {}", ident, expr)
        }
        println!();
    }
}

pub struct Val {
    pub expr: Expression,
    pub ty: TypeSet,
}
