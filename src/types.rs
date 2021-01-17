use std::collections::BTreeSet;

use derive_more::Display;

use crate::parse::*;

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "number")]
    Number,
    #[display(fmt = "string")]
    String,
    #[display(fmt = "list")]
    List,
    #[display(fmt = "table")]
    Table,
    #[display(fmt = "error")]
    Error,
    #[display(fmt = "nil")]
    Nil,
}

#[derive(Debug, Display, Clone)]
pub enum TypeSet {
    #[display(fmt = "?")]
    Unknown,
    #[display(fmt = "{}", "format_typeset(_0)")]
    Set(BTreeSet<Type>),
}

fn format_typeset(set: &BTreeSet<Type>) -> String {
    let mut s = String::new();
    for (i, ty) in set.iter().enumerate() {
        s.push_str(&ty.to_string());
        if i < s.len() - 1 {
            s.push_str(" | ");
        }
    }
    s
}
