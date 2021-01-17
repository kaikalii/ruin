#![allow(dead_code)]

use rpds::{RedBlackTreeMap, Vector};

use crate::num::Num;

pub type List = Vector<Value>;
pub type Table = RedBlackTreeMap<Value, Value>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Nil,
    Num(Num),
    Bool(bool),
    List(List),
    Table(Table),
}
