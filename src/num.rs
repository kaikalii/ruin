use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display, Formatter},
    num::ParseFloatError,
    ops::*,
    str::FromStr,
};

pub fn modulo<T>(a: T, b: T) -> T
where
    T: Copy + Rem<Output = T> + Add<Output = T>,
{
    (a % b + b) % b
}

#[derive(Clone, Copy)]
pub enum Num {
    Int(i64),
    Float(f64),
}

impl Debug for Num {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Num::Int(n) => write!(f, "{}", n),
            Num::Float(n) => write!(f, "{}", n),
        }
    }
}

impl Display for Num {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Num::Int(n) => write!(f, "{:?}", n),
            Num::Float(n) => write!(f, "{:?}", n),
        }
    }
}

impl From<i64> for Num {
    fn from(i: i64) -> Num {
        Num::Int(i)
    }
}

impl From<f64> for Num {
    fn from(f: f64) -> Num {
        Num::Float(f)
    }
}

impl From<Num> for f64 {
    fn from(num: Num) -> f64 {
        match num {
            Num::Int(i) => i as f64,
            Num::Float(f) => f,
        }
    }
}

impl PartialEq for Num {
    fn eq(&self, other: &Self) -> bool {
        (f64::from(*self) - f64::from(*other)).abs() < std::f64::EPSILON
    }
}

impl Eq for Num {}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        f64::from(*self).partial_cmp(&f64::from(*other))
    }
}

impl Ord for Num {
    fn cmp(&self, other: &Self) -> Ordering {
        let a = f64::from(*self);
        let b = f64::from(*other);
        match (a.is_nan(), b.is_nan()) {
            (false, false) => a.partial_cmp(&b).unwrap(),
            (false, true) => Ordering::Less,
            (true, false) => Ordering::Greater,
            (true, true) => Ordering::Equal,
        }
    }
}

impl FromStr for Num {
    type Err = ParseFloatError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<i64>() {
            Ok(i) => Ok(Num::Int(i)),
            Err(_) => s.parse::<f64>().map(Num::Float),
        }
    }
}

macro_rules! bin_op {
    ($trait:ident, $method:ident, $function:path) => {
        impl $trait for Num {
            type Output = Self;
            fn $method(self, other: Self) -> Self {
                match (self, other) {
                    (Num::Int(a), Num::Int(b)) => Num::Int($function(a, b)),
                    (Num::Int(a), Num::Float(b)) => Num::Float($function(a as f64, b)),
                    (Num::Float(a), Num::Int(b)) => Num::Float($function(a, b as f64)),
                    (Num::Float(a), Num::Float(b)) => Num::Float($function(a, b)),
                }
            }
        }
    };
}

bin_op!(Add, add, Add::add);
bin_op!(Sub, sub, Sub::sub);
bin_op!(Mul, mul, Mul::mul);
bin_op!(Div, div, Div::div);
bin_op!(Rem, rem, modulo);

impl Neg for Num {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Num::Int(i) => Num::Int(-i),
            Num::Float(f) => Num::Float(-f),
        }
    }
}
