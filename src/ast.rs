use std::fmt;

// Added PartialEq here
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Value),
    Ref(String),
    RelRef(i32, i32),
    Binary(Box<Expr>, Op, Box<Expr>),
    Unary(Op, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Array(Vec<Expr>),
    Dict(Vec<(String, Expr)>),
    Range(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Assign(String, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    Eq,
    Neq,
    Gt,
    Lt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Empty,
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Dict(Vec<(String, Value)>),
    Range(i64, i64),
    Lambda(Vec<String>, Box<Expr>),
    Error(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Empty => write!(f, "empty"),
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(a) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Dict(d) => {
                write!(f, "{{ ")?;
                for (i, (k, v)) in d.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, " }}")
            }
            Value::Range(s, e) => write!(f, "{}..{}", s, e),
            Value::Lambda(_, _) => write!(f, "<fn>"),
            Value::Error(e) => write!(f, "#ERR: {}", e),
        }
    }
}
