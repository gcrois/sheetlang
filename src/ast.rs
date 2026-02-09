use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Value),
    Ref(String),
    AbsRef(Vec<i32>),
    RelRef(Vec<i32>),
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
    AssignAbs(Vec<i32>, Box<Expr>),
    RangeAssign(CellRange, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ViewCapture {
    pub axes: [usize; 2],
    pub offset: Vec<i32>,
}

// Represents a single cell coordinate (A1, B2, etc.)
#[derive(Clone, Debug, PartialEq)]
pub struct CellCoord {
    pub col: String,  // "A", "B", "C", ..., "Z"
    pub row: i32,     // 1-based row number
}

impl CellCoord {
    // Parse "A1" → CellCoord { col: "A", row: 1 }
    pub fn from_str(s: &str) -> Option<Self> {
        let mut chars = s.chars();
        let col_char = chars.next()?;

        // Must be single uppercase letter A-Z
        if !col_char.is_ascii_uppercase() {
            return None;
        }

        let row_str: String = chars.collect();

        // Must have digits, no more letters
        if row_str.is_empty() || !row_str.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }

        let row_num = row_str.parse::<i32>().ok()?;
        if row_num < 1 {
            return None;
        }

        Some(CellCoord {
            col: col_char.to_string(),
            row: row_num,
        })
    }

    pub fn to_string(&self) -> String {
        format!("{}{}", self.col, self.row)
    }

    fn col_index(&self) -> i32 {
        // "A" → 0, "B" → 1, etc.
        (self.col.chars().next().unwrap() as u8 - b'A') as i32
    }
}

// Represents a cell range with optional step
#[derive(Clone, Debug, PartialEq)]
pub struct CellRange {
    pub start: CellCoord,
    pub end: CellCoord,
    pub step: Option<i32>,  // Step size (default 1)
}

impl CellRange {
    // Parse "A1:A5" or "A1:C3" or "A1:A10:2"
    pub fn from_str(s: &str) -> Option<Self> {
        let parts: Vec<&str> = s.split(':').collect();

        if parts.len() < 2 || parts.len() > 3 {
            return None;
        }

        let start = CellCoord::from_str(parts[0])?;
        let end = CellCoord::from_str(parts[1])?;
        let step = if parts.len() == 3 {
            Some(parts[2].parse::<i32>().ok()?)
        } else {
            None
        };

        Some(CellRange { start, end, step })
    }

    // Expand range into list of cell coordinates
    pub fn expand(&self) -> Vec<String> {
        let mut result = Vec::new();

        let start_col = self.start.col_index();
        let end_col = self.end.col_index();
        let start_row = self.start.row;
        let end_row = self.end.row;
        let step = self.step.unwrap_or(1);

        if step <= 0 {
            return result;  // Invalid step
        }

        // Auto-swap if backwards
        let (start_col, end_col) = if start_col <= end_col {
            (start_col, end_col)
        } else {
            (end_col, start_col)
        };

        let (start_row, end_row) = if start_row <= end_row {
            (start_row, end_row)
        } else {
            (end_row, start_row)
        };

        if start_col == end_col {
            // 1D vertical: A1:A5
            let col_name = ((start_col as u8) + b'A') as char;
            let mut row = start_row;
            while row <= end_row {
                result.push(format!("{}{}", col_name, row));
                row += step;
            }
        } else if start_row == end_row {
            // 1D horizontal: A1:E1
            let mut col = start_col;
            while col <= end_col {
                let col_name = ((col as u8) + b'A') as char;
                result.push(format!("{}{}", col_name, start_row));
                col += step;
            }
        } else {
            // 2D rectangular: A1:C3
            // Row-major order (fill rows first)
            let mut row = start_row;
            while row <= end_row {
                let mut col = start_col;
                while col <= end_col {
                    let col_name = ((col as u8) + b'A') as char;
                    result.push(format!("{}{}", col_name, row));
                    col += step;
                }
                row += step;
            }
        }

        result
    }
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
    Closure {
        params: Vec<String>,
        body: Box<Expr>,
        env: HashMap<String, Value>,
        bound: Vec<Value>,
        tensor: String,
        view: ViewCapture,
    },
    Effect(Effect),
    Error(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Effect {
    Tick,
    TickRange(CellRange),
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
            Value::Closure { .. } => write!(f, "<fn>"),
            Value::Effect(_) => write!(f, "<effect>"),
            Value::Error(e) => write!(f, "#ERR: {}", e),
        }
    }
}
