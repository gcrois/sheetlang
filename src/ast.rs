use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Value),
    Ref(String),
    AbsRef(Vec<i32>),
    TensorAbsRef(String, Vec<i32>),
    RelRef(Vec<i32>),
    Binary(Box<Expr>, Op, Box<Expr>),
    Unary(Op, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Array(Vec<Expr>),
    Dict(Vec<(String, Expr)>),
    Range(Box<Expr>, Box<Expr>),
    CellRange(CellRange),
    Member(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Assign(String, Box<Expr>),
    AssignRel(Vec<i32>, Box<Expr>),
    AssignAbs(Vec<i32>, Box<Expr>),
    AssignAll(Option<String>, Box<Expr>),
    RangeAssign(CellRange, Box<Expr>),
    Effect(Box<Effect>),
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

    pub fn indices(&self) -> (i32, i32) {
        (self.col_index(), self.row - 1)
    }
}

// Represents a cell range with optional step
#[derive(Clone, Debug, PartialEq)]
pub struct CellRange {
    pub start: CellCoord,
    pub end: CellCoord,
    pub step: Option<i32>,  // Step size (default 1)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Alloc { name: String, shape: Vec<i32> },
    Use { name: String },
    View { axes: Option<Vec<i32>>, offset: Option<Vec<i32>> },
    SetInput { target: String, value: Expr },
    Effects(EffectsCommand),
    Tick(Option<CellRange>),
    Show(Option<CellRange>),
    BShow(Option<CellRange>),
    Demo(u8),
    Help,
    Encode,
    Exit,
    Eval(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum EffectsCommand {
    Auto(usize),
    Run(Option<usize>),
    Pending,
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
    Command { origin: Option<(String, Vec<i32>)>, command: Box<Command> },
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
            Value::Closure { params, body, env, bound, .. } => {
                let mut subs = HashMap::new();
                for (idx, param) in params.iter().enumerate() {
                    if let Some(val) = bound.get(idx) {
                        subs.insert(param.clone(), val.clone());
                    }
                }
                for (key, val) in env {
                    subs.entry(key.clone()).or_insert_with(|| val.clone());
                }

                let params_display = params
                    .iter()
                    .enumerate()
                    .map(|(idx, name)| {
                        bound
                            .get(idx)
                            .map(|v| v.to_string())
                            .unwrap_or_else(|| name.clone())
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let body_display = if subs.is_empty() {
                    body.to_string()
                } else {
                    format_expr_with_subs(body, &subs)
                };

                write!(f, "({}) => {}", params_display, body_display)
            }
            Value::Effect(effect) => write!(f, "{}", effect),
            Value::Error(e) => write!(f, "#ERR: {}", e),
        }
    }
}

impl fmt::Display for CellCoord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Display for CellRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.step {
            Some(step) => write!(f, "{}:{}:{}", self.start, self.end, step),
            None => write!(f, "{}:{}", self.start, self.end),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::And => "&&",
            Op::Or => "||",
            Op::Not => "!",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::Gt => ">",
            Op::Lt => "<",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(v) => write!(f, "{}", v),
            Expr::Ref(name) => write!(f, "{}", name),
            Expr::AbsRef(coords) => {
                write!(f, "#[{}]", coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","))
            }
            Expr::TensorAbsRef(name, coords) => {
                write!(
                    f,
                    "{}#[{}]",
                    name,
                    coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
                )
            }
            Expr::RelRef(coords) => {
                write!(f, "@[{}]", coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","))
            }
            Expr::Binary(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Unary(op, expr) => write!(f, "({}{})", op, expr),
            Expr::If(cond, then_branch, else_branch) => {
                write!(f, "if {} then {} else {}", cond, then_branch, else_branch)
            }
            Expr::Array(items) => {
                let inner = items.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "[{}]", inner)
            }
            Expr::Dict(items) => {
                let inner = items
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{ {} }}", inner)
            }
            Expr::Range(start, end) => write!(f, "{}..{}", start, end),
            Expr::CellRange(range) => write!(f, "{}", range),
            Expr::Member(obj, key) => write!(f, "{}.{}", obj, key),
            Expr::Index(obj, idx) => write!(f, "{}[{}]", obj, idx),
            Expr::Lambda(args, body) => write!(f, "({}) => {}", args.join(", "), body),
            Expr::Call(func, args) => {
                let inner = args.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{}({})", func, inner)
            }
            Expr::Assign(target, body) => write!(f, "{} = {}", target, body),
            Expr::AssignRel(coords, body) => {
                write!(f, "@[{}] = {}", coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","), body)
            }
            Expr::AssignAbs(coords, body) => {
                write!(f, "#[{}] = {}", coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","), body)
            }
            Expr::AssignAll(tensor, body) => {
                if let Some(name) = tensor {
                    write!(f, "{}#[*] = {}", name, body)
                } else {
                    write!(f, "#[*] = {}", body)
                }
            }
            Expr::RangeAssign(range, body) => write!(f, "{} = {}", range, body),
            Expr::Effect(effect) => write!(f, "{}", effect),
        }
    }
}

fn format_expr_with_subs(expr: &Expr, subs: &HashMap<String, Value>) -> String {
    format_expr_with_subs_inner(expr, subs, &HashSet::new())
}

fn format_expr_with_subs_inner(
    expr: &Expr,
    subs: &HashMap<String, Value>,
    shadowed: &HashSet<String>,
) -> String {
    match expr {
        Expr::Literal(v) => v.to_string(),
        Expr::Ref(name) => {
            if !shadowed.contains(name) {
                if let Some(val) = subs.get(name) {
                    return val.to_string();
                }
            }
            name.clone()
        }
        Expr::AbsRef(coords) => format!(
            "#[{}]",
            coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
        ),
        Expr::TensorAbsRef(name, coords) => format!(
            "{}#[{}]",
            name,
            coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
        ),
        Expr::RelRef(coords) => format!(
            "@[{}]",
            coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
        ),
        Expr::Binary(lhs, op, rhs) => format!(
            "({} {} {})",
            format_expr_with_subs_inner(lhs, subs, shadowed),
            op,
            format_expr_with_subs_inner(rhs, subs, shadowed)
        ),
        Expr::Unary(op, expr) => format!(
            "({}{})",
            op,
            format_expr_with_subs_inner(expr, subs, shadowed)
        ),
        Expr::If(cond, then_branch, else_branch) => format!(
            "if {} then {} else {}",
            format_expr_with_subs_inner(cond, subs, shadowed),
            format_expr_with_subs_inner(then_branch, subs, shadowed),
            format_expr_with_subs_inner(else_branch, subs, shadowed)
        ),
        Expr::Array(items) => format!(
            "[{}]",
            items
                .iter()
                .map(|e| format_expr_with_subs_inner(e, subs, shadowed))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Expr::Dict(items) => format!(
            "{{ {} }}",
            items
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_expr_with_subs_inner(v, subs, shadowed)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Expr::Range(start, end) => format!(
            "{}..{}",
            format_expr_with_subs_inner(start, subs, shadowed),
            format_expr_with_subs_inner(end, subs, shadowed)
        ),
        Expr::CellRange(range) => range.to_string(),
        Expr::Member(obj, key) => format!(
            "{}.{}",
            format_expr_with_subs_inner(obj, subs, shadowed),
            key
        ),
        Expr::Index(obj, idx) => format!(
            "{}[{}]",
            format_expr_with_subs_inner(obj, subs, shadowed),
            format_expr_with_subs_inner(idx, subs, shadowed)
        ),
        Expr::Lambda(args, body) => {
            let mut next_shadowed = shadowed.clone();
            for arg in args {
                next_shadowed.insert(arg.clone());
            }
            format!(
                "({}) => {}",
                args.join(", "),
                format_expr_with_subs_inner(body, subs, &next_shadowed)
            )
        }
        Expr::Call(func, args) => format!(
            "{}({})",
            format_expr_with_subs_inner(func, subs, shadowed),
            args
                .iter()
                .map(|e| format_expr_with_subs_inner(e, subs, shadowed))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Expr::Assign(target, body) => format!(
            "{} = {}",
            target,
            format_expr_with_subs_inner(body, subs, shadowed)
        ),
        Expr::AssignRel(coords, body) => format!(
            "@[{}] = {}",
            coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","),
            format_expr_with_subs_inner(body, subs, shadowed)
        ),
        Expr::AssignAbs(coords, body) => format!(
            "#[{}] = {}",
            coords.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","),
            format_expr_with_subs_inner(body, subs, shadowed)
        ),
        Expr::AssignAll(tensor, body) => {
            let prefix = tensor.as_ref().map(|t| format!("{}#", t)).unwrap_or_else(|| "#".to_string());
            format!(
                "{}[*] = {}",
                prefix,
                format_expr_with_subs_inner(body, subs, shadowed)
            )
        }
        Expr::RangeAssign(range, body) => format!(
            "{} = {}",
            range,
            format_expr_with_subs_inner(body, subs, shadowed)
        ),
        Expr::Effect(effect) => effect.to_string(),
    }
}

impl fmt::Display for EffectsCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EffectsCommand::Auto(n) => write!(f, "effects auto {}", n),
            EffectsCommand::Run(Some(n)) => write!(f, "effects run {}", n),
            EffectsCommand::Run(None) => write!(f, "effects run"),
            EffectsCommand::Pending => write!(f, "effects pending"),
        }
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Command::Alloc { name, shape } => {
                let dims = shape.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "alloc {} [{}]", name, dims)
            }
            Command::Use { name } => write!(f, "use {}", name),
            Command::View { axes, offset } => {
                match (axes, offset) {
                    (Some(axes), Some(offset)) => {
                        let axes = axes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                        let offset = offset.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                        write!(f, "view axes [{}] offset [{}]", axes, offset)
                    }
                    (Some(axes), None) => {
                        let axes = axes.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                        write!(f, "view axes [{}]", axes)
                    }
                    (None, Some(offset)) => {
                        let offset = offset.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
                        write!(f, "view offset [{}]", offset)
                    }
                    (None, None) => write!(f, "view"),
                }
            }
            Command::SetInput { target, value } => write!(f, "set {} {}", target, value),
            Command::Effects(cmd) => write!(f, "{}", cmd),
            Command::Tick(Some(range)) => write!(f, "tick {}", range),
            Command::Tick(None) => write!(f, "tick"),
            Command::Show(Some(range)) => write!(f, "show {}", range),
            Command::Show(None) => write!(f, "show"),
            Command::BShow(Some(range)) => write!(f, "bshow {}", range),
            Command::BShow(None) => write!(f, "bshow"),
            Command::Demo(n) => write!(f, "demo {}", n),
            Command::Help => write!(f, "help"),
            Command::Encode => write!(f, "encode"),
            Command::Exit => write!(f, "exit"),
            Command::Eval(expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Effect::Tick => write!(f, "effect {{ tick }}"),
            Effect::TickRange(range) => write!(f, "effect {{ tick {} }}", range),
            Effect::Command { command, .. } => write!(f, "effect {{ {} }}", command),
        }
    }
}
