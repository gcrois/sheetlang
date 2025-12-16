use crate::ast::{Expr, Op, Value};
use std::collections::HashMap;

// --- Helper for Coordinates ---
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub struct Coord {
    pub col: i32,
    pub row: i32,
}

impl Coord {
    pub fn from_str(s: &str) -> Option<Self> {
        let mut chars = s.chars();
        let col_char = chars.next()?;
        if !col_char.is_ascii_uppercase() {
            return None;
        }

        let row_str: String = chars.collect();
        let row_num = row_str.parse::<i32>().ok()?;

        Some(Coord {
            col: (col_char as u8 - b'A') as i32,
            row: row_num - 1,
        })
    }

    pub fn offset(&self, dx: i32, dy: i32) -> Coord {
        Coord {
            col: self.col + dx,
            row: self.row + dy,
        }
    }
}

// --- The Engine ---
pub struct Engine {
    pub formulas: HashMap<Coord, Expr>,
    pub state_prev: HashMap<Coord, Value>,
    pub state_curr: HashMap<Coord, Value>,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            formulas: HashMap::new(),
            state_prev: HashMap::new(),
            state_curr: HashMap::new(),
        }
    }

    pub fn set_formula(&mut self, key: &str, expr: Expr) -> Result<(), String> {
        if let Some(c) = Coord::from_str(key) {
            self.formulas.insert(c, expr);
            Ok(())
        } else {
            Err(format!("Invalid cell reference {}", key))
        }
    }

    pub fn tick(&mut self) {
        let new_prev = self.state_curr.clone();

        self.state_prev = new_prev;

        let mut next_tick_vals = self.state_prev.clone();

        for (coord, expr) in &self.formulas {
            let val = self.eval(expr, &self.state_prev, Some(*coord), &HashMap::new());
            next_tick_vals.insert(*coord, val);
        }

        self.state_curr = next_tick_vals;
    }

    fn eval(
        &self,
        expr: &Expr,
        grid: &HashMap<Coord, Value>,
        self_coord: Option<Coord>,
        locals: &HashMap<String, Value>,
    ) -> Value {
        match expr {
            Expr::Literal(v) => v.clone(),

            Expr::Ref(name) => {
                if name == "true" {
                    return Value::Bool(true);
                }
                if name == "false" {
                    return Value::Bool(false);
                }
                if let Some(v) = locals.get(name) {
                    return v.clone();
                }
                if let Some(c) = Coord::from_str(name) {
                    // Treat empty/missing cells as 0 for references, like spreadsheets
                    return grid.get(&c).cloned().unwrap_or(Value::Int(0));
                }
                Value::Empty
            }

            Expr::RelRef(dx, dy) => {
                if let Some(c) = self_coord {
                    let target = c.offset(*dx, *dy);
                    grid.get(&target).cloned().unwrap_or(Value::Empty)
                } else {
                    Value::Empty
                }
            }

            Expr::Binary(lhs, op, rhs) => {
                let l = self.eval(lhs, grid, self_coord, locals);
                let r = self.eval(rhs, grid, self_coord, locals);
                self.apply_op(op, l, r)
            }

            Expr::Unary(op, expr) => {
                let val = self.eval(expr, grid, self_coord, locals);
                match op {
                    Op::Not => Value::Bool(!self.is_truthy(&val)),
                    Op::Sub => {
                        if let Value::Int(n) = val {
                            Value::Int(-n)
                        } else {
                            Value::Empty
                        }
                    }
                    _ => val,
                }
            }

            Expr::If(cond, then_branch, else_branch) => {
                let c = self.eval(cond, grid, self_coord, locals);
                if self.is_truthy(&c) {
                    self.eval(then_branch, grid, self_coord, locals)
                } else {
                    self.eval(else_branch, grid, self_coord, locals)
                }
            }

            Expr::Lambda(args, body) => Value::Lambda(args.clone(), body.clone()),

            Expr::Call(func_expr, arg_exprs) => {
                let func = self.eval(func_expr, grid, self_coord, locals);
                let mut arg_vals = Vec::new();
                for a in arg_exprs {
                    arg_vals.push(self.eval(a, grid, self_coord, locals));
                }

                if let Value::Lambda(param_names, body) = func {
                    let mut new_locals = locals.clone();
                    for (i, name) in param_names.iter().enumerate() {
                        if let Some(val) = arg_vals.get(i) {
                            new_locals.insert(name.clone(), val.clone());
                        }
                    }
                    self.eval(&body, grid, self_coord, &new_locals)
                } else {
                    Value::Empty
                }
            }

            Expr::Array(items) => {
                let v = items
                    .iter()
                    .map(|e| self.eval(e, grid, self_coord, locals))
                    .collect();
                Value::Array(v)
            }

            Expr::Dict(items) => {
                let mut d = Vec::new();
                for (k, v_expr) in items {
                    let v = self.eval(v_expr, grid, self_coord, locals);
                    d.push((k.clone(), v));
                }
                Value::Dict(d)
            }

            Expr::Range(start, end) => {
                let s = self.eval(start, grid, self_coord, locals);
                let e = self.eval(end, grid, self_coord, locals);
                if let (Value::Int(s_val), Value::Int(e_val)) = (s, e) {
                    Value::Range(s_val, e_val)
                } else {
                    Value::Empty
                }
            }

            Expr::Member(obj_expr, key) => {
                let obj = self.eval(obj_expr, grid, self_coord, locals);
                if let Value::Dict(pairs) = obj {
                    for (k, v) in pairs {
                        if k == *key {
                            return v;
                        }
                    }
                }
                Value::Empty
            }

            Expr::Index(obj_expr, idx_expr) => {
                let obj = self.eval(obj_expr, grid, self_coord, locals);
                let idx = self.eval(idx_expr, grid, self_coord, locals);
                match (obj, idx) {
                    (Value::Array(arr), Value::Int(i)) => {
                        if i >= 0 && (i as usize) < arr.len() {
                            arr[i as usize].clone()
                        } else {
                            Value::Empty
                        }
                    }
                    (Value::Dict(pairs), Value::String(s)) => {
                        for (k, v) in pairs {
                            if k == s {
                                return v;
                            }
                        }
                        Value::Empty
                    }
                    _ => Value::Empty,
                }
            }

            Expr::Assign(_, _) => Value::Empty,
        }
    }

    fn apply_op(&self, op: &Op, l: Value, r: Value) -> Value {
        match (op, l, r) {
            (Op::Add, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                (Value::String(a), Value::String(b)) => Value::String(a + &b),
                (Value::Empty, _) | (_, Value::Empty) => Value::Empty,
                _ => Value::Error("Invalid types for Add".into()),
            },

            (Op::Sub, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                (Value::Empty, _) | (_, Value::Empty) => Value::Empty,
                _ => Value::Error("Invalid types for Sub".into()),
            },

            (Op::Mul, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                (Value::Empty, _) | (_, Value::Empty) => Value::Empty,
                _ => Value::Error("Invalid types for Mul".into()),
            },

            (Op::Div, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => {
                    if b == 0 {
                        Value::Error("Division by zero".into())
                    } else {
                        Value::Int(a / b)
                    }
                }
                (Value::Empty, _) | (_, Value::Empty) => Value::Empty,
                _ => Value::Error("Invalid types for Div".into()),
            },

            (Op::Eq, a, b) => Value::Bool(a == b),
            (Op::Neq, a, b) => Value::Bool(a != b),

            (Op::Gt, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                (Value::String(a), Value::String(b)) => Value::Bool(a > b),
                _ => Value::Bool(false), // Fallback
            },

            (Op::Lt, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (Value::String(a), Value::String(b)) => Value::Bool(a < b),
                _ => Value::Bool(false), // Fallback
            },

            (Op::And, l, r) => {
                if self.is_truthy(&l) {
                    r
                } else {
                    l
                }
            }
            (Op::Or, l, r) => {
                if self.is_truthy(&l) {
                    l
                } else {
                    r
                }
            }
            (Op::Not, l, _) => Value::Bool(!self.is_truthy(&l)),
        }
    }

    fn is_truthy(&self, v: &Value) -> bool {
        match v {
            Value::Empty => false,
            Value::Int(n) => *n != 0,
            Value::Bool(b) => *b,
            _ => true,
        }
    }
}
