use crate::ast::{Effect, Expr, Op, Value, ViewCapture};
use std::collections::{HashMap, HashSet, VecDeque};

// --- Coordinate and View Helpers ---
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Coord(pub Vec<i32>);

impl Coord {
    pub fn from_str(s: &str) -> Option<Self> {
        // Parse A1-style (single-letter column, 1-based row)
        let mut chars = s.chars();
        let col_char = chars.next()?;
        if !col_char.is_ascii_uppercase() {
            return None;
        }
        let row_str: String = chars.collect();
        let row_num = row_str.parse::<i32>().ok()?;
        if row_num < 1 {
            return None;
        }

        Some(Coord(vec![
            (col_char as u8 - b'A') as i32,
            row_num - 1,
        ]))
    }

    pub fn as_2d(&self) -> Option<(i32, i32)> {
        if self.0.len() == 2 {
            Some((self.0[0], self.0[1]))
        } else {
            None
        }
    }

    pub fn offset(&self, delta: &[i32]) -> Coord {
        let len = self.0.len().max(delta.len());
        let mut out = Vec::with_capacity(len);
        for i in 0..len {
            let base = self.0.get(i).cloned().unwrap_or(0);
            let d = delta.get(i).cloned().unwrap_or(0);
            out.push(base + d);
        }
        Coord(out)
    }

    pub fn to_cell_name(&self) -> String {
        if let Some((col, row)) = self.as_2d() {
            if (0..=25).contains(&col) {
                let col_char = (b'A' + col as u8) as char;
                return format!("{}{}", col_char, row + 1);
            }
        }
        format!("#[{}]", self.0.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","))
    }
}

pub type View = ViewCapture;

impl View {
    pub fn default_for_dims(dims: usize) -> Self {
        let mut offset = vec![0; dims.max(2)];
        if offset.len() < 2 {
            offset.resize(2, 0);
        }
        View {
            axes: [0, 1],
            offset,
        }
    }
}

// Parse a range like "A1:C3" into a list of 2D coordinates
pub fn parse_range(s: &str) -> Option<Vec<Coord>> {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 2 {
        return None;
    }

    let start = Coord::from_str(parts[0])?;
    let end = Coord::from_str(parts[1])?;

    let (start_col, start_row) = start.as_2d()?;
    let (end_col, end_row) = end.as_2d()?;

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

    let mut result = Vec::new();
    for row in start_row..=end_row {
        for col in start_col..=end_col {
            result.push(Coord(vec![col, row]));
        }
    }

    Some(result)
}

// --- Tensor and Engine ---
#[derive(Clone, Debug)]
pub struct Tensor {
    pub shape: Vec<i32>,
    pub formulas: HashMap<Coord, Expr>,
    pub inputs: HashMap<Coord, Value>,
    pub state_prev: HashMap<Coord, Value>,
    pub state_curr: HashMap<Coord, Value>,
}

impl Tensor {
    pub fn new(shape: Vec<i32>) -> Self {
        Self {
            shape,
            formulas: HashMap::new(),
            inputs: HashMap::new(),
            state_prev: HashMap::new(),
            state_curr: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct TickResult {
    pub enqueued_effects: usize,
    pub executed_effects: usize,
}

pub struct Engine {
    pub tensors: HashMap<String, Tensor>,
    pub active: String,
    pub view: View,
    pub pending_effects: VecDeque<Effect>,
    pub effect_auto_budget: usize,
}

impl Engine {
    pub fn new() -> Self {
        let mut tensors = HashMap::new();
        tensors.insert("sheet".to_string(), Tensor::new(Vec::new()));
        Self {
            tensors,
            active: "sheet".to_string(),
            view: View::default_for_dims(2),
            pending_effects: VecDeque::new(),
            effect_auto_budget: 0,
        }
    }

    pub fn alloc_tensor(&mut self, name: &str, shape: Vec<i32>) -> Result<(), String> {
        if self.tensors.contains_key(name) {
            return Err(format!("Tensor '{}' already exists", name));
        }
        self.tensors.insert(name.to_string(), Tensor::new(shape.clone()));
        self.active = name.to_string();
        self.view = View::default_for_dims(shape.len().max(2));
        Ok(())
    }

    pub fn use_tensor(&mut self, name: &str) -> Result<(), String> {
        if !self.tensors.contains_key(name) {
            return Err(format!("Tensor '{}' not found", name));
        }
        self.active = name.to_string();
        let dims = self.active_dims();
        self.view = View::default_for_dims(dims);
        Ok(())
    }

    pub fn set_view_axes(&mut self, axes: [usize; 2]) -> Result<(), String> {
        let dims = self.active_dims();
        if axes[0] >= dims || axes[1] >= dims || axes[0] == axes[1] {
            return Err(format!("Invalid view axes {:?} for dims {}", axes, dims));
        }
        self.view.axes = axes;
        Ok(())
    }

    pub fn set_view_offset(&mut self, offset: Vec<i32>) -> Result<(), String> {
        let dims = self.active_dims();
        if offset.len() != dims {
            return Err(format!("Offset length {} must match dims {}", offset.len(), dims));
        }
        self.view.offset = offset;
        Ok(())
    }

    pub fn set_formula(&mut self, key: &str, expr: Expr) -> Result<(), String> {
        let coord2d = Coord::from_str(key).ok_or_else(|| format!("Invalid cell reference {}", key))?;
        let coord = self.map_view_coord_with(&self.view, &self.active, &coord2d)?;
        self.set_formula_coord(coord, expr);
        Ok(())
    }

    pub fn set_formula_coord(&mut self, coord: Coord, expr: Expr) {
        if let Some(tensor) = self.tensors.get_mut(&self.active) {
            tensor.formulas.insert(coord, expr);
        }
    }

    pub fn set_input(&mut self, key: &str, value: Value) -> Result<(), String> {
        let coord2d = Coord::from_str(key).ok_or_else(|| format!("Invalid cell reference {}", key))?;
        let coord = self.map_view_coord_with(&self.view, &self.active, &coord2d)?;
        self.set_input_coord(coord, value);
        self.recompute_active();
        Ok(())
    }

    pub fn set_input_coord(&mut self, coord: Coord, value: Value) {
        if let Some(tensor) = self.tensors.get_mut(&self.active) {
            tensor.inputs.insert(coord, value);
        }
    }

    pub fn clear_input(&mut self, key: &str) -> Result<(), String> {
        let coord2d = Coord::from_str(key).ok_or_else(|| format!("Invalid cell reference {}", key))?;
        let coord = self.map_view_coord_with(&self.view, &self.active, &coord2d)?;
        if let Some(tensor) = self.tensors.get_mut(&self.active) {
            tensor.inputs.remove(&coord);
        }
        self.recompute_active();
        Ok(())
    }

    pub fn tick(&mut self) -> TickResult {
        let effects = self.tick_internal(None);
        let enqueued = effects.len();
        self.pending_effects.extend(effects);
        let executed = if self.effect_auto_budget > 0 {
            self.run_effects_internal(self.effect_auto_budget)
        } else {
            0
        };
        TickResult {
            enqueued_effects: enqueued,
            executed_effects: executed,
        }
    }

    pub fn tick_range(&mut self, coords: &[Coord]) -> TickResult {
        let effects = self.tick_internal(Some(coords));
        let enqueued = effects.len();
        self.pending_effects.extend(effects);
        let executed = if self.effect_auto_budget > 0 {
            self.run_effects_internal(self.effect_auto_budget)
        } else {
            0
        };
        TickResult {
            enqueued_effects: enqueued,
            executed_effects: executed,
        }
    }

    pub fn pending_effects_len(&self) -> usize {
        self.pending_effects.len()
    }

    pub fn run_effects(&mut self, limit: usize) -> usize {
        self.run_effects_internal(limit)
    }

    pub fn recompute_active(&mut self) {
        let tensor_name = self.active.clone();
        let (formulas, inputs, state_prev) = {
            let tensor = self
                .tensors
                .get(&tensor_name)
                .expect("active tensor missing");
            (tensor.formulas.clone(), tensor.inputs.clone(), tensor.state_prev.clone())
        };

        let mut next = state_prev.clone();
        for (coord, expr) in formulas {
            if inputs.contains_key(&coord) {
                continue;
            }
            let val = self.eval(&expr, &tensor_name, &self.view, Some(&coord), &HashMap::new());
            next.insert(coord, val);
        }
        for (coord, val) in inputs {
            next.insert(coord, val);
        }

        if let Some(tensor) = self.tensors.get_mut(&tensor_name) {
            tensor.state_curr = next;
        }
    }

    pub fn get_values_in_range(&self, coords: &[Coord]) -> Vec<(Coord, Value)> {
        let tensor = self.tensors.get(&self.active).expect("active tensor missing");
        coords
            .iter()
            .filter_map(|coord2d| {
                let nd = self
                    .map_view_coord_with(&self.view, &self.active, coord2d)
                    .ok()?;
                tensor
                    .state_curr
                    .get(&nd)
                    .map(|val| (coord2d.clone(), val.clone()))
            })
            .collect()
    }

    pub fn get_all_coords_in_view(&self) -> Vec<Coord> {
        let tensor = self.tensors.get(&self.active).expect("active tensor missing");
        let mut coords = Vec::new();
        for coord in tensor.state_curr.keys() {
            if let Some(view_coord) = self.project_to_view(coord) {
                coords.push(view_coord);
            }
        }
        coords
    }

    pub fn get_all_coords(&self) -> Vec<Coord> {
        let tensor = self.tensors.get(&self.active).expect("active tensor missing");
        tensor.state_curr.keys().cloned().collect()
    }

    fn dims_for(&self, tensor_name: &str, view: &View) -> usize {
        let tensor = self
            .tensors
            .get(tensor_name)
            .expect("active tensor missing");
        let max_axis = view.axes.iter().copied().max().unwrap_or(1);
        tensor
            .shape
            .len()
            .max(view.offset.len())
            .max(max_axis + 1)
            .max(2)
    }

    fn active_dims(&self) -> usize {
        self.dims_for(&self.active, &self.view)
    }

    fn map_view_coord_with(
        &self,
        view: &View,
        tensor_name: &str,
        coord2d: &Coord,
    ) -> Result<Coord, String> {
        let (col, row) = coord2d
            .as_2d()
            .ok_or_else(|| "Expected 2D view coordinate".to_string())?;
        let dims = self.dims_for(tensor_name, view);
        let mut out = vec![0; dims];
        for i in 0..dims {
            if let Some(v) = view.offset.get(i) {
                out[i] = *v;
            }
        }
        out[view.axes[0]] = col;
        out[view.axes[1]] = row;
        Ok(Coord(out))
    }

    pub fn map_view_coord(&self, coord2d: &Coord) -> Result<Coord, String> {
        self.map_view_coord_with(&self.view, &self.active, coord2d)
    }

    fn project_to_view(&self, coord: &Coord) -> Option<Coord> {
        let dims = self.active_dims();
        let mut normalized = coord.0.clone();
        if normalized.len() < dims {
            normalized.resize(dims, 0);
        }

        for i in 0..dims {
            if i == self.view.axes[0] || i == self.view.axes[1] {
                continue;
            }
            let expected = self.view.offset.get(i).cloned().unwrap_or(0);
            if normalized[i] != expected {
                return None;
            }
        }

        Some(Coord(vec![
            normalized[self.view.axes[0]],
            normalized[self.view.axes[1]],
        ]))
    }

    fn tick_internal(&mut self, coords: Option<&[Coord]>) -> Vec<Effect> {
        let tensor_name = self.active.clone();
        // Advance temporal state
        if let Some(tensor) = self.tensors.get_mut(&tensor_name) {
            tensor.state_prev = tensor.state_curr.clone();
        }

        let (formulas, inputs, state_prev) = {
            let tensor = self
                .tensors
                .get(&tensor_name)
                .expect("active tensor missing");
            (tensor.formulas.clone(), tensor.inputs.clone(), tensor.state_prev.clone())
        };

        let allowed: Option<HashSet<Coord>> = coords.map(|c| c.iter().cloned().collect());

        let mut next = state_prev.clone();
        let mut effects = Vec::new();
        for (coord, expr) in formulas {
            if let Some(allowed) = &allowed {
                if !allowed.contains(&coord) {
                    continue;
                }
            }
            if inputs.contains_key(&coord) {
                continue;
            }
            let val = self.eval(&expr, &tensor_name, &self.view, Some(&coord), &HashMap::new());
            if let Value::Effect(effect) = &val {
                effects.push(effect.clone());
            }
            next.insert(coord, val);
        }

        for (coord, val) in inputs {
            next.insert(coord, val);
        }

        if let Some(tensor) = self.tensors.get_mut(&tensor_name) {
            tensor.state_curr = next;
        }

        effects
    }

    fn run_effects_internal(&mut self, limit: usize) -> usize {
        let mut executed = 0;
        while executed < limit {
            let effect = match self.pending_effects.pop_front() {
                Some(e) => e,
                None => break,
            };
            executed += 1;
            match effect {
                Effect::Tick => {
                    let new_effects = self.tick_internal(None);
                    self.pending_effects.extend(new_effects);
                }
                Effect::TickRange(range) => {
                    let coords_2d: Vec<Coord> = range
                        .expand()
                        .iter()
                        .filter_map(|s| Coord::from_str(s))
                        .collect();
                    let mut coords = Vec::new();
                    for c in coords_2d {
                    if let Ok(mapped) = self.map_view_coord_with(&self.view, &self.active, &c) {
                        coords.push(mapped);
                    }
                }
                    let new_effects = self.tick_internal(Some(&coords));
                    self.pending_effects.extend(new_effects);
                }
            }
        }
        executed
    }

    fn eval(
        &self,
        expr: &Expr,
        tensor_name: &str,
        view: &View,
        self_coord: Option<&Coord>,
        locals: &HashMap<String, Value>,
    ) -> Value {
        let tensor = match self.tensors.get(tensor_name) {
            Some(t) => t,
            None => return Value::Error(format!("Tensor '{}' not found", tensor_name)),
        };

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
                if let Some(c2d) = Coord::from_str(name) {
                    if let Ok(coord) = self.map_view_coord_with(view, tensor_name, &c2d) {
                        return self.read_cell_value(tensor, &coord, Value::Int(0));
                    }
                }
                Value::Empty
            }

            Expr::AbsRef(coords) => {
                let coord = Coord(coords.clone());
                self.read_cell_value(tensor, &coord, Value::Int(0))
            }

            Expr::RelRef(delta) => {
                if let Some(c) = self_coord {
                    let target = c.offset(delta);
                    self.read_cell_value(tensor, &target, Value::Empty)
                } else {
                    Value::Empty
                }
            }

            Expr::Binary(lhs, op, rhs) => {
                let l = self.eval(lhs, tensor_name, view, self_coord, locals);
                let r = self.eval(rhs, tensor_name, view, self_coord, locals);
                self.apply_op(op, l, r)
            }

            Expr::Unary(op, expr) => {
                let val = self.eval(expr, tensor_name, view, self_coord, locals);
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
                let c = self.eval(cond, tensor_name, view, self_coord, locals);
                if self.is_truthy(&c) {
                    self.eval(then_branch, tensor_name, view, self_coord, locals)
                } else {
                    self.eval(else_branch, tensor_name, view, self_coord, locals)
                }
            }

            Expr::Lambda(args, body) => Value::Closure {
                params: args.clone(),
                body: body.clone(),
                env: locals.clone(),
                bound: Vec::new(),
                tensor: tensor_name.to_string(),
                view: view.clone(),
            },

            Expr::Call(func_expr, arg_exprs) => {
                if let Expr::Ref(name) = &**func_expr {
                    if name == "tick" && arg_exprs.is_empty() {
                        return Value::Effect(Effect::Tick);
                    }
                }

                let func = self.eval(func_expr, tensor_name, view, self_coord, locals);
                let mut arg_vals = Vec::new();
                for a in arg_exprs {
                    arg_vals.push(self.eval(a, tensor_name, view, self_coord, locals));
                }

                self.apply_call(func, arg_vals, tensor_name, self_coord)
            }

            Expr::Array(items) => {
                let v = items
                    .iter()
                    .map(|e| self.eval(e, tensor_name, view, self_coord, locals))
                    .collect();
                Value::Array(v)
            }

            Expr::Dict(items) => {
                let mut d = Vec::new();
                for (k, v_expr) in items {
                    let v = self.eval(v_expr, tensor_name, view, self_coord, locals);
                    d.push((k.clone(), v));
                }
                Value::Dict(d)
            }

            Expr::Range(start, end) => {
                let s = self.eval(start, tensor_name, view, self_coord, locals);
                let e = self.eval(end, tensor_name, view, self_coord, locals);
                if let (Value::Int(s_val), Value::Int(e_val)) = (s, e) {
                    Value::Range(s_val, e_val)
                } else {
                    Value::Empty
                }
            }

            Expr::Member(obj_expr, key) => {
                let obj = self.eval(obj_expr, tensor_name, view, self_coord, locals);
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
                let obj = self.eval(obj_expr, tensor_name, view, self_coord, locals);
                let idx = self.eval(idx_expr, tensor_name, view, self_coord, locals);
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
            Expr::RangeAssign(_, _) => Value::Empty,
        }
    }

    fn apply_call(
        &self,
        func: Value,
        args: Vec<Value>,
        caller_tensor: &str,
        self_coord: Option<&Coord>,
    ) -> Value {
        match func {
            Value::Closure {
                params,
                body,
                env,
                bound,
                tensor,
                view,
            } => {
                let mut all_args = bound;
                all_args.extend(args);

                if all_args.len() < params.len() {
                    return Value::Closure {
                        params,
                        body,
                        env,
                        bound: all_args,
                        tensor,
                        view,
                    };
                }

                let mut new_locals = env.clone();
                for (i, name) in params.iter().enumerate() {
                    if let Some(val) = all_args.get(i) {
                        new_locals.insert(name.clone(), val.clone());
                    }
                }

                let use_self_coord = if tensor == caller_tensor {
                    self_coord
                } else {
                    None
                };

                let result = self.eval(&body, &tensor, &view, use_self_coord, &new_locals);

                if all_args.len() > params.len() {
                    let rest = all_args[params.len()..].to_vec();
                    return self.apply_call(result, rest, caller_tensor, self_coord);
                }

                result
            }
            _ => Value::Empty,
        }
    }

    fn read_cell_value(&self, tensor: &Tensor, coord: &Coord, default: Value) -> Value {
        if let Some(v) = tensor.inputs.get(coord) {
            return v.clone();
        }
        tensor
            .state_prev
            .get(coord)
            .cloned()
            .unwrap_or(default)
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
                _ => Value::Bool(false),
            },

            (Op::Lt, l, r) => match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (Value::String(a), Value::String(b)) => Value::Bool(a < b),
                _ => Value::Bool(false),
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
