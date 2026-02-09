use wasm_bindgen::prelude::*;
use crate::interpreter::{Coord, Engine};
use crate::parser::parser;
use crate::lexer::Token;
use crate::ast::{Expr, IndexSpec};
use chumsky::Parser;
use logos::Logos;
use chumsky::input::Stream;
use serde::Serialize;
use std::collections::HashSet;

#[wasm_bindgen]
pub struct Sheet {
    engine: Engine,
}

#[wasm_bindgen]
impl Sheet {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            engine: Engine::new(),
        }
    }

    pub fn set_formula(&mut self, cell: &str, input: &str) -> Result<(), String> {
        let token_iter = Token::lexer(input)
            .spanned()
            .map(|(tok, _span)| tok.unwrap_or(Token::Dot));

        let token_stream = chumsky::input::Stream::from_iter(token_iter);

        match parser().parse(token_stream).into_result() {
            Ok(expr) => {
                self.engine.set_formula(cell, expr)
            }
            Err(errs) => {
                Err(format!("Parse error: {:?}", errs))
            }
        }
    }

    pub fn tick(&mut self) {
        self.engine.tick();
    }

    pub fn tick_range(&mut self, range: &str) -> Result<(), String> {
        let coords = crate::interpreter::parse_range(range)
            .ok_or_else(|| format!("Invalid range: {}", range))?;
        let coords_nd: Vec<Coord> = coords
            .iter()
            .filter_map(|c| self.engine.map_view_coord(c).ok())
            .collect();
        self.engine.tick_range(&coords_nd);
        Ok(())
    }

    pub fn get_value(&self, cell: &str) -> String {
         if let Some(coord2d) = crate::interpreter::Coord::from_str(cell) {
             if let Ok(coord) = self.engine.map_view_coord(&coord2d) {
                 if let Some(tensor) = self.engine.tensors.get(&self.engine.active) {
                     return tensor
                         .state_curr
                         .get(&coord)
                         .map(|v| v.to_string())
                         .unwrap_or_else(|| "empty".to_string());
                 }
             }
             "empty".to_string()
         } else {
             "error".to_string()
         }
    }

    pub fn get_all_values(&self) -> String {
        let mut out = String::new();
        let coords = self.engine.get_all_coords_in_view();
        let values = self.engine.get_values_in_range(&coords);
        let mut sorted_values = values.clone();
        sorted_values.sort_by(|(a, _), (b, _)| {
            let (a_col, a_row) = a.as_2d().unwrap_or((0, 0));
            let (b_col, b_row) = b.as_2d().unwrap_or((0, 0));
            let row_cmp = a_row.cmp(&b_row);
            if row_cmp == std::cmp::Ordering::Equal {
                a_col.cmp(&b_col)
            } else {
                row_cmp
            }
        });

        for (coord, val) in sorted_values {
            out.push_str(&format!("{}: {}\n", coord.to_cell_name(), val));
        }
        out
    }

    pub fn get_values_in_range(&self, range: &str) -> String {
        let coords = match crate::interpreter::parse_range(range) {
            Some(c) => c,
            None => return format!("error: Invalid range {}", range),
        };

        let mut out = String::new();
        let values = self.engine.get_values_in_range(&coords);
        let mut sorted_values = values.clone();
        sorted_values.sort_by(|(a, _), (b, _)| {
            let (a_col, a_row) = a.as_2d().unwrap_or((0, 0));
            let (b_col, b_row) = b.as_2d().unwrap_or((0, 0));
            let row_cmp = a_row.cmp(&b_row);
            if row_cmp == std::cmp::Ordering::Equal {
                a_col.cmp(&b_col)
            } else {
                row_cmp
            }
        });

        for (coord, val) in sorted_values {
            out.push_str(&format!("{}: {}\n", coord.to_cell_name(), val));
        }
        out
    }

    pub fn get_all_coords(&self) -> String {
        let coords = self.engine.get_all_coords_in_view();
        let mut sorted_coords = coords.clone();
        sorted_coords.sort_by(|a, b| {
            let (a_col, a_row) = a.as_2d().unwrap_or((0, 0));
            let (b_col, b_row) = b.as_2d().unwrap_or((0, 0));
            let row_cmp = a_row.cmp(&b_row);
            if row_cmp == std::cmp::Ordering::Equal {
                a_col.cmp(&b_col)
            } else {
                row_cmp
            }
        });

        sorted_coords
            .iter()
            .map(|c| c.to_cell_name())
            .collect::<Vec<_>>()
            .join(",")
    }

    pub fn build_timestamp() -> String {
        format!("{} {}", env!("BUILD_DATE"), env!("BUILD_TIME"))
    }

    pub fn build_info(&self) -> String {
        crate::command::build_info_line()
    }

    pub fn build_commit_url(&self) -> String {
        env!("BUILD_GIT_COMMIT_URL").to_string()
    }

    pub fn build_branch_url(&self) -> String {
        env!("BUILD_GIT_BRANCH_URL").to_string()
    }

    pub fn get_help() -> String {
        crate::command::Command::format_help()
    }

    pub fn ui_snapshot(&self) -> JsValue {
        let mut tensors: Vec<UiTensorSummary> = self
            .engine
            .tensors
            .iter()
            .map(|(name, tensor)| UiTensorSummary {
                name: name.clone(),
                shape: tensor.shape.clone(),
                formula_count: tensor.formulas.len(),
                input_count: tensor.inputs.len(),
                state_count: tensor.state_curr.len(),
                is_active: *name == self.engine.active,
            })
            .collect();

        tensors.sort_by(|a, b| a.name.cmp(&b.name));

        let snapshot = UiSnapshot {
            active_tensor: self.engine.active.clone(),
            dims: self.engine.view.offset.len().max(2),
            view_axes: self.engine.view.axes,
            view_offset: self.engine.view.offset.clone(),
            pending_effects: self.engine.pending_effects_len(),
            effect_auto_budget: self.engine.effect_auto_budget,
            tensors,
        };

        serde_wasm_bindgen::to_value(&snapshot).unwrap_or(JsValue::NULL)
    }

    pub fn ui_cells(&self, start_col: i32, start_row: i32, cols: u32, rows: u32) -> JsValue {
        let mut cells = Vec::new();
        let active = match self.engine.tensors.get(&self.engine.active) {
            Some(t) => t,
            None => return serde_wasm_bindgen::to_value(&cells).unwrap_or(JsValue::NULL),
        };

        for y in 0..rows {
            for x in 0..cols {
                let col = start_col + x as i32;
                let row = start_row + y as i32;
                let coord2d = Coord(vec![col, row]);
                let mapped = match self.engine.map_view_coord(&coord2d) {
                    Ok(c) => c,
                    Err(_) => continue,
                };

                let value = active.state_curr.get(&mapped).map(|v| v.to_string());
                let formula_expr = active.formulas.get(&mapped).cloned();
                let formula = formula_expr.as_ref().map(|e| e.to_string());
                let input = active.inputs.get(&mapped).map(|v| v.to_string());

                cells.push(UiCell {
                    col,
                    row,
                    view_label: view_label(col, row),
                    tensor_coord: mapped.0.clone(),
                    tensor_token: coord_token(&mapped),
                    value,
                    formula,
                    input,
                    has_formula: formula_expr.is_some(),
                    has_input: active.inputs.contains_key(&mapped),
                    emits_effect: formula_expr.as_ref().map(expr_may_emit_effect).unwrap_or(false),
                });
            }
        }

        serde_wasm_bindgen::to_value(&cells).unwrap_or(JsValue::NULL)
    }

    pub fn ui_inspect_view_cell(&self, col: i32, row: i32) -> JsValue {
        let coord2d = Coord(vec![col, row]);
        let mapped = match self.engine.map_view_coord(&coord2d) {
            Ok(c) => c,
            Err(e) => {
                let result = UiCellDetail {
                    col,
                    row,
                    view_label: view_label(col, row),
                    tensor_coord: Vec::new(),
                    tensor_token: "".to_string(),
                    value: None,
                    formula: None,
                    input: None,
                    has_formula: false,
                    has_input: false,
                    emits_effect: false,
                    from_input: false,
                    dependencies: Vec::new(),
                    dependents: Vec::new(),
                    error: Some(e),
                };
                return serde_wasm_bindgen::to_value(&result).unwrap_or(JsValue::NULL);
            }
        };

        let active = match self.engine.tensors.get(&self.engine.active) {
            Some(t) => t,
            None => {
                let result = UiCellDetail {
                    col,
                    row,
                    view_label: view_label(col, row),
                    tensor_coord: mapped.0.clone(),
                    tensor_token: coord_token(&mapped),
                    value: None,
                    formula: None,
                    input: None,
                    has_formula: false,
                    has_input: false,
                    emits_effect: false,
                    from_input: false,
                    dependencies: Vec::new(),
                    dependents: Vec::new(),
                    error: Some("Active tensor missing".to_string()),
                };
                return serde_wasm_bindgen::to_value(&result).unwrap_or(JsValue::NULL);
            }
        };

        let value = active.state_curr.get(&mapped).map(|v| v.to_string());
        let formula_expr = active.formulas.get(&mapped).cloned();
        let formula = formula_expr.as_ref().map(|e| e.to_string());
        let input = active.inputs.get(&mapped).map(|v| v.to_string());

        let mut dependency_tokens = HashSet::new();
        if let Some(expr) = &formula_expr {
            collect_dependencies(
                expr,
                &self.engine,
                &self.engine.active,
                &mapped,
                &mut dependency_tokens,
            );
        }

        let mut dependent_tokens = HashSet::new();
        for (coord, expr) in &active.formulas {
            let mut deps = HashSet::new();
            collect_dependencies(
                expr,
                &self.engine,
                &self.engine.active,
                coord,
                &mut deps,
            );
            if deps.contains(&(self.engine.active.clone(), mapped.clone())) {
                dependent_tokens.insert(coord_token(coord));
            }
        }

        let mut dependencies = dependency_tokens
            .into_iter()
            .map(|(tensor_name, coord)| {
                if tensor_name == self.engine.active {
                    coord_token(&coord)
                } else {
                    format!("{}{}", tensor_name, coord_token(&coord))
                }
            })
            .collect::<Vec<_>>();
        dependencies.sort();

        let mut dependents = dependent_tokens.into_iter().collect::<Vec<_>>();
        dependents.sort();

        let detail = UiCellDetail {
            col,
            row,
            view_label: view_label(col, row),
            tensor_coord: mapped.0.clone(),
            tensor_token: coord_token(&mapped),
            value,
            formula,
            input: input.clone(),
            has_formula: formula_expr.is_some(),
            has_input: input.is_some(),
            emits_effect: formula_expr.as_ref().map(expr_may_emit_effect).unwrap_or(false),
            from_input: input.is_some(),
            dependencies,
            dependents,
            error: None,
        };

        serde_wasm_bindgen::to_value(&detail).unwrap_or(JsValue::NULL)
    }

    pub fn ui_active_state(&self) -> JsValue {
        let tensor = match self.engine.tensors.get(&self.engine.active) {
            Some(t) => t,
            None => return serde_wasm_bindgen::to_value(&Vec::<UiStateEntry>::new()).unwrap_or(JsValue::NULL),
        };

        let mut out: Vec<UiStateEntry> = tensor
            .state_curr
            .iter()
            .map(|(coord, value)| UiStateEntry {
                coord: coord_token(coord),
                value: value.to_string(),
            })
            .collect();

        out.sort_by(|a, b| a.coord.cmp(&b.coord));
        serde_wasm_bindgen::to_value(&out).unwrap_or(JsValue::NULL)
    }

    /// Execute a command string - parses and executes, returning the result as a JS object
    pub fn execute_command(&mut self, input: &str) -> JsValue {
        // Collect tokens and their character-level spans
        let lexed: Vec<(Token, std::ops::Range<usize>)> = Token::lexer(input)
            .spanned()
            .map(|(tok, span)| (tok.unwrap_or(Token::Dot), span))
            .collect();

        // Skip lines with no tokens (e.g., comment-only lines) - return empty output
        if lexed.is_empty() {
            let result = CommandResult::Output { text: String::new() };
            return serde_wasm_bindgen::to_value(&result).unwrap_or(JsValue::NULL);
        }

        // Create token stream (will have token-index spans)
        let tokens: Vec<Token> = lexed.iter().map(|(tok, _)| tok.clone()).collect();
        let token_stream = Stream::from_iter(tokens.into_iter());

        let cmd_parser = crate::command::command_parser();
        let result = match cmd_parser.parse(token_stream).into_result() {
            Ok(cmd) => {
                let result = crate::command::execute_with_auto_effects(cmd, &mut self.engine);
                convert_command_result(result)
            }
            Err(errors) => {
                // Extract detailed error information from the parser
                if errors.is_empty() {
                    CommandResult::Error {
                        message: "Failed to parse command".to_string(),
                        span_start: None,
                        span_end: None,
                    }
                } else {
                    let first_error = &errors[0];
                    let reason = first_error.reason().to_string();
                    let span = first_error.span();

                    // Map token-index span to character-level span
                    let (char_start, char_end) = if span.start < lexed.len() {
                        let start_char = lexed[span.start].1.start;
                        let end_idx = span.end.min(lexed.len());
                        let end_char = if end_idx > 0 && end_idx <= lexed.len() {
                            lexed[end_idx - 1].1.end
                        } else {
                            start_char
                        };
                        (Some(start_char), Some(end_char))
                    } else {
                        // Error at end of input
                        (Some(input.len()), Some(input.len()))
                    };

                    // Get what was found
                    let found = match first_error.found() {
                        Some(token) => format!("'{:?}'", token),
                        None => "end of input".to_string(),
                    };

                    // Get what was expected
                    let expected = first_error.expected()
                        .map(|e| format!("{:?}", e))
                        .collect::<Vec<_>>();

                    let message = if expected.is_empty() {
                        format!("{}, found {}", reason, found)
                    } else if expected.len() == 1 {
                        format!("{}, expected {}, found {}", reason, expected[0], found)
                    } else {
                        format!("{}, expected one of [{}], found {}",
                            reason,
                            expected.join(", "),
                            found)
                    };

                    CommandResult::Error {
                        message,
                        span_start: char_start,
                        span_end: char_end,
                    }
                }
            }
        };

        serde_wasm_bindgen::to_value(&result).unwrap_or(JsValue::NULL)
    }
}

fn view_label(col: i32, row: i32) -> String {
    if (0..=25).contains(&col) && row >= 0 {
        let col_char = (b'A' + col as u8) as char;
        return format!("{}{}", col_char, row + 1);
    }
    format!("#[{},{}]", col, row)
}

fn coord_token(coord: &Coord) -> String {
    format!(
        "#[{}]",
        coord.0.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",")
    )
}

fn expr_may_emit_effect(expr: &Expr) -> bool {
    match expr {
        Expr::Effect(_) => true,
        Expr::Call(func, args) => {
            if let Expr::Ref(name) = &**func {
                if name == "tick" {
                    return true;
                }
            }
            expr_may_emit_effect(func) || args.iter().any(expr_may_emit_effect)
        }
        Expr::Binary(lhs, _, rhs) => expr_may_emit_effect(lhs) || expr_may_emit_effect(rhs),
        Expr::Unary(_, inner) => expr_may_emit_effect(inner),
        Expr::If(cond, then_branch, else_branch) => {
            expr_may_emit_effect(cond)
                || expr_may_emit_effect(then_branch)
                || expr_may_emit_effect(else_branch)
        }
        Expr::Array(items) | Expr::Set(items) => items.iter().any(expr_may_emit_effect),
        Expr::Dict(items) => items.iter().any(|(_, value)| expr_may_emit_effect(value)),
        Expr::Range(start, end) => expr_may_emit_effect(start) || expr_may_emit_effect(end),
        Expr::Member(obj, _) => expr_may_emit_effect(obj),
        Expr::Index(obj, idx) => expr_may_emit_effect(obj) || expr_may_emit_effect(idx),
        Expr::Lambda(_, body) => expr_may_emit_effect(body),
        Expr::Assign(_, body)
        | Expr::AssignRel(_, body)
        | Expr::AssignAbs(_, body)
        | Expr::AssignAbsRange(_, body)
        | Expr::AssignAll(_, body)
        | Expr::RangeAssign(_, body)
        | Expr::InputAssign(_, body)
        | Expr::InputAssignRel(_, body)
        | Expr::InputAssignAbs(_, body)
        | Expr::InputAssignAbsRange(_, body)
        | Expr::InputAssignAll(_, body)
        | Expr::InputRangeAssign(_, body) => expr_may_emit_effect(body),
        _ => false,
    }
}

fn expand_rel_index(spec: &IndexSpec) -> Vec<i32> {
    match *spec {
        IndexSpec::Single(v) => vec![v],
        IndexSpec::Range(a, b) => {
            let start = a.unwrap_or(0);
            let end = b.unwrap_or(0);
            if start <= end {
                (start..=end).collect()
            } else {
                (end..=start).collect()
            }
        }
    }
}

fn collect_dependencies(
    expr: &Expr,
    engine: &Engine,
    tensor_name: &str,
    owner_coord: &Coord,
    out: &mut HashSet<(String, Coord)>,
) {
    match expr {
        Expr::Ref(name) => {
            if let Some(view_coord) = Coord::from_str(name) {
                if let Ok(mapped) = engine.map_view_coord(&view_coord) {
                    out.insert((tensor_name.to_string(), mapped));
                }
            }
        }
        Expr::AbsRef(coords) => {
            out.insert((tensor_name.to_string(), Coord(coords.clone())));
        }
        Expr::TensorAbsRef(other_tensor, coords) => {
            out.insert((other_tensor.clone(), Coord(coords.clone())));
        }
        Expr::RelRef(delta) => {
            out.insert((tensor_name.to_string(), owner_coord.offset(delta)));
        }
        Expr::RelRange(specs) => {
            if specs.is_empty() {
                return;
            }

            fn walk(
                tensor_name: &str,
                owner_coord: &Coord,
                specs: &[IndexSpec],
                dim: usize,
                acc: &mut Vec<i32>,
                out: &mut HashSet<(String, Coord)>,
            ) {
                let offsets = expand_rel_index(&specs[dim]);
                for off in offsets {
                    if acc.len() <= dim {
                        acc.resize(dim + 1, 0);
                    }
                    let base = owner_coord.0.get(dim).cloned().unwrap_or(0);
                    acc[dim] = base + off;
                    if dim + 1 == specs.len() {
                        out.insert((tensor_name.to_string(), Coord(acc.clone())));
                    } else {
                        walk(tensor_name, owner_coord, specs, dim + 1, acc, out);
                    }
                }
            }

            let mut acc = owner_coord.0.clone();
            walk(tensor_name, owner_coord, specs, 0, &mut acc, out);
        }
        Expr::Binary(lhs, _, rhs) => {
            collect_dependencies(lhs, engine, tensor_name, owner_coord, out);
            collect_dependencies(rhs, engine, tensor_name, owner_coord, out);
        }
        Expr::Unary(_, inner) => collect_dependencies(inner, engine, tensor_name, owner_coord, out),
        Expr::If(cond, then_branch, else_branch) => {
            collect_dependencies(cond, engine, tensor_name, owner_coord, out);
            collect_dependencies(then_branch, engine, tensor_name, owner_coord, out);
            collect_dependencies(else_branch, engine, tensor_name, owner_coord, out);
        }
        Expr::Array(items) | Expr::Set(items) => {
            for item in items {
                collect_dependencies(item, engine, tensor_name, owner_coord, out);
            }
        }
        Expr::Dict(items) => {
            for (_, value) in items {
                collect_dependencies(value, engine, tensor_name, owner_coord, out);
            }
        }
        Expr::Range(start, end) => {
            collect_dependencies(start, engine, tensor_name, owner_coord, out);
            collect_dependencies(end, engine, tensor_name, owner_coord, out);
        }
        Expr::Member(obj, _) => collect_dependencies(obj, engine, tensor_name, owner_coord, out),
        Expr::Index(obj, idx) => {
            collect_dependencies(obj, engine, tensor_name, owner_coord, out);
            collect_dependencies(idx, engine, tensor_name, owner_coord, out);
        }
        Expr::Lambda(_, body) => collect_dependencies(body, engine, tensor_name, owner_coord, out),
        Expr::Call(func, args) => {
            collect_dependencies(func, engine, tensor_name, owner_coord, out);
            for arg in args {
                collect_dependencies(arg, engine, tensor_name, owner_coord, out);
            }
        }
        Expr::Assign(_, body)
        | Expr::AssignRel(_, body)
        | Expr::AssignAbs(_, body)
        | Expr::AssignAbsRange(_, body)
        | Expr::AssignAll(_, body)
        | Expr::RangeAssign(_, body)
        | Expr::InputAssign(_, body)
        | Expr::InputAssignRel(_, body)
        | Expr::InputAssignAbs(_, body)
        | Expr::InputAssignAbsRange(_, body)
        | Expr::InputAssignAll(_, body)
        | Expr::InputRangeAssign(_, body) => {
            collect_dependencies(body, engine, tensor_name, owner_coord, out)
        }
        Expr::Literal(_) | Expr::CellRange(_) | Expr::Effect(_) => {}
    }
}

#[derive(Serialize)]
struct UiTensorSummary {
    name: String,
    shape: Vec<i32>,
    formula_count: usize,
    input_count: usize,
    state_count: usize,
    is_active: bool,
}

#[derive(Serialize)]
struct UiSnapshot {
    active_tensor: String,
    dims: usize,
    view_axes: [usize; 2],
    view_offset: Vec<i32>,
    pending_effects: usize,
    effect_auto_budget: usize,
    tensors: Vec<UiTensorSummary>,
}

#[derive(Serialize)]
struct UiCell {
    col: i32,
    row: i32,
    view_label: String,
    tensor_coord: Vec<i32>,
    tensor_token: String,
    value: Option<String>,
    formula: Option<String>,
    input: Option<String>,
    has_formula: bool,
    has_input: bool,
    emits_effect: bool,
}

#[derive(Serialize)]
struct UiCellDetail {
    col: i32,
    row: i32,
    view_label: String,
    tensor_coord: Vec<i32>,
    tensor_token: String,
    value: Option<String>,
    formula: Option<String>,
    input: Option<String>,
    has_formula: bool,
    has_input: bool,
    emits_effect: bool,
    from_input: bool,
    dependencies: Vec<String>,
    dependents: Vec<String>,
    error: Option<String>,
}

#[derive(Serialize)]
struct UiStateEntry {
    coord: String,
    value: String,
}

/// Result of executing a command - automatically generates TypeScript types
#[derive(Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum CommandResult {
    Output { text: String },
    BShow { coords: Vec<String> },
    Demo { script: String },
    Exit,
    Batch { results: Vec<CommandResult> },
    Error {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        span_start: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        span_end: Option<usize>,
    },
}

fn convert_command_result(result: crate::command::CommandResult) -> CommandResult {
    match result {
        crate::command::CommandResult::Output(text) => CommandResult::Output { text },
        crate::command::CommandResult::BShow(coords) => {
            let coord_names: Vec<String> = coords.iter()
                .map(|c| c.to_cell_name())
                .collect();
            CommandResult::BShow { coords: coord_names }
        }
        crate::command::CommandResult::Demo(script) => CommandResult::Demo { script },
        crate::command::CommandResult::Exit => CommandResult::Exit,
        crate::command::CommandResult::Batch(results) => {
            let converted = results.into_iter()
                .map(convert_command_result)
                .collect();
            CommandResult::Batch { results: converted }
        }
    }
}
