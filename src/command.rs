pub use crate::ast::{Command, EffectsCommand};
use crate::ast::{Effect, Expr, Value};
use crate::interpreter::{Engine, Coord};
use crate::lexer::Token;
use chumsky::prelude::*;

impl Command {
    fn base_name(&self) -> &'static str {
        match self {
            Command::Alloc { .. } => "alloc",
            Command::Use { .. } => "use",
            Command::View { .. } => "view",
            Command::SetInput { .. } => "set",
            Command::Effects(_) => "effects",
            Command::Tick(_) => "tick",
            Command::Show(_) => "show",
            Command::BShow(_) => "bshow",
            Command::Demo(_) => "demo",
            Command::Help => "help",
            Command::Encode => "encode",
            Command::Exit => "exit",
            Command::Eval(_) => "<expr>",
        }
    }

    pub fn syntax(&self) -> String {
        match self {
            Command::Alloc { .. } => "alloc <name> [dims]".to_string(),
            Command::Use { .. } => "use <name>".to_string(),
            Command::View { .. } => "view axes [a,b] offset [o0,o1,...]".to_string(),
            Command::SetInput { .. } => "set A1 <literal>".to_string(),
            Command::Effects(_) => "effects <auto|run|pending> [n]".to_string(),
            Command::Tick(opt) | Command::Show(opt) | Command::BShow(opt) => {
                let base = self.base_name();
                if opt.is_some() {
                    format!("{} <range>", base)
                } else {
                    format!("{} [range]", base)
                }
            }
            Command::Demo(_) => "demo [number]".to_string(),
            Command::Help => "help".to_string(),
            Command::Encode => "encode".to_string(),
            Command::Exit => "exit".to_string(),
            Command::Eval(Expr::Assign(_, _)) => "A1 = <expr>".to_string(),
            Command::Eval(Expr::AssignRel(_, _)) => "@[i,j] = <expr>".to_string(),
            Command::Eval(Expr::AssignAbs(_, _)) => "#[i,j] = <expr>".to_string(),
            Command::Eval(Expr::AssignAbsRange(_, _)) => "#[i:j,k] = <expr>".to_string(),
            Command::Eval(Expr::AssignAll(_, _)) => "#[*] = <expr>".to_string(),
            Command::Eval(Expr::RangeAssign(_, _)) => "A1:C3 = <expr>".to_string(),
            Command::Eval(Expr::InputAssign(_, _)) => "A1 := <expr>".to_string(),
            Command::Eval(Expr::InputAssignRel(_, _)) => "@[i,j] := <expr>".to_string(),
            Command::Eval(Expr::InputAssignAbs(_, _)) => "#[i,j] := <expr>".to_string(),
            Command::Eval(Expr::InputAssignAbsRange(_, _)) => "#[i:j,k] := <expr>".to_string(),
            Command::Eval(Expr::InputAssignAll(_, _)) => "#[*] := <expr>".to_string(),
            Command::Eval(Expr::InputRangeAssign(_, _)) => "A1:C3 := <expr>".to_string(),
            Command::Eval(_) => "<expr>".to_string(),
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            Command::Alloc { .. } => "Allocate a new tensor with a shape",
            Command::Use { .. } => "Switch the active tensor",
            Command::View { .. } => "Configure the active 2D view (axes/offset)",
            Command::SetInput { .. } => "Set an input value (no formula), applied immediately",
            Command::Effects(_) => "Manage effect execution policy",
            Command::Tick(None) => "Advance the engine one step (all formulas)",
            Command::Tick(Some(_)) => "Advance the engine one step (range only)",
            Command::Show(None) => "Display all cell values",
            Command::Show(Some(_)) => "Display cell values in range",
            Command::BShow(None) => "Display binary grid visualization (all cells)",
            Command::BShow(Some(_)) => "Display binary grid visualization (range)",
            Command::Demo(_) => "Load and run a demo script, or list demos",
            Command::Help => "Show this help message",
            Command::Encode => "Generate shareable URL from command history",
            Command::Exit => "Exit the CLI",
            Command::Eval(Expr::Assign(_, _)) => "Set a formula for a single cell",
            Command::Eval(Expr::AssignRel(_, _)) => "Set a formula relative to the origin (effects only)",
            Command::Eval(Expr::AssignAbs(_, _)) => "Set a formula for a single cell (absolute tensor coord)",
            Command::Eval(Expr::AssignAbsRange(_, _)) => "Set a formula for a tensor coord range",
            Command::Eval(Expr::AssignAll(_, _)) => "Set a formula for all cells in a tensor",
            Command::Eval(Expr::RangeAssign(_, _)) => "Set a formula for a range of cells",
            Command::Eval(Expr::InputAssign(_, _)) => "Set a state value for a single cell (immediate)",
            Command::Eval(Expr::InputAssignRel(_, _)) => "Set a state value relative to origin (effects only)",
            Command::Eval(Expr::InputAssignAbs(_, _)) => "Set a state value for a single cell (absolute tensor coord)",
            Command::Eval(Expr::InputAssignAbsRange(_, _)) => "Set state values for a tensor coord range",
            Command::Eval(Expr::InputAssignAll(_, _)) => "Set state values for all cells in a tensor",
            Command::Eval(Expr::InputRangeAssign(_, _)) => "Set state values for a range of cells",
            Command::Eval(_) => "Evaluate an expression",
        }
    }

    pub fn examples(&self) -> Vec<&'static str> {
        match self {
            Command::Alloc { .. } => vec!["alloc sheet [10, 10]"],
            Command::Use { .. } => vec!["use sheet"],
            Command::View { .. } => vec!["view axes [0,1] offset [0,0]"],
            Command::SetInput { .. } => vec!["set A1 42", "set B2 \"hi\""],
            Command::Effects(_) => vec!["effects pending", "effects auto 100", "effects run 10"],
            Command::Tick(_) => vec!["tick", "tick A1:C3"],
            Command::Show(_) => vec!["show", "show A1:C3"],
            Command::BShow(_) => vec!["bshow", "bshow A1:C3"],
            Command::Demo(_) => vec!["demo", "demo 1", "demo 9"],
            Command::Help => vec!["help"],
            Command::Encode => vec!["encode"],
            Command::Exit => vec!["exit"],
            Command::Eval(Expr::Assign(_, _)) => vec!["A1 = 10", "B1 = A1 + 5"],
            Command::Eval(Expr::AssignRel(_, _)) => vec!["@[-1,0] = 5"],
            Command::Eval(Expr::AssignAbs(_, _)) => vec!["#[0,0] = 10", "#[1,0] = #[0,0] + 5"],
            Command::Eval(Expr::AssignAbsRange(_, _)) => vec!["#[0:2,0] = 1"],
            Command::Eval(Expr::AssignAll(_, _)) => vec!["#[*] = 0", "grid#[*] = 1"],
            Command::Eval(Expr::RangeAssign(_, _)) => vec!["A1:C3 = 0", "A1:C3 = @[0,0] * 2"],
            Command::Eval(Expr::InputAssign(_, _)) => vec!["A1 := 10"],
            Command::Eval(Expr::InputAssignRel(_, _)) => vec!["@[-1,0] := 5"],
            Command::Eval(Expr::InputAssignAbs(_, _)) => vec!["#[0,0] := 10"],
            Command::Eval(Expr::InputAssignAbsRange(_, _)) => vec!["#[0:2,0] := 1"],
            Command::Eval(Expr::InputAssignAll(_, _)) => vec!["#[*] := 0"],
            Command::Eval(Expr::InputRangeAssign(_, _)) => vec!["A1:C3 := 0"],
            Command::Eval(_) => vec![],
        }
    }

    /// Get help text for all command types (one instance of each)
    pub fn all_command_types() -> Vec<Command> {
        vec![
            Command::Alloc { name: "sheet".to_string(), shape: vec![10, 10] },
            Command::Use { name: "sheet".to_string() },
            Command::View { axes: Some(vec![0, 1]), offset: Some(vec![0, 0]) },
            Command::SetInput { target: "A1".to_string(), value: Expr::Literal(Value::Int(0)) },
            Command::Effects(EffectsCommand::Pending),
            Command::Tick(None),
            Command::Show(None),
            Command::BShow(None),
            Command::Demo(Some(1)),
            Command::Eval(Expr::Assign("A1".to_string(), Box::new(Expr::Literal(crate::ast::Value::Int(0))))),
            Command::Eval(Expr::AssignRel(vec![0, 0], Box::new(Expr::Literal(crate::ast::Value::Int(0))))),
            Command::Eval(Expr::AssignAbs(vec![0, 0], Box::new(Expr::Literal(crate::ast::Value::Int(0))))),
            Command::Eval(Expr::AssignAbsRange(
                vec![crate::ast::IndexSpec::Range(Some(0), Some(1)), crate::ast::IndexSpec::Single(0)],
                Box::new(Expr::Literal(crate::ast::Value::Int(0)))
            )),
            Command::Eval(Expr::AssignAll(None, Box::new(Expr::Literal(crate::ast::Value::Int(0))))),
            Command::Eval(Expr::RangeAssign(
                crate::ast::CellRange {
                    start: crate::ast::CellCoord { col: "A".to_string(), row: 1 },
                    end: crate::ast::CellCoord { col: "C".to_string(), row: 3 },
                    step: None,
                },
                Box::new(Expr::Literal(crate::ast::Value::Int(0))),
            )),
            Command::Eval(Expr::InputAssign("A1".to_string(), Box::new(Expr::Literal(crate::ast::Value::Int(0))))),
            Command::Help,
            Command::Encode,
            Command::Exit,
        ]
    }

    pub fn print_help() {
        println!("{}", Self::format_help());
    }
}

fn execute_effect(effect: Effect, engine: &mut Engine) -> CommandResult {
    match effect {
        Effect::Tick => Command::Tick(None).execute(engine),
        Effect::TickRange(range) => Command::Tick(Some(range)).execute(engine),
        Effect::Command { origin, command } => {
            let origin_coord = origin.as_ref().map(|(_, coords)| Coord(coords.clone()));
            command.execute_with_origin(engine, origin_coord.as_ref())
        }
    }
}

fn run_effects(engine: &mut Engine, limit: usize) -> Vec<CommandResult> {
    let mut results = Vec::new();
    let mut executed = 0usize;
    while executed < limit {
        let effect = match engine.pop_effect() {
            Some(effect) => effect,
            None => break,
        };
        executed += 1;
        results.push(execute_effect(effect, engine));
    }
    results
}

fn flatten_results(result: CommandResult) -> Vec<CommandResult> {
    match result {
        CommandResult::Batch(items) => items,
        other => vec![other],
    }
}

pub fn execute_with_auto_effects(cmd: Command, engine: &mut Engine) -> CommandResult {
    let skip_auto = matches!(cmd, Command::Effects(EffectsCommand::Run(_)));
    let result = cmd.execute(engine);
    if skip_auto || engine.effect_auto_budget == 0 {
        return result;
    }
    if matches!(result, CommandResult::Exit) {
        return result;
    }
    let auto_results = run_effects(engine, engine.effect_auto_budget);
    if auto_results.is_empty() {
        return result;
    }
    let mut out = flatten_results(result);
    out.extend(auto_results);
    CommandResult::Batch(out)
}

/// Parser for CLI commands that shares grammar with the expression parser
pub fn command_parser<'src, I>() -> Boxed<'src, 'src, I, Command, extra::Err<Rich<'src, Token>>>
where
    I: Input<'src, Token = Token, Span = SimpleSpan>,
{
    crate::command_parse::command_parser_with_expr(crate::parser::parser())
}

/// Result of executing a command
pub enum CommandResult {
    /// Simple text output
    Output(String),
    /// Binary show visualization data (coordinates)
    BShow(Vec<Coord>),
    /// Demo script to execute (script content)
    Demo(String),
    /// Exit the CLI
    Exit,
    /// Multiple results emitted in sequence
    Batch(Vec<CommandResult>),
}

impl Command {
    /// Execute a command against an engine
    pub fn execute(self, engine: &mut Engine) -> CommandResult {
        self.execute_with_origin(engine, None)
    }

    pub fn execute_with_origin(self, engine: &mut Engine, origin: Option<&Coord>) -> CommandResult {
        match self {
            Command::Alloc { name, shape } => {
                match engine.alloc_tensor(&name, shape) {
                    Ok(()) => CommandResult::Output(format!("Allocated tensor '{}'", name)),
                    Err(e) => CommandResult::Output(format!("Error: {}", e)),
                }
            }

            Command::Use { name } => {
                match engine.use_tensor(&name) {
                    Ok(()) => CommandResult::Output(format!("Active tensor set to '{}'", name)),
                    Err(e) => CommandResult::Output(format!("Error: {}", e)),
                }
            }

            Command::View { axes, offset } => {
                let mut out = Vec::new();
                if let Some(axes) = axes {
                    if axes.len() != 2 {
                        return CommandResult::Output("Error: view axes must have length 2".to_string());
                    }
                    let a0 = axes[0] as isize;
                    let a1 = axes[1] as isize;
                    if a0 < 0 || a1 < 0 {
                        return CommandResult::Output("Error: view axes must be non-negative".to_string());
                    }
                    match engine.set_view_axes([a0 as usize, a1 as usize]) {
                        Ok(()) => out.push(format!("View axes set to [{}, {}]", a0, a1)),
                        Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                    }
                }
                if let Some(offset) = offset {
                    match engine.set_view_offset(offset.clone()) {
                        Ok(()) => out.push(format!("View offset set to [{}]", offset.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","))),
                        Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                    }
                }
                if out.is_empty() {
                    CommandResult::Output("No view changes applied".to_string())
                } else {
                    CommandResult::Output(out.join("\n"))
                }
            }

            Command::SetInput { target, value } => {
                let value = match eval_literal(&value) {
                    Ok(v) => v,
                    Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                };
                match engine.set_input(&target, value) {
                    Ok(()) => CommandResult::Output(format!("Input set for {}", target)),
                    Err(e) => CommandResult::Output(format!("Error: {}", e)),
                }
            }

            Command::Effects(cmd) => {
                match cmd {
                    EffectsCommand::Auto(n) => {
                        engine.effect_auto_budget = n;
                        CommandResult::Output(format!("Effect auto budget set to {}", n))
                    }
                    EffectsCommand::Run(limit) => {
                        let pending = engine.pending_effects_len();
                        let limit = limit.unwrap_or(pending);
                        let results = run_effects(engine, limit);
                        let executed = results.len();
                        let remaining = engine.pending_effects_len();
                        let summary = CommandResult::Output(format!(
                            "Executed {} effect(s). Pending: {}",
                            executed, remaining
                        ));
                        if results.is_empty() {
                            summary
                        } else {
                            let mut out = results;
                            out.push(summary);
                            CommandResult::Batch(out)
                        }
                    }
                    EffectsCommand::Pending => {
                        let pending = engine.pending_effects_len();
                        CommandResult::Output(format!("Pending effects: {}", pending))
                    }
                }
            }

            Command::Help => CommandResult::Output(Self::format_help()),

            Command::Tick(range_opt) => {
                if let Some(range) = range_opt {
                    let coords_2d: Vec<Coord> = range
                        .expand()
                        .iter()
                        .filter_map(|s| Coord::from_str(s))
                        .collect();
                    let coords_nd: Vec<Coord> = coords_2d
                        .iter()
                        .filter_map(|c| engine.map_view_coord(c).ok())
                        .collect();
                    let result = engine.tick_range(&coords_nd);
                    let mut out = format!(
                        "Tick processed for range {}:{}",
                        range.start.to_string(),
                        range.end.to_string()
                    );
                    if result.enqueued_effects > 0 {
                        out.push_str(&format!("\nEffects queued: {}", result.enqueued_effects));
                    }
                    let pending = engine.pending_effects_len();
                    if pending > 0 {
                        out.push_str(&format!(
                            "\nPending effects: {} (use 'effects run' or 'effects auto N')",
                            pending
                        ));
                    }
                    CommandResult::Output(out)
                } else {
                    let result = engine.tick();
                    let mut out = "Tick processed.".to_string();
                    if result.enqueued_effects > 0 {
                        out.push_str(&format!("\nEffects queued: {}", result.enqueued_effects));
                    }
                    let pending = engine.pending_effects_len();
                    if pending > 0 {
                        out.push_str(&format!(
                            "\nPending effects: {} (use 'effects run' or 'effects auto N')",
                            pending
                        ));
                    }
                    CommandResult::Output(out)
                }
            }

            Command::Show(range_opt) => {
                let tensor = match engine.tensors.get(&engine.active) {
                    Some(t) => t,
                    None => return CommandResult::Output("Current State (view):\n".to_string()),
                };
                let format_line = |label: String,
                                   val: Value,
                                   formula: Option<Expr>,
                                   input: Option<Value>|
                 -> String {
                    let mut line = format!("{}: {}", label, val);
                    let mut meta = Vec::new();
                    if let Some(expr) = formula {
                        meta.push(format!("formula: {}", expr));
                    }
                    if let Some(value) = input {
                        meta.push(format!("input: {}", value));
                    }
                    if !meta.is_empty() {
                        line.push_str(&format!(" ({})", meta.join(", ")));
                    }
                    line
                };
                let output = if let Some(range) = range_opt {
                    let coords: Vec<Coord> = range
                        .expand()
                        .iter()
                        .filter_map(|s| Coord::from_str(s))
                        .collect();
                    let mut entries: Vec<(Coord, Coord, Value, Option<Expr>, Option<Value>)> = Vec::new();
                    for coord2d in coords {
                        let nd = match engine.map_view_coord(&coord2d) {
                            Ok(c) => c,
                            Err(_) => continue,
                        };
                        let val = match tensor.state_curr.get(&nd) {
                            Some(v) => v.clone(),
                            None => continue,
                        };
                        let formula = tensor.formulas.get(&nd).cloned();
                        let input = tensor.inputs.get(&nd).cloned();
                        entries.push((coord2d, nd, val, formula, input));
                    }

                    entries.sort_by(|(a, _, _, _, _), (b, _, _, _, _)| {
                        let (a_col, a_row) = a.as_2d().unwrap_or((0, 0));
                        let (b_col, b_row) = b.as_2d().unwrap_or((0, 0));
                        let row_cmp = a_row.cmp(&b_row);
                        if row_cmp == std::cmp::Ordering::Equal {
                            a_col.cmp(&b_col)
                        } else {
                            row_cmp
                        }
                    });

                    let mut out = format!("Current State (range {}:{}):\n", range.start.to_string(), range.end.to_string());
                    for (_coord2d, nd, val, formula, input) in entries {
                        let label = nd.to_string();
                        out.push_str(&format!("{}\n", format_line(label, val, formula, input)));
                    }
                    out
                } else {
                    let coords = engine.get_all_coords_in_view();
                    let mut entries: Vec<(Coord, Coord, Value, Option<Expr>, Option<Value>)> = Vec::new();
                    for coord2d in coords {
                        let nd = match engine.map_view_coord(&coord2d) {
                            Ok(c) => c,
                            Err(_) => continue,
                        };
                        let val = match tensor.state_curr.get(&nd) {
                            Some(v) => v.clone(),
                            None => continue,
                        };
                        let formula = tensor.formulas.get(&nd).cloned();
                        let input = tensor.inputs.get(&nd).cloned();
                        entries.push((coord2d, nd, val, formula, input));
                    }

                    entries.sort_by(|(a, _, _, _, _), (b, _, _, _, _)| {
                        let (a_col, a_row) = a.as_2d().unwrap_or((0, 0));
                        let (b_col, b_row) = b.as_2d().unwrap_or((0, 0));
                        let row_cmp = a_row.cmp(&b_row);
                        if row_cmp == std::cmp::Ordering::Equal {
                            a_col.cmp(&b_col)
                        } else {
                            row_cmp
                        }
                    });

                    let mut out = "Current State (view):\n".to_string();
                    for (_coord2d, nd, val, formula, input) in entries {
                        let label = nd.to_string();
                        out.push_str(&format!("{}\n", format_line(label, val, formula, input)));
                    }
                    out
                };
                CommandResult::Output(output)
            }

            Command::BShow(range_opt) => {
                let coords = if let Some(range) = range_opt {
                    range
                        .expand()
                        .iter()
                        .filter_map(|s| Coord::from_str(s))
                        .collect()
                } else {
                    engine.get_all_coords_in_view()
                };

                CommandResult::BShow(coords)
            }

            Command::Demo(Some(n)) => match get_demo_script_owned(n) {
                Some(script) => CommandResult::Demo(script),
                None => CommandResult::Output(format!(
                    "Error: Demo {} not found. Available demos: 1-{}",
                    n,
                    get_demo_count()
                )),
            },
            Command::Demo(None) => CommandResult::Output(format_demo_list()),

            Command::Encode => {
                CommandResult::Output("Encode command handled by web CLI".to_string())
            }

            Command::Exit => CommandResult::Exit,

            Command::Eval(expr) => {
                let output = match expr {
                    Expr::RangeAssign(range, body) => {
                        let expanded_coords = range.expand();
                        let mut count = 0;

                        match &*body {
                            Expr::Array(items) => {
                                for (i, cell_name) in expanded_coords.iter().enumerate() {
                                    if let Some(coord2d) = Coord::from_str(cell_name) {
                                        let coord = match engine.map_view_coord(&coord2d) {
                                            Ok(c) => c,
                                            Err(_) => continue,
                                        };
                                        if let Some(item_expr) = items.get(i) {
                                            engine.set_formula_coord(coord, item_expr.clone());
                                            count += 1;
                                        } else {
                                            engine.set_formula_coord(coord, Expr::Literal(Value::Empty));
                                            count += 1;
                                        }
                                    }
                                }
                            }
                            _ => {
                                for cell_name in expanded_coords.iter() {
                                    if let Some(coord2d) = Coord::from_str(cell_name) {
                                        let coord = match engine.map_view_coord(&coord2d) {
                                            Ok(c) => c,
                                            Err(_) => continue,
                                        };
                                        engine.set_formula_coord(coord, (*body).clone());
                                        count += 1;
                                    }
                                }
                            }
                        }

                        format!("Formula set for {} cells", count)
                    }
                    Expr::Assign(target, body) => {
                        if let Some(coord2d) = Coord::from_str(&target) {
                            let coord = match engine.map_view_coord(&coord2d) {
                                Ok(c) => c,
                                Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                            };
                            let label = coord.to_string();
                            engine.set_formula_coord(coord, *body);
                            format!("Formula set for 1 cell ({})", label)
                        } else {
                            format!("Error: Invalid cell reference {}", target)
                        }
                    }
                    Expr::AssignRel(coords, body) => {
                        let origin = match origin {
                            Some(coord) => coord,
                            None => {
                                return CommandResult::Output(
                                    "Error: Relative assignment requires an origin (use inside effect)".to_string(),
                                )
                            }
                        };
                        let target = origin.offset(&coords);
                        let label = target.to_string();
                        engine.set_formula_coord(target, *body);
                        format!("Formula set for 1 cell ({})", label)
                    }
                    Expr::AssignAbs(coords, body) => {
                        let coord_label = Expr::AbsRef(coords.clone()).to_string();
                        let coord = Coord(coords);
                        engine.set_formula_coord(coord, *body);
                        format!("Formula set for 1 cell ({})", coord_label)
                    }
                    Expr::AssignAbsRange(specs, body) => {
                        let shape = engine
                            .tensors
                            .get(&engine.active)
                            .map(|t| t.shape.clone())
                            .unwrap_or_default();
                        let coords = match expand_coord_spec(&specs, &shape) {
                            Ok(coords) => coords,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let mut count = 0;
                        for coord in coords {
                            engine.set_formula_coord(coord, (*body).clone());
                            count += 1;
                        }
                        format!("Formula set for {} cells", count)
                    }
                    Expr::AssignAll(tensor, body) => {
                        let (tensor_name, shape) = if let Some(name) = tensor {
                            match engine.tensors.get(&name) {
                                Some(t) => (name.clone(), t.shape.clone()),
                                None => {
                                    return CommandResult::Output(format!(
                                        "Error: Tensor '{}' not found",
                                        name
                                    ))
                                }
                            }
                        } else {
                            let name = engine.active.clone();
                            let shape = engine
                                .tensors
                                .get(&name)
                                .map(|t| t.shape.clone())
                                .unwrap_or_default();
                            (name, shape)
                        };

                        let coords = all_coords_for_shape(&shape);
                        let mut count = 0;
                        for coord in coords {
                            if engine
                                .set_formula_coord_in(&tensor_name, coord, (*body).clone())
                                .is_ok()
                            {
                                count += 1;
                            }
                        }
                        format!("Formula set for {} cells", count)
                    }
                    Expr::InputAssign(target, body) => {
                        let value = match eval_literal(&body) {
                            Ok(v) => v,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        if let Some(coord2d) = Coord::from_str(&target) {
                            let coord = match engine.map_view_coord(&coord2d) {
                                Ok(c) => c,
                                Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                            };
                            let label = coord.to_string();
                            engine.set_state_coord(coord, value);
                            format!("State set for 1 cell ({})", label)
                        } else {
                            format!("Error: Invalid cell reference {}", target)
                        }
                    }
                    Expr::InputAssignRel(coords, body) => {
                        let value = match eval_literal(&body) {
                            Ok(v) => v,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let origin = match origin {
                            Some(coord) => coord,
                            None => {
                                return CommandResult::Output(
                                    "Error: Relative input assignment requires an origin (use inside effect)".to_string(),
                                )
                            }
                        };
                        let target = origin.offset(&coords);
                        let label = target.to_string();
                        engine.set_state_coord(target, value);
                        format!("State set for 1 cell ({})", label)
                    }
                    Expr::InputAssignAbs(coords, body) => {
                        let value = match eval_literal(&body) {
                            Ok(v) => v,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let coord_label = Expr::AbsRef(coords.clone()).to_string();
                        let coord = Coord(coords);
                        engine.set_state_coord(coord, value);
                        format!("State set for 1 cell ({})", coord_label)
                    }
                    Expr::InputAssignAbsRange(specs, body) => {
                        let value = match eval_literal(&body) {
                            Ok(v) => v,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let shape = engine
                            .tensors
                            .get(&engine.active)
                            .map(|t| t.shape.clone())
                            .unwrap_or_default();
                        let coords = match expand_coord_spec(&specs, &shape) {
                            Ok(coords) => coords,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let mut count = 0;
                        let active = engine.active.clone();
                        for coord in coords {
                            if engine.set_state_coord_in(&active, coord, value.clone()).is_ok() {
                                count += 1;
                            }
                        }
                        format!("State set for {} cells", count)
                    }
                    Expr::InputAssignAll(tensor, body) => {
                        let value = match eval_literal(&body) {
                            Ok(v) => v,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let (tensor_name, shape) = if let Some(name) = tensor {
                            match engine.tensors.get(&name) {
                                Some(t) => (name.clone(), t.shape.clone()),
                                None => {
                                    return CommandResult::Output(format!(
                                        "Error: Tensor '{}' not found",
                                        name
                                    ))
                                }
                            }
                        } else {
                            let name = engine.active.clone();
                            let shape = engine
                                .tensors
                                .get(&name)
                                .map(|t| t.shape.clone())
                                .unwrap_or_default();
                            (name, shape)
                        };
                        let coords = all_coords_for_shape(&shape);
                        let mut count = 0;
                        for coord in coords {
                            if engine.set_state_coord_in(&tensor_name, coord, value.clone()).is_ok() {
                                count += 1;
                            }
                        }
                        format!("State set for {} cells", count)
                    }
                    Expr::InputRangeAssign(range, body) => {
                        let value = match eval_literal(&body) {
                            Ok(v) => v,
                            Err(e) => return CommandResult::Output(format!("Error: {}", e)),
                        };
                        let expanded_coords = range.expand();
                        let mut count = 0;
                        for cell_name in expanded_coords.iter() {
                            if let Some(coord2d) = Coord::from_str(cell_name) {
                                let coord = match engine.map_view_coord(&coord2d) {
                                    Ok(c) => c,
                                    Err(_) => continue,
                                };
                                engine.set_state_coord(coord, value.clone());
                                count += 1;
                            }
                        }
                        format!("State set for {} cells", count)
                    }
                    _ => "Expression parsed (use 'A1 = ...' to assign)".to_string(),
                };
                CommandResult::Output(output)
            }
        }
    }

    pub fn format_help() -> String {
        let mut out = String::new();
        out.push_str("SheetLang Commands:\n");
        out.push_str(&format!("{}\n\n", build_info_line()));

        for cmd in Self::all_command_types() {
            out.push_str(&format!("  {}\n", cmd.syntax()));
            out.push_str(&format!("    {}\n", cmd.description()));
            let examples = cmd.examples();
            if !examples.is_empty() {
                out.push_str(&format!("    Examples: {}\n", examples.join(", ")));
            }
            out.push_str("\n");
        }

        out
    }
}

fn eval_literal(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Literal(v) => Ok(v.clone()),
        Expr::Array(items) => {
            let mut out = Vec::new();
            for item in items {
                out.push(eval_literal(item)?);
            }
            Ok(Value::Array(out))
        }
        Expr::Set(items) => {
            let mut out = Vec::new();
            for item in items {
                out.push(eval_literal(item)?);
            }
            Ok(Value::Set(out))
        }
        Expr::Dict(items) => {
            let mut out = Vec::new();
            for (k, v_expr) in items {
                out.push((k.clone(), eval_literal(v_expr)?));
            }
            Ok(Value::Dict(out))
        }
        Expr::Range(start, end) => {
            let s = eval_literal(start)?;
            let e = eval_literal(end)?;
            match (s, e) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Range(a, b)),
                _ => Err("Range bounds must be integers".to_string()),
            }
        }
        Expr::Unary(op, inner) => {
            let v = eval_literal(inner)?;
            match op {
                crate::ast::Op::Not => Ok(Value::Bool(!is_truthy(&v))),
                crate::ast::Op::Sub => {
                    if let Value::Int(n) = v {
                        Ok(Value::Int(-n))
                    } else {
                        Err("Unary '-' requires an integer".to_string())
                    }
                }
                _ => Ok(v),
            }
        }
        Expr::Binary(lhs, op, rhs) => {
            let l = eval_literal(lhs)?;
            let r = eval_literal(rhs)?;
            match op {
                crate::ast::Op::Add => match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                    _ => Err("Invalid types for '+'".to_string()),
                },
                crate::ast::Op::Sub => match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    _ => Err("Invalid types for '-'".to_string()),
                },
                crate::ast::Op::Mul => match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    _ => Err("Invalid types for '*'".to_string()),
                },
                crate::ast::Op::Div => match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            Err("Division by zero".to_string())
                        } else {
                            Ok(Value::Int(a / b))
                        }
                    }
                    _ => Err("Invalid types for '/'".to_string()),
                },
                crate::ast::Op::Eq => Ok(Value::Bool(l == r)),
                crate::ast::Op::Neq => Ok(Value::Bool(l != r)),
                crate::ast::Op::Gt => match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
                    _ => Ok(Value::Bool(false)),
                },
                crate::ast::Op::Lt => match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
                    _ => Ok(Value::Bool(false)),
                },
                crate::ast::Op::And => {
                    if is_truthy(&l) {
                        Ok(r)
                    } else {
                        Ok(l)
                    }
                }
                crate::ast::Op::Or => {
                    if is_truthy(&l) {
                        Ok(l)
                    } else {
                        Ok(r)
                    }
                }
                crate::ast::Op::Not => Ok(Value::Bool(!is_truthy(&l))),
                crate::ast::Op::Remove => match (l, r) {
                    (Value::Array(items), rhs) => Ok(Value::Array(items.into_iter().filter(|v| v != &rhs).collect())),
                    (Value::Set(items), rhs) => Ok(Value::Set(items.into_iter().filter(|v| v != &rhs).collect())),
                    _ => Err("Invalid types for '\\'".to_string()),
                },
                crate::ast::Op::In => match (l, r) {
                    (item, Value::Set(items)) => Ok(Value::Bool(items.contains(&item))),
                    (item, Value::Array(items)) => Ok(Value::Bool(items.contains(&item))),
                    _ => Ok(Value::Bool(false)),
                },
            }
        }
        _ => Err("Input must be a literal expression".to_string()),
    }
}

fn all_coords_for_shape(shape: &[i32]) -> Vec<Coord> {
    if shape.is_empty() {
        return vec![Coord(Vec::new())];
    }
    let mut coords = Vec::new();
    let mut current = vec![0; shape.len()];

    fn walk(dim: usize, shape: &[i32], current: &mut Vec<i32>, out: &mut Vec<Coord>) {
        if dim == shape.len() {
            out.push(Coord(current.clone()));
            return;
        }
        let size = shape[dim];
        if size <= 0 {
            return;
        }
        for i in 0..size {
            current[dim] = i;
            walk(dim + 1, shape, current, out);
        }
    }

    walk(0, shape, &mut current, &mut coords);
    coords
}

fn expand_index_spec(
    spec: &crate::ast::IndexSpec,
    dim_size: Option<i32>,
) -> Result<Vec<i32>, String> {
    match *spec {
        crate::ast::IndexSpec::Single(v) => Ok(vec![v]),
        crate::ast::IndexSpec::Range(start, end) => {
            let start = start.unwrap_or(0);
            let end = match end {
                Some(v) => v,
                None => {
                    let size = dim_size.ok_or_else(|| {
                        "Open-ended slices require a tensor shape".to_string()
                    })?;
                    if size <= 0 {
                        return Ok(Vec::new());
                    }
                    size - 1
                }
            };
            let (a, b) = if start <= end { (start, end) } else { (end, start) };
            Ok((a..=b).collect())
        }
    }
}

fn expand_coord_spec(specs: &[crate::ast::IndexSpec], shape: &[i32]) -> Result<Vec<Coord>, String> {
    if specs.is_empty() {
        return Ok(vec![Coord(Vec::new())]);
    }
    let mut coords = Vec::new();
    let mut current = Vec::with_capacity(specs.len());

    fn walk(
        idx: usize,
        specs: &[crate::ast::IndexSpec],
        shape: &[i32],
        current: &mut Vec<i32>,
        out: &mut Vec<Coord>,
    ) -> Result<(), String> {
        if idx == specs.len() {
            out.push(Coord(current.clone()));
            return Ok(());
        }
        let dim_size = shape.get(idx).cloned();
        let values = expand_index_spec(&specs[idx], dim_size)?;
        for v in values {
            current.push(v);
            walk(idx + 1, specs, shape, current, out)?;
            current.pop();
        }
        Ok(())
    }

    walk(0, specs, shape, &mut current, &mut coords)?;
    Ok(coords)
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Empty => false,
        Value::Int(n) => *n != 0,
        Value::Bool(b) => *b,
        _ => true,
    }
}

pub fn build_info_line() -> String {
    let date = env!("BUILD_DATE");
    let time = env!("BUILD_TIME");
    let branch = env!("BUILD_GIT_BRANCH");
    let commit = env!("BUILD_GIT_HASH");
    let commit_url = env!("BUILD_GIT_COMMIT_URL");
    let branch_url = env!("BUILD_GIT_BRANCH_URL");
    let dirty = env!("BUILD_GIT_DIRTY") == "1";

    let branch_display = if branch.is_empty() { "dev" } else { branch };
    let commit_display = if commit.is_empty() {
        "dev".to_string()
    } else if dirty {
        format!("{}*", commit)
    } else {
        commit.to_string()
    };
    let time_display = if time.is_empty() { "unknown" } else { time };
    let out = if cfg!(target_arch = "wasm32") {
        format!(
            "Build: {} {} · \x1b[4;34mbranch {}\x1b[0m · \x1b[4;34mcommit {}\x1b[0m",
            date,
            time_display,
            branch_display,
            commit_display.as_str()
        )
    } else {
        format!(
            "Build: {} {} · branch {} · commit {}",
            date,
            time_display,
            branch_display,
            commit_display.as_str()
        )
    };

    let mut out = out;

    if !cfg!(target_arch = "wasm32") {
        if !commit_url.is_empty() && commit != "dev" {
            out.push('\n');
            out.push_str(commit_url);
        }
        if !branch_url.is_empty() && branch != "dev" {
            out.push('\n');
            out.push_str(branch_url);
        }
    }

    out
}

// Demo script helpers
include!(concat!(env!("OUT_DIR"), "/demo_scripts.rs"));

fn get_demo_script_owned(n: u8) -> Option<String> {
    get_demo_script(n).map(|s| s.to_string())
}

fn format_demo_list() -> String {
    let count = get_demo_count();
    if count == 0 {
        return "No demos available.".to_string();
    }

    let mut out = String::new();
    out.push_str("Available demos:\n");
    for n in 1..=count {
        let raw = get_demo_name(n).unwrap_or("<unknown>");
        let name = raw.strip_suffix(".sheet").unwrap_or(raw);
        out.push_str(&format!("  {}: {}\n", n, name));
    }
    out.push_str("Run one with: demo <number>");
    out
}
