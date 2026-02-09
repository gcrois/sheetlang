use crate::ast::{CellRange, Expr, Value};
use crate::interpreter::{Engine, Coord};
use crate::lexer::Token;
use chumsky::prelude::*;

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
            Command::Demo(_) => "demo <number>".to_string(),
            Command::Help => "help".to_string(),
            Command::Encode => "encode".to_string(),
            Command::Exit => "exit".to_string(),
            Command::Eval(Expr::Assign(_, _)) => "A1 = <expr>".to_string(),
            Command::Eval(Expr::RangeAssign(_, _)) => "A1:C3 = <expr>".to_string(),
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
            Command::Demo(_) => "Load and run a demo script",
            Command::Help => "Show this help message",
            Command::Encode => "Generate shareable URL from command history",
            Command::Exit => "Exit the CLI",
            Command::Eval(Expr::Assign(_, _)) => "Set a formula for a single cell",
            Command::Eval(Expr::RangeAssign(_, _)) => "Set a formula for a range of cells",
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
            Command::Demo(_) => vec!["demo 1", "demo 9"],
            Command::Help => vec!["help"],
            Command::Encode => vec!["encode"],
            Command::Exit => vec!["exit"],
            Command::Eval(Expr::Assign(_, _)) => vec!["A1 = 10", "B1 = A1 + 5"],
            Command::Eval(Expr::RangeAssign(_, _)) => vec!["A1:C3 = 0", "A1:C3 = @[0,0] * 2"],
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
            Command::Demo(1),
            Command::Eval(Expr::Assign("A1".to_string(), Box::new(Expr::Literal(crate::ast::Value::Int(0))))),
            Command::Eval(Expr::RangeAssign(
                crate::ast::CellRange {
                    start: crate::ast::CellCoord { col: "A".to_string(), row: 1 },
                    end: crate::ast::CellCoord { col: "C".to_string(), row: 3 },
                    step: None,
                },
                Box::new(Expr::Literal(crate::ast::Value::Int(0))),
            )),
            Command::Help,
            Command::Encode,
            Command::Exit,
        ]
    }

    pub fn print_help() {
        println!("{}", Self::format_help());
    }
}

/// Parser for CLI commands that shares grammar with the expression parser
pub fn command_parser<'src, I>() -> impl Parser<'src, I, Command, extra::Err<Rich<'src, Token>>>
where
    I: Input<'src, Token = Token, Span = SimpleSpan>,
{
    let expr_parser = crate::parser::parser();
    let expr_parser_set = crate::parser::parser();
    let ident = select! { Token::Ident(s) => s };
    let int_list = select! { Token::Int(n) => n as i32 }
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket));

    // Parse cell coordinate (A1, B2, etc.)
    let cell_coord = ident.clone().try_map(|s, span| {
        let mut chars = s.chars();
        let _first = match chars.next() {
            Some(c) if c.is_ascii_uppercase() => c,
            _ => return Err(Rich::custom(span, format!("'{}' is not a cell reference (must start with uppercase letter)", s))),
        };

        let rest: String = chars.collect();
        if rest.is_empty() || !rest.chars().all(|c| c.is_ascii_digit()) {
            return Err(Rich::custom(span, format!("'{}' is not a cell reference (must be like A1, B2, etc.)", s)));
        }

        crate::ast::CellCoord::from_str(&s)
            .ok_or_else(|| Rich::custom(span, "Invalid cell reference"))
    });

    // Parse cell range (A1:C3 or A1:C3:2)
    let cell_range = cell_coord
        .clone()
        .then_ignore(just(Token::Colon))
        .then(cell_coord)
        .then(
            just(Token::Colon)
                .ignore_then(select! { Token::Int(n) => n as i32 })
                .or_not(),
        )
        .map(|((start, end), step)| CellRange { start, end, step });

    // tick [range]
    let tick_cmd = ident
        .clone()
        .filter(|s| s == "tick")
        .ignore_then(cell_range.clone().or_not())
        .map(Command::Tick);

    // alloc <name> [dims]
    let alloc_cmd = ident
        .clone()
        .filter(|s| s == "alloc")
        .ignore_then(ident.clone())
        .then(int_list.clone())
        .map(|(name, shape)| Command::Alloc { name, shape });

    // use <name>
    let use_cmd = ident
        .clone()
        .filter(|s| s == "use")
        .ignore_then(ident.clone())
        .map(|name| Command::Use { name });

    // view axes [a,b] offset [o0,...] (order flexible, parts optional)
    let axes_clause = ident
        .clone()
        .filter(|s| s == "axes")
        .ignore_then(int_list.clone());
    let offset_clause = ident
        .clone()
        .filter(|s| s == "offset")
        .ignore_then(int_list.clone());
    let view_cmd = ident
        .clone()
        .filter(|s| s == "view")
        .ignore_then(choice((
            axes_clause.clone().then(offset_clause.clone()).map(|(axes, offset)| (Some(axes), Some(offset))),
            offset_clause.clone().then(axes_clause.clone()).map(|(offset, axes)| (Some(axes), Some(offset))),
            axes_clause.clone().map(|axes| (Some(axes), None)),
            offset_clause.clone().map(|offset| (None, Some(offset))),
        )))
        .map(|(axes, offset)| Command::View { axes, offset });

    // set A1 <expr> (literal only at execution)
    let set_cmd = ident
        .clone()
        .filter(|s| s == "set")
        .ignore_then(ident.clone())
        .then(expr_parser_set)
        .map(|(target, value)| Command::SetInput { target, value });

    // effects auto N | effects run [N] | effects pending
    let effects_cmd = ident
        .clone()
        .filter(|s| s == "effects")
        .ignore_then(choice((
            ident
                .clone()
                .filter(|s| s == "auto")
                .ignore_then(select! { Token::Int(n) => n as usize })
                .map(EffectsCommand::Auto),
            ident
                .clone()
                .filter(|s| s == "run")
                .ignore_then(select! { Token::Int(n) => n as usize }.or_not())
                .map(EffectsCommand::Run),
            ident
                .clone()
                .filter(|s| s == "pending")
                .to(EffectsCommand::Pending),
        )))
        .map(Command::Effects);

    // show [range]
    let show_cmd = ident
        .clone()
        .filter(|s| s == "show")
        .ignore_then(cell_range.clone().or_not())
        .map(Command::Show);

    // bshow [range]
    let bshow_cmd = ident
        .clone()
        .filter(|s| s == "bshow")
        .ignore_then(cell_range.clone().or_not())
        .map(Command::BShow);

    // help
    let help_cmd = ident
        .clone()
        .filter(|s| s == "help")
        .to(Command::Help);

    // encode
    let encode_cmd = ident
        .clone()
        .filter(|s| s == "encode")
        .to(Command::Encode);

    // demo <n>
    let demo_cmd = ident
        .clone()
        .filter(|s| s == "demo")
        .ignore_then(select! { Token::Int(n) => n as u8 })
        .map(Command::Demo);

    // exit
    let exit_cmd = ident
        .clone()
        .filter(|s| s == "exit")
        .to(Command::Exit);

    // Try command parsers first, then fall back to expression parser
    choice((
        alloc_cmd,
        use_cmd,
        view_cmd,
        set_cmd,
        effects_cmd,
        tick_cmd,
        show_cmd,
        bshow_cmd,
        demo_cmd,
        help_cmd,
        encode_cmd,
        exit_cmd,
    ))
    .or(expr_parser.map(Command::Eval))
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
}

impl Command {
    /// Execute a command against an engine
    pub fn execute(self, engine: &mut Engine) -> CommandResult {
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
                        let executed = engine.run_effects(limit);
                        let remaining = engine.pending_effects_len();
                        CommandResult::Output(format!(
                            "Executed {} effect(s). Pending: {}",
                            executed, remaining
                        ))
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
                let output = if let Some(range) = range_opt {
                    let coords: Vec<Coord> = range
                        .expand()
                        .iter()
                        .filter_map(|s| Coord::from_str(s))
                        .collect();
                    let values = engine.get_values_in_range(&coords);
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

                    let mut out = format!("Current State (range {}:{}):\n", range.start.to_string(), range.end.to_string());
                    for (coord, val) in sorted_values {
                        out.push_str(&format!("{}: {}\n", coord.to_cell_name(), val));
                    }
                    out
                } else {
                    let coords = engine.get_all_coords_in_view();
                    let values = engine.get_values_in_range(&coords);
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

                    let mut out = "Current State (view):\n".to_string();
                    for (coord, val) in sorted_values {
                        out.push_str(&format!("{}: {}\n", coord.to_cell_name(), val));
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

            Command::Demo(n) => {
                match get_demo_script(n) {
                    Some(script) => CommandResult::Demo(script.to_string()),
                    None => {
                        CommandResult::Output(format!(
                            "Error: Demo {} not found. Available demos: 1-{}",
                            n,
                            get_demo_count()
                        ))
                    }
                }
            }

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
                            engine.set_formula_coord(coord, *body);
                            format!("Formula set for {}", target)
                        } else {
                            format!("Error: Invalid cell reference {}", target)
                        }
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
            }
        }
        _ => Err("Input must be a literal expression".to_string()),
    }
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

// Include generated demo scripts code
include!(concat!(env!("OUT_DIR"), "/demo_scripts.rs"));
