use crate::ast::{CellRange, Expr, Value};
use crate::interpreter::{Engine, Coord};
use crate::lexer::Token;
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Tick(Option<CellRange>),
    Show(Option<CellRange>),
    BShow(Option<CellRange>),
    Demo(u8),
    Help,
    Encode,
    Exit,
    Eval(Expr),
}

impl Command {
    fn base_name(&self) -> &'static str {
        match self {
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
    let ident = select! { Token::Ident(s) => s };

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
            Command::Help => CommandResult::Output(Self::format_help()),

            Command::Tick(range_opt) => {
                if let Some(range) = range_opt {
                    let coords: Vec<Coord> = range
                        .expand()
                        .iter()
                        .filter_map(|s| Coord::from_str(s))
                        .collect();
                    engine.tick_range(&coords);
                    CommandResult::Output(format!(
                        "Tick processed for range {}:{}",
                        range.start.to_string(),
                        range.end.to_string()
                    ))
                } else {
                    engine.tick();
                    CommandResult::Output("Tick processed.".to_string())
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
                        let row_cmp = a.row.cmp(&b.row);
                        if row_cmp == std::cmp::Ordering::Equal {
                            a.col.cmp(&b.col)
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
                    let mut entries: Vec<_> = engine.state_curr.iter().collect();
                    entries.sort_by(|(a, _), (b, _)| {
                        let row_cmp = a.row.cmp(&b.row);
                        if row_cmp == std::cmp::Ordering::Equal {
                            a.col.cmp(&b.col)
                        } else {
                            row_cmp
                        }
                    });

                    let mut out = "Current State:\n".to_string();
                    for (coord, val) in entries {
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
                    engine.get_all_coords()
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
                                    if let Some(coord) = Coord::from_str(cell_name) {
                                        if let Some(item_expr) = items.get(i) {
                                            engine.formulas.insert(coord, item_expr.clone());
                                            count += 1;
                                        } else {
                                            engine.formulas.insert(coord, Expr::Literal(Value::Empty));
                                            count += 1;
                                        }
                                    }
                                }
                            }
                            _ => {
                                for cell_name in expanded_coords.iter() {
                                    if let Some(coord) = Coord::from_str(cell_name) {
                                        engine.formulas.insert(coord, (*body).clone());
                                        count += 1;
                                    }
                                }
                            }
                        }

                        format!("Formula set for {} cells", count)
                    }
                    Expr::Assign(target, body) => {
                        if let Some(coord) = Coord::from_str(&target) {
                            engine.formulas.insert(coord, *body);
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
