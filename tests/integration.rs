use chumsky::Parser;
use logos::Logos;
use sheetlang::ast::Value;
use sheetlang::command::{command_parser, execute_with_auto_effects, CommandResult};
use sheetlang::interpreter::{Coord, Engine};
use sheetlang::lexer::Token;
use std::fs;

fn dump_state(engine: &Engine) -> String {
    let mut out = String::new();
    let coords = engine.get_all_coords_in_view();
    let tensor = match engine.tensors.get(&engine.active) {
        Some(t) => t,
        None => return "(empty state)\n".to_string(),
    };
    let mut entries: Vec<(Coord, Coord, Value, Option<sheetlang::ast::Expr>, Option<Value>)> = Vec::new();
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

    for (_coord2d, nd, val, formula, input) in entries {
        let label = nd.to_string();
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
        out.push_str(&line);
        out.push('\n');
    }
    if out.is_empty() {
        return "(empty state)\n".to_string();
    }
    out
}

fn run_script(engine: &mut Engine, input: &str) -> String {
    let mut output = String::new();
    output.push_str("--- SCRIPT ---\n");

    let mut tick_count = 0;
    for line in input.lines() {
        let code_part = line.splitn(2, "//").next().unwrap_or("").trim();
        let raw_line = line.trim_end();
        if raw_line.is_empty() {
            continue;
        }
        output.push_str("> ");
        output.push_str(raw_line);
        output.push('\n');

        if code_part.is_empty() {
            continue;
        }

        let token_iter = Token::lexer(code_part)
            .spanned()
            .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
        let token_stream = chumsky::input::Stream::from_iter(token_iter);
        let cmd = match command_parser().parse(token_stream).into_result() {
            Ok(cmd) => cmd,
            Err(errs) => {
                for e in errs {
                    output.push_str(&format!("Parse Error on '{}': {:?}\n", code_part, e));
                }
                continue;
            }
        };

        let result = execute_with_auto_effects(cmd, engine);
        append_result(&result, engine, &mut output, &mut tick_count);
    }

    output
}

fn append_result(result: &CommandResult, engine: &Engine, output: &mut String, tick_count: &mut usize) {
    match result {
        CommandResult::Output(text) => {
            if !text.is_empty() {
                output.push_str(text);
                output.push('\n');
            }
            if text.contains("Tick processed") {
                *tick_count += 1;
                output.push_str(&format!("\n[Tick {}]\n", tick_count));
                output.push_str(&dump_state(engine));
            }
        }
        CommandResult::BShow(_) => {}
        CommandResult::Demo(_) => {}
        CommandResult::Exit => {}
        CommandResult::Batch(results) => {
            for result in results {
                append_result(result, engine, output, tick_count);
            }
        }
    }
}

fn exec_lines(engine: &mut Engine, lines: &[&str]) -> Vec<CommandResult> {
    let mut results = Vec::new();
    for line in lines {
        let token_iter = Token::lexer(line)
            .spanned()
            .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
        let token_stream = chumsky::input::Stream::from_iter(token_iter);
        let cmd = command_parser().parse(token_stream).into_result().unwrap();
        results.push(execute_with_auto_effects(cmd, engine));
    }
    results
}

fn get_val(engine: &Engine, cell: &str) -> Value {
    let c2d = Coord::from_str(cell).unwrap();
    let coord = engine.map_view_coord(&c2d).unwrap();
    let tensor = engine.tensors.get(&engine.active).unwrap();
    tensor.state_curr.get(&coord).cloned().unwrap_or(Value::Empty)
}

#[test]
fn test_script_snapshots() {
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(|| {
            insta::glob!("scripts/*.sheet", |path| {
                let input = fs::read_to_string(path).unwrap();
                let mut engine = Engine::new();
                let output = run_script(&mut engine, &input);
                insta::assert_snapshot!(output);
            });
        })
        .expect("Failed to spawn snapshot thread")
        .join()
        .expect("Snapshot thread panicked");
}

#[test]
fn test_command_flow_numpy_model() {
    let mut engine = Engine::new();
    let lines = [
        "alloc cube [2,2,2]",
        "view axes [0,2] offset [0,1,0]",
        "set A1 7",
        "B1 = #[0,1,0] + 5",
        "tick",
    ];

    exec_lines(&mut engine, &lines);
    assert_eq!(get_val(&engine, "B1"), Value::Int(12));
}

#[test]
fn test_effects_commands() {
    let mut engine = Engine::new();
    let lines = [
        "A1 = tick()",
        "tick",
        "effects pending",
        "effects run 1",
    ];

    let results = exec_lines(&mut engine, &lines);

    match &results[2] {
        CommandResult::Output(text) => assert!(text.contains("Pending effects")),
        _ => panic!("Expected output for effects pending"),
    }
}

#[test]
fn test_set_rejects_non_literal() {
    let mut engine = Engine::new();
    let lines = [
        "A1 = 5",
        "set B1 A1",
    ];

    let results = exec_lines(&mut engine, &lines);
    match &results[1] {
        CommandResult::Output(text) => assert!(text.contains("Input must be a literal")),
        _ => panic!("Expected output for set B1 A1"),
    }
}
