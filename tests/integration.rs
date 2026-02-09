use chumsky::Parser;
use logos::Logos;
use sheetlang::ast::Value;
use sheetlang::command::{command_parser, CommandResult};
use sheetlang::interpreter::{Coord, Engine};
use sheetlang::lexer::Token;
use std::fs;

fn dump_state(engine: &Engine) -> String {
    let mut out = String::new();
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

    for (coord, val) in sorted_values {
        out.push_str(&format!("{}: {}\n", coord.to_cell_name(), val));
    }
    if out.is_empty() {
        return "(empty state)\n".to_string();
    }
    out
}

fn run_script(engine: &mut Engine, input: &str) -> String {
    let mut output = String::new();
    output.push_str("--- CODE ---\n");
    output.push_str(input);
    output.push_str("\n\n--- EXECUTION ---\n");

    let mut tick_count = 0;
    for line in input.lines() {
        let code_part = line.splitn(2, "//").next().unwrap_or("").trim();
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

        match cmd.execute(engine) {
            CommandResult::Output(text) => {
                if !text.is_empty() {
                    output.push_str(&text);
                    output.push('\n');
                }
                if text.contains("Tick processed") {
                    tick_count += 1;
                    output.push_str(&format!("\n[Tick {}]\n", tick_count));
                    output.push_str(&dump_state(engine));
                }
            }
            CommandResult::BShow(_) => {}
            CommandResult::Demo(_) => {}
            CommandResult::Exit => {}
        }
    }

    output
}

fn exec_lines(engine: &mut Engine, lines: &[&str]) -> Vec<CommandResult> {
    let mut results = Vec::new();
    for line in lines {
        let token_iter = Token::lexer(line)
            .spanned()
            .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
        let token_stream = chumsky::input::Stream::from_iter(token_iter);
        let cmd = command_parser().parse(token_stream).into_result().unwrap();
        results.push(cmd.execute(engine));
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
