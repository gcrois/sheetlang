use ariadne::{Color, Label, Report, ReportKind, Source};
use ast::Value;
use chumsky::input::Stream;
use chumsky::prelude::*;
use command::{Command, CommandResult};
use interpreter::Engine;
use lexer::Token;
use logos::Logos;
use rustyline::DefaultEditor;
use sheetlang::{ast, command, interpreter, lexer};

fn main() {
    let mut rl = DefaultEditor::new().unwrap();
    let mut engine = Engine::new();

    println!("SheetLang");
    println!("Type 'help' for available commands");

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let line_str = line.trim();
                if line_str.is_empty() {
                    continue;
                }

                // Parse the command
                let token_iter = Token::lexer(line_str)
                    .spanned()
                    .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
                let input = Stream::from_iter(token_iter);

                let cmd_parser = command::command_parser();
                let parse_result = cmd_parser.parse(input);

                match parse_result.into_result() {
                    Ok(cmd) => {
                        match execute_command(cmd, &mut engine) {
                            Ok(Some(script)) => {
                                // Execute demo script line by line
                                println!("Loading demo...\n");
                                for line in script.lines() {
                                    let trimmed = line.trim();
                                    if trimmed.is_empty() {
                                        continue;
                                    }

                                    // Lex and collect tokens
                                    let tokens: Vec<_> = Token::lexer(trimmed)
                                        .spanned()
                                        .map(|(tok, _span)| tok.unwrap_or(Token::Dot))
                                        .collect();

                                    // Skip lines with no tokens (e.g., comment-only lines)
                                    if tokens.is_empty() {
                                        continue;
                                    }

                                    println!(">> {}", trimmed);

                                    // Parse and execute the line
                                    let input = Stream::from_iter(tokens.into_iter());
                                    let cmd_parser = command::command_parser();
                                    let parse_result = cmd_parser.parse(input);

                                    match parse_result.into_result() {
                                        Ok(inner_cmd) => {
                                            if let Err(e) = execute_command(inner_cmd, &mut engine) {
                                                if e == "exit" {
                                                    break;
                                                }
                                                println!("Error: {}", e);
                                            }
                                        }
                                        Err(_) => {
                                            // Silently skip parse errors in demo
                                        }
                                    }
                                }
                                println!();
                            }
                            Ok(None) => {}
                            Err(e) => {
                                if e == "exit" {
                                    break;
                                }
                                println!("Error: {}", e);
                            }
                        }
                    }
                    Err(errors) => {
                        for err in errors {
                            Report::build(ReportKind::Error, ((), err.span().into_range()))
                                .with_message(err.to_string())
                                .with_label(
                                    Label::new(((), err.span().into_range()))
                                        .with_message(err.reason().to_string())
                                        .with_color(Color::Red),
                                )
                                .finish()
                                .print(Source::from(line_str))
                                .unwrap();
                        }
                    }
                }
            }
            Err(_) => break,
        }
    }
}

fn execute_command(cmd: Command, engine: &mut Engine) -> Result<Option<String>, String> {
    match cmd.execute(engine) {
        CommandResult::Output(text) => {
            print!("{}", text);
            Ok(None)
        }
        CommandResult::BShow(coords) => {
            render_bshow(&coords, engine);
            Ok(None)
        }
        CommandResult::Demo(script) => {
            // Return the script to be executed by the main loop
            Ok(Some(script))
        }
        CommandResult::Exit => Err("exit".to_string()),
    }
}

fn render_bshow(coords: &[interpreter::Coord], engine: &Engine) {
    if coords.is_empty() {
        println!("(empty)");
        return;
    }

    // Find bounds
    let min_col = coords.iter().map(|c| c.col).min().unwrap();
    let max_col = coords.iter().map(|c| c.col).max().unwrap();
    let min_row = coords.iter().map(|c| c.row).min().unwrap();
    let max_row = coords.iter().map(|c| c.row).max().unwrap();

    // Build grid
    let width = (max_col - min_col + 1) as usize;
    let height = (max_row - min_row + 1) as usize;
    let mut grid = vec![vec![false; width]; height];

    for coord in coords {
        if let Some(val) = engine.state_curr.get(coord) {
            let is_truthy = match val {
                Value::Empty => false,
                Value::Int(n) => *n != 0,
                Value::Bool(b) => *b,
                _ => true,
            };
            let grid_row = (coord.row - min_row) as usize;
            let grid_col = (coord.col - min_col) as usize;
            grid[grid_row][grid_col] = is_truthy;
        }
    }

    // Display
    let cell_width = 2;
    let full_block = "█";
    let empty_block = " ";

    println!("┌{}┐", "─".repeat(width * cell_width));
    for row in &grid {
        let row_str: String = row
            .iter()
            .map(|&is_truthy| {
                if is_truthy {
                    full_block.repeat(cell_width)
                } else {
                    empty_block.repeat(cell_width)
                }
            })
            .collect();
        println!("│{}│", row_str);
    }
    println!("└{}┘", "─".repeat(width * cell_width));
}
