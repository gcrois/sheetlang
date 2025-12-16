mod ast;
mod interpreter;
mod lexer;
mod parser;

use ariadne::{Color, Label, Report, ReportKind, Source};
use ast::Expr;
use chumsky::input::Stream;
use chumsky::prelude::*;
use interpreter::Engine;
use lexer::Token;
use logos::Logos;
use rustyline::DefaultEditor;

fn main() {
    let mut rl = DefaultEditor::new().unwrap();
    let mut engine = Engine::new();

    println!("SheetLang 0.2.0 - Tick Engine");
    println!("Commands: 'tick', 'show', or assignments 'A1 = ...'");

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let line_str = line.trim();
                if line_str.is_empty() {
                    continue;
                }
                if line_str == "exit" {
                    break;
                }

                if line_str == "tick" {
                    engine.tick();
                    println!("Tick processed.");
                    continue;
                }

                if line_str == "show" {
                    println!("Current State:");
                    for (coord, val) in &engine.state_curr {
                        let col_char = (b'A' + coord.col as u8) as char;
                        println!("{}{}: {}", col_char, coord.row + 1, val);
                    }
                    continue;
                }

                let token_iter = Token::lexer(line_str)
                    .spanned()
                    .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
                let input = Stream::from_iter(token_iter);
                let parse_result = parser::parser().parse(input);

                match parse_result.into_result() {
                    Ok(expr) => match expr {
                        Expr::Assign(target, body) => {
                            if let Err(e) = engine.set_formula(&target, *body) {
                                println!("Error: {}", e);
                            } else {
                                println!("Formula set for {}", target);
                            }
                        }
                        _ => {
                            println!("Expression parsed (use 'A1 = ...' to assign): {:?}", expr);
                        }
                    },
                    Err(errors) => {
                        for err in errors {
                            // Fix: Explicitly specify Range<usize> type and use Label/Color
                            Report::<std::ops::Range<usize>>::build(
                                ReportKind::Error,
                                (),
                                err.span().start,
                            )
                            .with_message(err.to_string())
                            .with_label(
                                Label::new(err.span().into_range())
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

#[cfg(test)]
mod tests;
