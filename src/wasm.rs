use wasm_bindgen::prelude::*;
use crate::interpreter::Engine;
use crate::parser::parser;
use crate::lexer::Token;
use chumsky::Parser;
use logos::Logos;
use chumsky::input::Stream;
use serde::Serialize;

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
        self.engine.tick_range(&coords);
        Ok(())
    }

    pub fn get_value(&self, cell: &str) -> String {
         if let Some(coord) = crate::interpreter::Coord::from_str(cell) {
             self.engine.state_curr.get(&coord)
                 .map(|v| v.to_string())
                 .unwrap_or_else(|| "empty".to_string())
         } else {
             "error".to_string()
         }
    }

    pub fn get_all_values(&self) -> String {
        let mut out = String::new();
        // Convert to vector to sort for deterministic output
        let mut entries: Vec<_> = self.engine.state_curr.iter().collect();
        // Sort by row then col
        entries.sort_by(|(a_coord, _), (b_coord, _)| {
            let row_cmp = a_coord.row.cmp(&b_coord.row);
            if row_cmp == std::cmp::Ordering::Equal {
                a_coord.col.cmp(&b_coord.col)
            } else {
                row_cmp
            }
        });

        for (coord, val) in entries {
             let col_char = (b'A' + coord.col as u8) as char;
             out.push_str(&format!("{}{}: {}\n", col_char, coord.row + 1, val));
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
            let row_cmp = a.row.cmp(&b.row);
            if row_cmp == std::cmp::Ordering::Equal {
                a.col.cmp(&b.col)
            } else {
                row_cmp
            }
        });

        for (coord, val) in sorted_values {
            let col_char = (b'A' + coord.col as u8) as char;
            out.push_str(&format!("{}{}: {}\n", col_char, coord.row + 1, val));
        }
        out
    }

    pub fn get_all_coords(&self) -> String {
        let coords = self.engine.get_all_coords();
        let mut sorted_coords = coords.clone();
        sorted_coords.sort_by(|a, b| {
            let row_cmp = a.row.cmp(&b.row);
            if row_cmp == std::cmp::Ordering::Equal {
                a.col.cmp(&b.col)
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
                match cmd.execute(&mut self.engine) {
                    crate::command::CommandResult::Output(text) => {
                        CommandResult::Output { text }
                    }
                    crate::command::CommandResult::BShow(coords) => {
                        let coord_names: Vec<String> = coords.iter()
                            .map(|c| c.to_cell_name())
                            .collect();
                        CommandResult::BShow { coords: coord_names }
                    }
                    crate::command::CommandResult::Demo(script) => {
                        CommandResult::Demo { script }
                    }
                    crate::command::CommandResult::Exit => {
                        CommandResult::Exit
                    }
                }
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

/// Result of executing a command - automatically generates TypeScript types
#[derive(Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum CommandResult {
    Output { text: String },
    BShow { coords: Vec<String> },
    Demo { script: String },
    Exit,
    Error {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        span_start: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        span_end: Option<usize>,
    },
}
