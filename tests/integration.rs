use chumsky::prelude::*;
use logos::Logos;
use sheetlang::ast::Expr;
use sheetlang::interpreter::Engine;
use sheetlang::lexer::Token;
use sheetlang::parser::parser;
use std::fs;

#[test]
fn test_snapshots() {
    // glob! automatically suffixes the snapshot name with the filename
    // e.g. 'basic.sheet' -> 'test_snapshots__basic.snap'
    insta::glob!("scripts/*.sheet", |path| {
        let input = fs::read_to_string(path).unwrap();

        let mut engine = Engine::new();
        let mut output = String::new();
        
        // Store (CellName, ExpectedString)
        let mut expectations: Vec<(String, String)> = Vec::new();

        output.push_str("--- CODE ---\n");
        output.push_str(&input);
        output.push_str("\n\n--- PARSE & EXECUTION ---\n");

        for line in input.lines() {
            // 1. Split Code and Comment (e.g. "A1 = 5 // = 5")
            let parts: Vec<&str> = line.splitn(2, "//").collect();
            let code_part = parts[0].trim();
            let comment_part = if parts.len() > 1 {
                Some(parts[1].trim())
            } else {
                None
            };

            if code_part.is_empty() {
                continue;
            }

            // 2. Parse Code
            let token_iter = Token::lexer(code_part)
                .spanned()
                .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
            let token_stream = chumsky::input::Stream::from_iter(token_iter);

            match parser().parse(token_stream).into_result() {
                Ok(expr) => {
                    if let Expr::Assign(target, body) = expr {
                        // Apply formula
                        if let Err(e) = engine.set_formula(&target, *body) {
                            output.push_str(&format!("Error setting {}: {}\n", target, e));
                        }

                        // 3. Capture Expectation if present
                        // Syntax: // = <value>
                        if let Some(comment) = comment_part {
                            if let Some(expected_val) = comment.strip_prefix('=') {
                                expectations.push((target, expected_val.trim().to_string()));
                            }
                        }
                    }
                }
                Err(errs) => {
                    for e in errs {
                        output.push_str(&format!("Parse Error on '{}': {:?}\n", code_part, e));
                    }
                }
            }
        }

        // 3. Run Ticks
        output.push_str("\n[Tick 1]\n");
        engine.tick();
        output.push_str(&dump_state(&engine));

        output.push_str("\n[Tick 2]\n");
        engine.tick();
        output.push_str(&dump_state(&engine));

        // 4. Verify Expectations (against final state)
        if !expectations.is_empty() {
            output.push_str("\n--- CHECKS ---\n");
            for (cell, expected) in expectations {
                let actual_val = get_val_str(&engine, &cell);
                if actual_val == expected {
                    output.push_str(&format!("✅ {}: Matches ({})\n", cell, actual_val));
                } else {
                    output.push_str(&format!(
                        "❌ {}: FAILED (Expected '{}', got '{}')\n",
                        cell, expected, actual_val
                    ));
                }
            }
        }

        insta::assert_snapshot!(output);
    });
}

fn get_val_str(engine: &Engine, cell: &str) -> String {
    if let Some(coord) = sheetlang::interpreter::Coord::from_str(cell) {
        engine
            .state_curr
            .get(&coord)
            .map(|v| v.to_string())
            .unwrap_or_else(|| "empty".to_string())
    } else {
        "error".to_string()
    }
}

fn dump_state(engine: &Engine) -> String {
    let mut out = String::new();
    let mut keys: Vec<_> = engine.state_curr.keys().collect();
    keys.sort_by_key(|c| (c.row, c.col));

    for k in keys {
        let col_char = (b'A' + k.col as u8) as char;
        let val = engine.state_curr.get(k).unwrap();
        out.push_str(&format!("{}{}: {}\n", col_char, k.row + 1, val));
    }
    if out.is_empty() {
        return "(empty state)\n".to_string();
    }
    out
}
