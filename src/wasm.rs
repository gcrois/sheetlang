use wasm_bindgen::prelude::*;
use crate::interpreter::Engine;
use crate::parser::parser;
use crate::lexer::Token;
use chumsky::Parser;
use logos::Logos;

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

    pub fn build_timestamp() -> String {
        format!("{} {}", env!("BUILD_DATE"), env!("BUILD_TIME"))
    }
}
