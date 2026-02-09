pub mod ast;
pub mod command;
pub mod command_parse;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod tests;

#[cfg(target_arch = "wasm32")]
pub mod wasm;
