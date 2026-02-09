use chumsky::Parser;
use logos::Logos;
use sheetlang::ast::Value;
use sheetlang::command::{command_parser, CommandResult};
use sheetlang::interpreter::{Coord, Engine};
use sheetlang::lexer::Token;

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
