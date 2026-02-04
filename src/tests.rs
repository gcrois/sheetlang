#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Value};
    use crate::interpreter::{Coord, Engine};
    use crate::lexer::Token;
    use crate::parser::parser;
    use chumsky::prelude::*;
    use logos::Logos;

    // --- Helper to parse a string into an Expr ---
    fn parse_expr(input: &str) -> Expr {
        let token_iter = Token::lexer(input)
            .spanned()
            .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
        let token_stream = chumsky::input::Stream::from_iter(token_iter);
        parser()
            .parse(token_stream)
            .into_result()
            .expect("Parse failed")
    }

    // --- Helper to get value from engine easily ---
    fn get_val(engine: &Engine, cell: &str) -> Value {
        let c = Coord::from_str(cell).unwrap();
        engine.state_curr.get(&c).cloned().unwrap_or(Value::Empty)
    }

    // --- Helper to assert integer values ---
    fn assert_int(engine: &Engine, cell: &str, expected: i64) {
        let val = get_val(engine, cell);
        if let Value::Int(n) = val {
            assert_eq!(
                n, expected,
                "Cell {} expected {}, got {}",
                cell, expected, n
            );
        } else {
            panic!("Cell {} expected Int({}), got {:?}", cell, expected, val);
        }
    }

    #[test]
    fn test_basic_arithmetic() {
        let mut engine = Engine::new();
        engine.set_formula("A1", parse_expr("1 + 2")).unwrap();
        engine.set_formula("A2", parse_expr("2 * 3 + 4")).unwrap(); // 10
        engine.set_formula("A3", parse_expr("2 * (3 + 4)")).unwrap(); // 14

        engine.tick();

        assert_int(&engine, "A1", 3);
        assert_int(&engine, "A2", 10);
        assert_int(&engine, "A3", 14);
    }

    #[test]
    fn test_double_buffering_propagation() {
        // In a tick-based system:
        // Tick 1: A1 becomes 10. B1 reads OLD A1 (empty/0) -> 0.
        // Tick 2: A1 stays 10. B1 reads NEW A1 (10) -> 20.

        let mut engine = Engine::new();
        engine.set_formula("A1", parse_expr("10")).unwrap();
        engine.set_formula("B1", parse_expr("A1 * 2")).unwrap(); // A1 is 0 initially

        // --- Tick 1 ---
        engine.tick();
        assert_int(&engine, "A1", 10);
        // B1 should be 0 (or empty treated as 0) because A1 was empty in prev state
        let val_b1 = get_val(&engine, "B1");
        match val_b1 {
            Value::Int(0) | Value::Empty => {} // Pass
            _ => panic!("B1 should be 0 or Empty on tick 1, got {:?}", val_b1),
        }

        // --- Tick 2 ---
        engine.tick();
        assert_int(&engine, "A1", 10);
        assert_int(&engine, "B1", 20); // Now it sees A1=10
    }

    #[test]
    fn test_relative_references() {
        let mut engine = Engine::new();
        // A1 = 10
        // A2 = (Cell Above) + 5
        engine.set_formula("A1", parse_expr("10")).unwrap();
        engine
            .set_formula("A2", parse_expr("@[0, -1] + 5"))
            .unwrap();

        engine.tick(); // A1=10, A2=5 (sees empty A1)
        engine.tick(); // A1=10, A2=15 (sees A1=10)

        assert_int(&engine, "A2", 15);
    }

    #[test]
    fn test_cycles_counter() {
        // A1 = (A1 || 0) + 1
        let mut engine = Engine::new();
        engine
            .set_formula("A1", parse_expr("(A1 || 0) + 1"))
            .unwrap();

        engine.tick();
        assert_int(&engine, "A1", 1);

        engine.tick();
        assert_int(&engine, "A1", 2);

        engine.tick();
        assert_int(&engine, "A1", 3);
    }

    #[test]
    fn test_lambdas() {
        let mut engine = Engine::new();
        // A1: Define function square(x) = x * x
        engine
            .set_formula("A1", parse_expr("(x) => x * x"))
            .unwrap();
        // A2: Call A1(5)
        engine.set_formula("A2", parse_expr("A1(5)")).unwrap();

        engine.tick(); // A1 defines fn. A2 tries to call A1 (which was empty in prev) -> Empty.
        engine.tick(); // A2 calls A1 (now a function) -> 25.

        assert_int(&engine, "A2", 25);
    }

    #[test]
    fn test_lambda_scopes() {
        // Test that lambdas don't accidentally read global state when they should read args
        let mut engine = Engine::new();
        engine.set_formula("A1", parse_expr("100")).unwrap();
        // Function takes 'a', ignores global A1
        engine
            .set_formula("B1", parse_expr("(a) => a + 1"))
            .unwrap();
        engine.set_formula("C1", parse_expr("B1(10)")).unwrap();

        engine.tick();
        engine.tick();

        assert_int(&engine, "C1", 11); // Should be 10+1, not 100+1
    }

    #[test]
    fn test_complex_structures() {
        let mut engine = Engine::new();
        // A1 = [10, 20, 30]
        engine
            .set_formula("A1", parse_expr("[10, 20, 30]"))
            .unwrap();
        // A2 = A1[1]
        engine.set_formula("A2", parse_expr("A1[1]")).unwrap();

        engine.tick(); // A1 set, A2 reads empty
        engine.tick(); // A2 reads array

        assert_int(&engine, "A2", 20);
    }

    #[test]
    fn test_boolean_logic() {
        let mut engine = Engine::new();
        // 1 || 0 -> 1
        engine.set_formula("A1", parse_expr("1 || 0")).unwrap();
        // 0 || 5 -> 5
        engine.set_formula("A2", parse_expr("0 || 5")).unwrap();

        engine.tick();
        assert_int(&engine, "A1", 1);
        assert_int(&engine, "A2", 5);
    }

    #[test]
    fn test_cell_coord_parsing() {
        use crate::ast::CellCoord;

        assert_eq!(
            CellCoord::from_str("A1"),
            Some(CellCoord {
                col: "A".to_string(),
                row: 1
            })
        );
        assert_eq!(
            CellCoord::from_str("Z99"),
            Some(CellCoord {
                col: "Z".to_string(),
                row: 99
            })
        );
        assert_eq!(CellCoord::from_str("A0"), None); // Row must be >= 1
        assert_eq!(CellCoord::from_str("a1"), None); // Must be uppercase
        assert_eq!(CellCoord::from_str("AA1"), None); // Only single letter
        assert_eq!(CellCoord::from_str("A"), None); // Must have row number
    }

    #[test]
    fn test_cell_range_expansion_1d_vertical() {
        use crate::ast::CellRange;

        let range = CellRange::from_str("A1:A5").unwrap();
        assert_eq!(range.expand(), vec!["A1", "A2", "A3", "A4", "A5"]);
    }

    #[test]
    fn test_cell_range_expansion_1d_horizontal() {
        use crate::ast::CellRange;

        let range = CellRange::from_str("A1:E1").unwrap();
        assert_eq!(range.expand(), vec!["A1", "B1", "C1", "D1", "E1"]);
    }

    #[test]
    fn test_cell_range_expansion_2d() {
        use crate::ast::CellRange;

        let range = CellRange::from_str("A1:C2").unwrap();
        assert_eq!(
            range.expand(),
            vec!["A1", "B1", "C1", "A2", "B2", "C2"]
        );
    }

    #[test]
    fn test_cell_range_expansion_with_step() {
        use crate::ast::CellRange;

        let range = CellRange::from_str("A1:A10:2").unwrap();
        assert_eq!(range.expand(), vec!["A1", "A3", "A5", "A7", "A9"]);
    }

    #[test]
    fn test_cell_range_backwards() {
        use crate::ast::CellRange;

        // Should auto-swap to forward range
        let range = CellRange::from_str("A5:A1").unwrap();
        assert_eq!(range.expand(), vec!["A1", "A2", "A3", "A4", "A5"]);
    }

    #[test]
    fn test_slice_assignment_broadcast() {
        use crate::ast::CellRange;

        let mut engine = Engine::new();

        // Manually simulate what integration.rs would do
        let range = CellRange::from_str("A1:A3").unwrap();
        let value_expr = Expr::Literal(Value::Int(10));

        for coord in range.expand() {
            engine.set_formula(&coord, value_expr.clone()).unwrap();
        }

        engine.tick();

        assert_int(&engine, "A1", 10);
        assert_int(&engine, "A2", 10);
        assert_int(&engine, "A3", 10);
    }

    #[test]
    fn test_slice_assignment_array_expansion() {
        use crate::ast::CellRange;

        let mut engine = Engine::new();

        let range = CellRange::from_str("B1:B3").unwrap();
        let coords = range.expand();
        let array_items = vec![
            Expr::Literal(Value::Int(1)),
            Expr::Literal(Value::Int(2)),
            Expr::Literal(Value::Int(3)),
        ];

        for (i, coord) in coords.iter().enumerate() {
            if let Some(item) = array_items.get(i) {
                engine.set_formula(coord, item.clone()).unwrap();
            }
        }

        engine.tick();

        assert_int(&engine, "B1", 1);
        assert_int(&engine, "B2", 2);
        assert_int(&engine, "B3", 3);
    }
}
