#[cfg(test)]
mod tests {
    use crate::ast::Value;
    use crate::interpreter::{Coord, Engine};
    use crate::lexer::Token;
    use crate::parser::parser;
    use chumsky::Parser;
    use logos::Logos;

    fn parse_expr(input: &str) -> crate::ast::Expr {
        let token_iter = Token::lexer(input)
            .spanned()
            .map(|(tok, _span)| tok.unwrap_or(Token::Dot));
        let token_stream = chumsky::input::Stream::from_iter(token_iter);
        parser()
            .parse(token_stream)
            .into_result()
            .expect("Parse failed")
    }

    fn get_val(engine: &Engine, cell: &str) -> Value {
        let c2d = Coord::from_str(cell).unwrap();
        let coord = engine.map_view_coord(&c2d).unwrap();
        let tensor = engine.tensors.get(&engine.active).unwrap();
        tensor.state_curr.get(&coord).cloned().unwrap_or(Value::Empty)
    }

    #[test]
    fn test_alloc_and_view_mapping() {
        let mut engine = Engine::new();
        engine.alloc_tensor("cube", vec![4, 4, 4]).unwrap();
        engine.set_view_axes([0, 2]).unwrap();
        engine.set_view_offset(vec![0, 1, 0]).unwrap();

        let a1 = Coord::from_str("A1").unwrap();
        let mapped = engine.map_view_coord(&a1).unwrap();
        assert_eq!(mapped.0, vec![0, 1, 0]);
    }

    #[test]
    fn test_abs_ref_and_immediate_inputs() {
        let mut engine = Engine::new();
        engine.alloc_tensor("t", vec![2, 2, 2]).unwrap();
        engine.set_view_offset(vec![0, 0, 1]).unwrap();

        engine.set_input("A1", Value::Int(7)).unwrap();
        let tensor = engine.tensors.get(&engine.active).unwrap();
        assert_eq!(tensor.state_curr.get(&Coord(vec![0, 0, 1])), Some(&Value::Int(7)));

        engine.set_formula("B1", parse_expr("#[0,0,1] + 5")).unwrap();
        engine.tick();
        assert_eq!(get_val(&engine, "B1"), Value::Int(12));
    }

    #[test]
    fn test_currying_across_ticks() {
        let mut engine = Engine::new();
        engine.set_formula("A1", parse_expr("(x, y) => x + y")).unwrap();
        engine.set_formula("B1", parse_expr("A1(5)")).unwrap();
        engine.set_formula("C1", parse_expr("B1(7)")).unwrap();

        engine.tick();
        engine.tick();
        engine.tick();

        assert_eq!(get_val(&engine, "C1"), Value::Int(12));
    }

    #[test]
    fn test_effect_queue() {
        let mut engine = Engine::new();
        engine.set_formula("A1", parse_expr("tick()" )).unwrap();
        let result = engine.tick();
        assert_eq!(result.enqueued_effects, 1);
        assert_eq!(engine.pending_effects_len(), 1);
    }

    #[test]
    fn test_abs_ref_nd_direct() {
        let mut engine = Engine::new();
        engine.alloc_tensor("t", vec![3, 3, 3]).unwrap();
        engine.set_input_coord(Coord(vec![2, 1, 0]), Value::Int(9));
        engine.recompute_active();

        engine.set_formula("A1", parse_expr("#[2,1,0]" )).unwrap();
        engine.tick();

        assert_eq!(get_val(&engine, "A1"), Value::Int(9));
    }
}
