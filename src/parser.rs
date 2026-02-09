use crate::ast::{CellCoord, CellRange, Effect, Expr, Op, Value};
use crate::command_parse::command_parser_with_expr;
use crate::lexer::Token;
use chumsky::prelude::*;

fn is_cell_reference(s: &str) -> bool {
    let mut chars = s.chars();

    // Must start with uppercase letter
    let _first = match chars.next() {
        Some(c) if c.is_ascii_uppercase() => c,
        _ => return false,
    };

    // Rest must be digits
    let rest: String = chars.collect();
    !rest.is_empty() && rest.chars().all(|c| c.is_ascii_digit())
}

#[derive(Clone)]
enum PostfixOp {
    Call(Vec<Expr>),
    Index(Expr),
    Member(String),
}

pub fn parser<'src, I>() -> Boxed<'src, 'src, I, Expr, extra::Err<Rich<'src, Token>>>
where
    I: Input<'src, Token = Token, Span = SimpleSpan>,
{
    // We box the return type of the closure to ensure 'expr' is a BoxedParser
    recursive(|expr| {
        let val = select! {
            Token::Int(n) => Expr::Literal(Value::Int(n)),
            Token::Str(s) => Expr::Literal(Value::String(s)),
            Token::Ident(s) => Expr::Ref(s),
        }
        .boxed();

        // Parse a signed integer: optional minus followed by an integer
        let signed_int = just(Token::Minus)
            .or_not()
            .then(select! { Token::Int(n) => n })
            .map(|(minus, n)| {
                if minus.is_some() {
                    -(n as i32)
                } else {
                    n as i32
                }
            })
            .boxed();

        // Parse cell coordinate (A1, B2, etc.)
        let cell_coord = select! { Token::Ident(s) => s }
            .try_map(|s, span| {
                if is_cell_reference(&s) {
                    CellCoord::from_str(&s)
                        .ok_or_else(|| Rich::custom(span, "Invalid cell reference"))
                } else {
                    Err(Rich::custom(span, "Not a cell reference"))
                }
            })
            .boxed();

        // Parse cell range (A1:A5 or A1:C3 or A1:A10:2)
        let cell_range = cell_coord
            .clone()
            .then_ignore(just(Token::Colon))
            .then(cell_coord)
            .then(
                just(Token::Colon)
                    .ignore_then(select! { Token::Int(n) => n as i32 })
                    .or_not()
            )
            .map(|((start, end), step)| {
                CellRange { start, end, step }
            })
            .boxed();

        let coord_vec = signed_int
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .boxed();

        // Absolute Ref: #[i, j, k]
        let abs_ref = just(Token::Hash)
            .ignore_then(coord_vec.clone())
            .map(Expr::AbsRef)
            .boxed();

        // Tensor-qualified absolute ref: name#[i, j, k]
        let tensor_abs_ref = select! { Token::Ident(s) => s }
            .then_ignore(just(Token::Hash))
            .then(coord_vec.clone())
            .map(|(name, coords)| Expr::TensorAbsRef(name, coords))
            .boxed();

        // Relative Ref: @[dx, dy, ...]
        let rel_ref = just(Token::At)
            .ignore_then(coord_vec.clone())
            .map(Expr::RelRef)
            .boxed();

        let array = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(Expr::Array)
            .boxed();

        let dict_entry = select! { Token::Ident(k) => k }
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .boxed();

        let dict = dict_entry
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(Expr::Dict)
            .boxed();

        let grouping = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .boxed();

        let lambda_args = select! { Token::Ident(a) => a }
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .boxed();

        let lambda = lambda_args
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(args, body)| Expr::Lambda(args, Box::new(body)))
            .boxed();

        let cell_range_expr = cell_range
            .clone()
            .map(Expr::CellRange)
            .boxed();

        let effect_cmd = select! { Token::Ident(s) if s == "effect" => () }
            .ignore_then(
                command_parser_with_expr(expr.clone())
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
            )
            .map(|cmd| Expr::Effect(Box::new(Effect::Command { origin: None, command: Box::new(cmd) })))
            .boxed();

        let atom = choice((
            lambda,
            tensor_abs_ref,
            abs_ref,
            rel_ref,
            cell_range_expr,
            effect_cmd,
            val,
            array,
            dict,
            grouping,
        ))
        .boxed();

        let call_op = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(PostfixOp::Call)
            .boxed();

        let index_op = expr
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(PostfixOp::Index)
            .boxed();

        let member_op = just(Token::Dot)
            .ignore_then(select! { Token::Ident(i) => i })
            .map(PostfixOp::Member)
            .boxed();

        let postfix = atom.foldl(
            choice((call_op, index_op, member_op)).repeated(),
            |lhs, op| match op {
                PostfixOp::Call(args) => Expr::Call(Box::new(lhs), args),
                PostfixOp::Index(idx) => Expr::Index(Box::new(lhs), Box::new(idx)),
                PostfixOp::Member(id) => Expr::Member(Box::new(lhs), id),
            },
        )
        .boxed();

        let unary = choice((just(Token::Minus).to(Op::Sub), just(Token::Not).to(Op::Not)))
            .repeated()
            .foldr(postfix, |op, rhs| Expr::Unary(op, Box::new(rhs)))
            .boxed();

        let product = unary.clone().foldl(
            choice((
                just(Token::Star).to(Op::Mul),
                just(Token::Slash).to(Op::Div),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        )
        .boxed();

        let sum = product.clone().foldl(
            choice((
                just(Token::Plus).to(Op::Add),
                just(Token::Minus).to(Op::Sub),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        )
        .boxed();

        let range = sum
            .clone()
            .foldl(just(Token::DotDot).then(sum).repeated(), |lhs, (_, rhs)| {
                Expr::Range(Box::new(lhs), Box::new(rhs))
            })
            .boxed();

        let compare = range.clone().foldl(
            choice((
                just(Token::EqEq).to(Op::Eq),
                just(Token::NotEq).to(Op::Neq),
                just(Token::Gt).to(Op::Gt),
                just(Token::Lt).to(Op::Lt),
            ))
            .then(range)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        )
        .boxed();

        let logical = compare.clone().foldl(
            choice((just(Token::And).to(Op::And), just(Token::Or).to(Op::Or)))
                .then(compare)
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        )
        .boxed();

        let abs_coord = just(Token::Hash)
            .ignore_then(coord_vec.clone())
            .boxed();

        let rel_coord = just(Token::At)
            .ignore_then(coord_vec.clone())
            .boxed();

        let all_coords = just(Token::Hash)
            .ignore_then(just(Token::LBracket).ignore_then(just(Token::Star)).then_ignore(just(Token::RBracket)))
            .boxed();

        let tensor_all_coords = select! { Token::Ident(s) => s }
            .then_ignore(just(Token::Hash))
            .then(just(Token::LBracket).ignore_then(just(Token::Star)).then_ignore(just(Token::RBracket)))
            .map(|(name, _)| name)
            .boxed();

        let assign = choice((
            // Try range assignment first: "A1:A5 = 10"
            cell_range
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(range, val)| Expr::RangeAssign(range, Box::new(val))),
            // All-cells assignment: "tensor#[*] = expr"
            tensor_all_coords
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(name, val)| Expr::AssignAll(Some(name), Box::new(val))),
            // All-cells assignment: "#[*] = expr"
            all_coords
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(_, val)| Expr::AssignAll(None, Box::new(val))),
            // Relative assignment: "@[dx,dy] = expr"
            rel_coord
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(coords, val)| Expr::AssignRel(coords, Box::new(val))),
            // Absolute assignment: "#[0,0] = 10"
            abs_coord
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(coords, val)| Expr::AssignAbs(coords, Box::new(val))),
            // Fallback to regular assignment: "A1 = 10"
            select! { Token::Ident(s) => s }
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(id, val)| Expr::Assign(id, Box::new(val))),
        ))
        .boxed();

        assign.or(logical).boxed()
    })
    .boxed()
}
