use crate::ast::{CellCoord, CellRange, Expr, Op, Value};
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

pub fn parser<'src, I>() -> impl Parser<'src, I, Expr, extra::Err<Rich<'src, Token>>>
where
    I: Input<'src, Token = Token, Span = SimpleSpan>,
{
    // We box the return type of the closure to ensure 'expr' is a BoxedParser
    recursive(|expr| {
        let val = select! {
            Token::Int(n) => Expr::Literal(Value::Int(n)),
            Token::Str(s) => Expr::Literal(Value::String(s)),
            Token::Ident(s) => Expr::Ref(s),
        };

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
            });

        // Parse cell coordinate (A1, B2, etc.)
        let cell_coord = select! { Token::Ident(s) => s }
            .try_map(|s, span| {
                if is_cell_reference(&s) {
                    CellCoord::from_str(&s)
                        .ok_or_else(|| Rich::custom(span, "Invalid cell reference"))
                } else {
                    Err(Rich::custom(span, "Not a cell reference"))
                }
            });

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
            });

        let coord_vec = signed_int
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket));

        // Absolute Ref: #[i, j, k]
        let abs_ref = just(Token::Hash)
            .ignore_then(coord_vec.clone())
            .map(Expr::AbsRef);

        // Relative Ref: @[dx, dy, ...]
        let rel_ref = just(Token::At)
            .ignore_then(coord_vec.clone())
            .map(Expr::RelRef);

        let array = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(Expr::Array);

        let dict_entry = select! { Token::Ident(k) => k }
            .then_ignore(just(Token::Colon))
            .then(expr.clone());

        let dict = dict_entry
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(Expr::Dict);

        let grouping = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let lambda_args = select! { Token::Ident(a) => a }
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen));

        let lambda = lambda_args
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(args, body)| Expr::Lambda(args, Box::new(body)));

        let atom = choice((lambda, abs_ref, rel_ref, val, array, dict, grouping)).boxed();

        let call_op = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(PostfixOp::Call);

        let index_op = expr
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(PostfixOp::Index);

        let member_op = just(Token::Dot)
            .ignore_then(select! { Token::Ident(i) => i })
            .map(PostfixOp::Member);

        let postfix = atom.foldl(
            choice((call_op, index_op, member_op)).repeated(),
            |lhs, op| match op {
                PostfixOp::Call(args) => Expr::Call(Box::new(lhs), args),
                PostfixOp::Index(idx) => Expr::Index(Box::new(lhs), Box::new(idx)),
                PostfixOp::Member(id) => Expr::Member(Box::new(lhs), id),
            },
        );

        let unary = choice((just(Token::Minus).to(Op::Sub), just(Token::Not).to(Op::Not)))
            .repeated()
            .foldr(postfix, |op, rhs| Expr::Unary(op, Box::new(rhs)));

        let product = unary.clone().foldl(
            choice((
                just(Token::Star).to(Op::Mul),
                just(Token::Slash).to(Op::Div),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        );

        let sum = product.clone().foldl(
            choice((
                just(Token::Plus).to(Op::Add),
                just(Token::Minus).to(Op::Sub),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        );

        let range = sum
            .clone()
            .foldl(just(Token::DotDot).then(sum).repeated(), |lhs, (_, rhs)| {
                Expr::Range(Box::new(lhs), Box::new(rhs))
            });

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
        );

        let logical = compare.clone().foldl(
            choice((just(Token::And).to(Op::And), just(Token::Or).to(Op::Or)))
                .then(compare)
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary(Box::new(lhs), op, Box::new(rhs)),
        );

        let abs_coord = just(Token::Hash)
            .ignore_then(coord_vec.clone());

        let assign = choice((
            // Try range assignment first: "A1:A5 = 10"
            cell_range
                .then_ignore(just(Token::Eq))
                .then(logical.clone())
                .map(|(range, val)| Expr::RangeAssign(range, Box::new(val))),
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
        ));

        assign.or(logical).boxed()
    })
}
