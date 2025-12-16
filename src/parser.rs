use crate::ast::{Expr, Op, Value};
use crate::lexer::Token;
use chumsky::prelude::*;

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

        // Relative Ref
        let rel_ref = just(Token::At)
            .ignore_then(
                select! { Token::Int(n) => n as i32 }
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket)),
            )
            .map(|coords| {
                let dx = coords.get(0).cloned().unwrap_or(0);
                let dy = coords.get(1).cloned().unwrap_or(0);
                Expr::RelRef(dx, dy)
            });

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

        let atom = choice((lambda, rel_ref, val, array, dict, grouping)).boxed();

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

        let assign = select! { Token::Ident(s) => s }
            .then_ignore(just(Token::Eq))
            .then(logical.clone())
            .map(|(id, val)| Expr::Assign(id, Box::new(val)));

        assign.or(logical).boxed()
    })
}
