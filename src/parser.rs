use crate::ast::{CellCoord, CellRange, Effect, Expr, IndexSpec, Op, Value};
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

#[derive(Clone)]
enum BraceEntry {
    Dict(String, Expr),
    Set(Expr),
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

        let open_range = just(Token::Colon)
            .ignore_then(signed_int.clone().or_not())
            .map(|end| IndexSpec::Range(None, end))
            .boxed();

        let index_spec = choice((
            open_range,
            signed_int
                .clone()
                .then(
                    just(Token::Colon)
                        .ignore_then(signed_int.clone().or_not())
                        .or_not(),
                )
                .map(|(start, tail)| match tail {
                    None => IndexSpec::Single(start),
                    Some(end) => IndexSpec::Range(Some(start), end),
                }),
        ))
        .boxed();

        let coord_vec = signed_int
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .boxed();

        let coord_spec = index_spec
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

        // Relative Ref or range: @[dx, dy, ...] or @[-1:1,-1:1]
        let rel_ref = just(Token::At)
            .ignore_then(coord_spec.clone())
            .try_map(|specs, span| {
                if specs.iter().any(|s| matches!(s, IndexSpec::Range(None, _) | IndexSpec::Range(_, None))) {
                    return Err(Rich::custom(span, "Relative ranges require explicit start and end"));
                }
                let all_single = specs.iter().all(|s| matches!(s, IndexSpec::Single(_)));
                if all_single {
                    let coords = specs
                        .iter()
                        .map(|s| match s {
                            IndexSpec::Single(v) => *v,
                            IndexSpec::Range(_, _) => 0,
                        })
                        .collect::<Vec<_>>();
                    Ok(Expr::RelRef(coords))
                } else {
                    Ok(Expr::RelRange(specs))
                }
            })
            .boxed();

        let array = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(Expr::Array)
            .boxed();

        let brace_entry = select! { Token::Ident(k) => k }
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .map(|(k, v)| BraceEntry::Dict(k, v))
            .or(expr.clone().map(BraceEntry::Set))
            .boxed();

        let brace_literal = brace_entry
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .try_map(|entries, span| {
                let mut dict_items = Vec::new();
                let mut set_items = Vec::new();
                let mut has_dict = false;
                let mut has_set = false;
                for entry in entries {
                    match entry {
                        BraceEntry::Dict(k, v) => {
                            has_dict = true;
                            dict_items.push((k, v));
                        }
                        BraceEntry::Set(v) => {
                            has_set = true;
                            set_items.push(v);
                        }
                    }
                }
                if has_dict && has_set {
                    Err(Rich::custom(span, "Cannot mix set and dict literals"))
                } else if has_dict {
                    Ok(Expr::Dict(dict_items))
                } else {
                    Ok(Expr::Set(set_items))
                }
            })
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
            brace_literal,
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
                just(Token::Backslash).to(Op::Remove),
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
                select! { Token::Ident(s) if s == "in" => () }.to(Op::In),
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

        let if_expr = select! { Token::Ident(s) if s == "if" => () }
            .ignore_then(expr.clone())
            .then_ignore(select! { Token::Ident(s) if s == "then" => () })
            .then(expr.clone())
            .then_ignore(select! { Token::Ident(s) if s == "else" => () })
            .then(expr.clone())
            .map(|((cond, then_branch), else_branch)| {
                Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch))
            })
            .boxed();

        let conditional = if_expr.or(logical).boxed();

        let abs_coord_spec = just(Token::Hash)
            .ignore_then(coord_spec.clone())
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
            // Input range assignment: "A1:A5 := 10"
            cell_range.clone()
                .then_ignore(just(Token::ColonEq))
                .then(conditional.clone())
                .map(|(range, val)| Expr::InputRangeAssign(range, Box::new(val))),
            // Input all-cells assignment: "tensor#[*] := expr"
            tensor_all_coords.clone()
                .then_ignore(just(Token::ColonEq))
                .then(conditional.clone())
                .map(|(name, val)| Expr::InputAssignAll(Some(name), Box::new(val))),
            // Input all-cells assignment: "#[*] := expr"
            all_coords.clone()
                .then_ignore(just(Token::ColonEq))
                .then(conditional.clone())
                .map(|(_, val)| Expr::InputAssignAll(None, Box::new(val))),
            // Input relative assignment: "@[dx,dy] := expr"
            rel_coord.clone()
                .then_ignore(just(Token::ColonEq))
                .then(conditional.clone())
                .map(|(coords, val)| Expr::InputAssignRel(coords, Box::new(val))),
            // Input absolute assignment (with ranges): "#[0,0] := 10" or "#[4:6,2] := 1"
            abs_coord_spec.clone()
                .then_ignore(just(Token::ColonEq))
                .then(conditional.clone())
                .map(|(specs, val)| {
                    let all_single = specs.iter().all(|s| matches!(s, IndexSpec::Single(_)));
                    if all_single {
                        let coords = specs
                            .iter()
                            .map(|s| match s {
                                IndexSpec::Single(v) => *v,
                                IndexSpec::Range(_, _) => 0,
                            })
                            .collect::<Vec<_>>();
                        Expr::InputAssignAbs(coords, Box::new(val))
                    } else {
                        Expr::InputAssignAbsRange(specs, Box::new(val))
                    }
                }),
            // Input regular assignment: "A1 := 10"
            select! { Token::Ident(s) => s }
                .then_ignore(just(Token::ColonEq))
                .then(conditional.clone())
                .map(|(id, val)| Expr::InputAssign(id, Box::new(val))),
            // Try range assignment first: "A1:A5 = 10"
            cell_range.clone()
                .then_ignore(just(Token::Eq))
                .then(conditional.clone())
                .map(|(range, val)| Expr::RangeAssign(range, Box::new(val))),
            // All-cells assignment: "tensor#[*] = expr"
            tensor_all_coords.clone()
                .then_ignore(just(Token::Eq))
                .then(conditional.clone())
                .map(|(name, val)| Expr::AssignAll(Some(name), Box::new(val))),
            // All-cells assignment: "#[*] = expr"
            all_coords.clone()
                .then_ignore(just(Token::Eq))
                .then(conditional.clone())
                .map(|(_, val)| Expr::AssignAll(None, Box::new(val))),
            // Relative assignment: "@[dx,dy] = expr"
            rel_coord.clone()
                .then_ignore(just(Token::Eq))
                .then(conditional.clone())
                .map(|(coords, val)| Expr::AssignRel(coords, Box::new(val))),
            // Absolute assignment (with ranges): "#[0,0] = 10" or "#[4:6,2] = 1"
            abs_coord_spec.clone()
                .then_ignore(just(Token::Eq))
                .then(conditional.clone())
                .map(|(specs, val)| {
                    let all_single = specs.iter().all(|s| matches!(s, IndexSpec::Single(_)));
                    if all_single {
                        let coords = specs
                            .iter()
                            .map(|s| match s {
                                IndexSpec::Single(v) => *v,
                                IndexSpec::Range(_, _) => 0,
                            })
                            .collect::<Vec<_>>();
                        Expr::AssignAbs(coords, Box::new(val))
                    } else {
                        Expr::AssignAbsRange(specs, Box::new(val))
                    }
                }),
            // Fallback to regular assignment: "A1 = 10"
            select! { Token::Ident(s) => s }
                .then_ignore(just(Token::Eq))
                .then(conditional.clone())
                .map(|(id, val)| Expr::Assign(id, Box::new(val))),
        ))
        .boxed();

        assign.or(conditional).boxed()
    })
    .boxed()
}
