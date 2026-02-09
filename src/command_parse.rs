use crate::ast::{CellRange, Command, EffectsCommand, Expr};
use crate::lexer::Token;
use chumsky::prelude::*;

pub fn command_parser_with_expr<'src, I, P>(
    expr_parser: P,
) -> Boxed<'src, 'src, I, Command, extra::Err<Rich<'src, Token>>>
where
    I: Input<'src, Token = Token, Span = SimpleSpan>,
    P: Parser<'src, I, Expr, extra::Err<Rich<'src, Token>>> + Clone + 'src,
{
    let expr_parser_set = expr_parser.clone();
    let ident = select! { Token::Ident(s) => s }.boxed();
    let int_list = select! { Token::Int(n) => n as i32 }
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .boxed();

    // Parse cell coordinate (A1, B2, etc.)
    let cell_coord = ident.clone().try_map(|s, span| {
        let mut chars = s.chars();
        let _first = match chars.next() {
            Some(c) if c.is_ascii_uppercase() => c,
            _ => return Err(Rich::custom(span, format!("'{}' is not a cell reference (must start with uppercase letter)", s))),
        };

        let rest: String = chars.collect();
        if rest.is_empty() || !rest.chars().all(|c| c.is_ascii_digit()) {
            return Err(Rich::custom(span, format!("'{}' is not a cell reference (must be like A1, B2, etc.)", s)));
        }

        crate::ast::CellCoord::from_str(&s)
            .ok_or_else(|| Rich::custom(span, "Invalid cell reference"))
    })
    .boxed();

    // Parse cell range (A1:C3 or A1:C3:2)
    let cell_range = cell_coord
        .clone()
        .then_ignore(just(Token::Colon))
        .then(cell_coord)
        .then(
            just(Token::Colon)
                .ignore_then(select! { Token::Int(n) => n as i32 })
                .or_not(),
        )
        .map(|((start, end), step)| CellRange { start, end, step })
        .boxed();

    // tick [range]
    let tick_cmd = ident
        .clone()
        .filter(|s| s == "tick")
        .ignore_then(cell_range.clone().or_not())
        .map(Command::Tick)
        .boxed();

    // alloc <name> [dims]
    let alloc_cmd = ident
        .clone()
        .filter(|s| s == "alloc")
        .ignore_then(ident.clone())
        .then(int_list.clone())
        .map(|(name, shape)| Command::Alloc { name, shape })
        .boxed();

    // use <name>
    let use_cmd = ident
        .clone()
        .filter(|s| s == "use")
        .ignore_then(ident.clone())
        .map(|name| Command::Use { name })
        .boxed();

    // view axes [a,b] offset [o0,...] (order flexible, parts optional)
    let axes_clause = ident
        .clone()
        .filter(|s| s == "axes")
        .ignore_then(int_list.clone())
        .boxed();
    let offset_clause = ident
        .clone()
        .filter(|s| s == "offset")
        .ignore_then(int_list.clone())
        .boxed();
    let view_cmd = ident
        .clone()
        .filter(|s| s == "view")
        .ignore_then(choice((
            axes_clause.clone().then(offset_clause.clone()).map(|(axes, offset)| (Some(axes), Some(offset))),
            offset_clause.clone().then(axes_clause.clone()).map(|(offset, axes)| (Some(axes), Some(offset))),
            axes_clause.clone().map(|axes| (Some(axes), None)),
            offset_clause.clone().map(|offset| (None, Some(offset))),
        )))
        .map(|(axes, offset)| Command::View { axes, offset })
        .boxed();

    // set A1 <expr> (literal only at execution)
    let set_cmd = ident
        .clone()
        .filter(|s| s == "set")
        .ignore_then(ident.clone())
        .then(expr_parser_set)
        .map(|(target, value)| Command::SetInput { target, value })
        .boxed();

    // effects auto N | effects run [N] | effects pending
    let effects_cmd = ident
        .clone()
        .filter(|s| s == "effects")
        .ignore_then(choice((
            ident
                .clone()
                .filter(|s| s == "auto")
                .ignore_then(select! { Token::Int(n) => n as usize })
                .map(EffectsCommand::Auto),
            ident
                .clone()
                .filter(|s| s == "run")
                .ignore_then(select! { Token::Int(n) => n as usize }.or_not())
                .map(EffectsCommand::Run),
            ident
                .clone()
                .filter(|s| s == "pending")
                .to(EffectsCommand::Pending),
        )))
        .map(Command::Effects)
        .boxed();

    // show [range]
    let show_cmd = ident
        .clone()
        .filter(|s| s == "show")
        .ignore_then(cell_range.clone().or_not())
        .map(Command::Show)
        .boxed();

    // bshow [range]
    let bshow_cmd = ident
        .clone()
        .filter(|s| s == "bshow")
        .ignore_then(cell_range.clone().or_not())
        .map(Command::BShow)
        .boxed();

    // help
    let help_cmd = ident
        .clone()
        .filter(|s| s == "help")
        .to(Command::Help)
        .boxed();

    // encode
    let encode_cmd = ident
        .clone()
        .filter(|s| s == "encode")
        .to(Command::Encode)
        .boxed();

    // demo [n]
    let demo_cmd = ident
        .clone()
        .filter(|s| s == "demo")
        .ignore_then(select! { Token::Int(n) => n as u8 }.or_not())
        .map(Command::Demo)
        .boxed();

    // exit
    let exit_cmd = ident
        .clone()
        .filter(|s| s == "exit")
        .to(Command::Exit)
        .boxed();

    choice((
        alloc_cmd,
        use_cmd,
        view_cmd,
        set_cmd,
        effects_cmd,
        tick_cmd,
        show_cmd,
        bshow_cmd,
        demo_cmd,
        help_cmd,
        encode_cmd,
        exit_cmd,
    ))
    .or(expr_parser.map(Command::Eval))
    .boxed()
}
