use logos::Logos;
use std::fmt;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Skip whitespace
#[logos(skip r"//[^\n]*")] // Skip line comments
#[logos(skip r"/\*([^*]|\*[^/])*\*/")] // Skip block comments
pub enum Token {
    // Arithmetic
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    // Logic
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,

    // Structural
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Eq,
    #[token("=>")]
    Arrow,
    #[token(".")]
    Dot,
    #[token("#")]
    Hash,
    #[token("@")]
    At, // For relative refs
    #[token("..")]
    DotDot, // For ranges

    // Data
    #[regex("[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),

    // Identifiers (Cells A1, Vars x, func names)
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // String literals "hello"
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| lex.slice().trim_matches('"').to_string())]
    Str(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
