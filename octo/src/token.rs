//! All things tokens  
use std::fmt;

/// Represent a single unit of source
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Token<'a> {
    pub src: &'a str,
    pub token_type: TokenType,
    pub span: Span,
}

impl<'a> Token<'a> {
    /// Returns a [Token] with its respective attributes
    ///
    /// # Arguments
    ///
    /// * `src` - A string slice of the source associated to the token
    /// * `token_type` - The [TokenType] of the token value
    /// * `span` - The [Span] of where the token source lies within the source
    pub fn new(src: &'a str, token_type: TokenType, span: Span) -> Self {
        Token {
            src,
            token_type,
            span,
        }
    }
}

/// The type of tokens available
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenType {
    EOF,
    Illegal,

    Let,    // let
    If,     // if
    Else,   // else
    Return, // return
    Mut,    // mut

    Func,   // fn
    Enum,   // enum
    Struct, // struct
    Impl,   // impl

    Add, // +
    Sub, // -
    Div, // /
    Mul, // *
    Mod, // %

    LT,   // <
    GT,   // >
    EQ,   // ==
    NEQ,  // !=
    LTEQ, // <=
    GTEQ, // >=

    Not,    // !
    Or,     // ||
    And,    // &
    AndAnd, // &&

    Dot,       // .
    Assign,    // =
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    LParam,   // (
    RParam,   // )
    LBracket, // [
    RBracket, // ]
    LBrace,   // {
    RBrace,   // }

    Identifier, // any word that isn't a keyword
    Number,     // any number
    Bool,       // ture | false
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::EOF => write!(f, "EOF"),
            TokenType::Illegal => write!(f, "Illegal"),
            TokenType::Let => write!(f, "let"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::Return => write!(f, "return"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::Func => write!(f, "fn"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::Struct => write!(f, "struct"),
            TokenType::Impl => write!(f, "impl"),
            TokenType::Add => write!(f, "+"),
            TokenType::Sub => write!(f, "-"),
            TokenType::Div => write!(f, "/"),
            TokenType::Mul => write!(f, "*"),
            TokenType::Mod => write!(f, "%"),
            TokenType::LT => write!(f, "<"),
            TokenType::GT => write!(f, ">"),
            TokenType::EQ => write!(f, "=="),
            TokenType::NEQ => write!(f, "!="),
            TokenType::LTEQ => write!(f, "<="),
            TokenType::GTEQ => write!(f, ">="),
            TokenType::Not => write!(f, "!"),
            TokenType::Or => write!(f, "||"),
            TokenType::And => write!(f, "&"),
            TokenType::AndAnd => write!(f, "&&"),
            TokenType::Dot => write!(f, "."),
            TokenType::Assign => write!(f, "="),
            TokenType::Comma => write!(f, ","),
            TokenType::Colon => write!(f, ":"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::LParam => write!(f, "("),
            TokenType::RParam => write!(f, ")"),
            TokenType::LBracket => write!(f, "["),
            TokenType::RBracket => write!(f, "]"),
            TokenType::LBrace => write!(f, "{{"),
            TokenType::RBrace => write!(f, "}}"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::Number => write!(f, "Number"),
            TokenType::Bool => write!(f, "Bool"),
        }
    }
}

/// Represents the start and end positions of a string slice
///
/// This will be useful for debugging purposes as it allows visability
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Returns a new [Span]
    ///
    /// # Arguments
    ///
    /// * `start` - a usize representing the start of the span
    /// * `end - a usize representing the end of the span
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}
