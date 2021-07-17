//! Contains all AST Node definitions

use crate::token::Token;

/// Statement AST Node
#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    LetStmt(LetStmt<'a>),
    ExprStmt(Expression<'a>),
}

/// Let Statement AST Node
#[derive(Debug, PartialEq, Clone)]
pub struct LetStmt<'a> {
    pub token: Token<'a>,
    pub mutability: bool,
    pub ident: Ident<'a>,
    pub expr: Expression<'a>,
}

impl<'a> LetStmt<'a> {
    pub fn new(token: Token<'a>, mutability: bool, ident: Ident<'a>, expr: Expression<'a>) -> Self {
        LetStmt {
            token,
            mutability,
            ident,
            expr,
        }
    }
}

/// Expression AST Node
#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Ident(Ident<'a>),
    Number(Number<'a>),
    Bool(Bool<'a>),
    Prefix(Prefix<'a>),
    Infix(Infix<'a>),
}

/// Prefix AST Node
#[derive(Debug, PartialEq, Clone)]
pub struct Prefix<'a> {
    pub token: Token<'a>,
    pub op: Operator,
    pub expr: Box<Expression<'a>>,
}

impl<'a> Prefix<'a> {
    pub fn new(token: Token<'a>, op: Operator, expr: Expression<'a>) -> Self {
        Prefix {
            token,
            op,
            expr: Box::new(expr),
        }
    }
}

/// Infix AST Node
#[derive(Debug, PartialEq, Clone)]
pub struct Infix<'a> {
    pub token: Token<'a>,
    pub op: Operator,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

impl<'a> Infix<'a> {
    pub fn new(
        token: Token<'a>,
        op: Operator,
        left: Expression<'a>,
        right: Expression<'a>,
    ) -> Self {
        Infix {
            token,
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

/// Infex Operator
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    EQ,
    NEQ,
    LT,
    GT,
    GTEQ,
    LTEQ,
    Or,
    And,
    Assign,
}

impl AsRef<str> for Operator {
    fn as_ref(&self) -> &str {
        match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Div => "/",
            Operator::Mul => "*",
            Operator::Mod => "%",
            Operator::EQ => "==",
            Operator::NEQ => "!=",
            Operator::LT => "<",
            Operator::GT => ">",
            Operator::GTEQ => ">=",
            Operator::LTEQ => "<=",
            Operator::Or => "||",
            Operator::And => "&&",
            Operator::Assign => "=",
        }
    }
}

/// Operator Precedence
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Lowest = 0,
    Equals = 1,
    LessGrater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

impl From<Operator> for Precedence {
    fn from(op: Operator) -> Precedence {
        match op {
            Operator::EQ | Operator::NEQ | Operator::Assign => Precedence::Equals,
            Operator::LT | Operator::GT => Precedence::LessGrater,
            Operator::Add | Operator::Sub => Precedence::Sum,
            Operator::Div | Operator::Mul => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

/// Identifire AST Node
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Ident<'a> {
    pub token: Token<'a>,
    pub ident: &'a str,
}

impl<'a> Ident<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Ident {
            token,
            ident: token.src,
        }
    }
}

/// Number AST Node
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Number<'a> {
    pub token: Token<'a>,
    pub value: f64,
}

impl<'a> Number<'a> {
    pub fn new(token: Token<'a>, value: f64) -> Self {
        Number { token, value }
    }
}

/// Bool AST Node
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Bool<'a> {
    pub token: Token<'a>,
    pub value: bool,
}

impl<'a> Bool<'a> {
    pub fn new(token: Token<'a>, value: bool) -> Self {
        Bool { token, value }
    }
}
