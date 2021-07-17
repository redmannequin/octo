use std::result;

use crate::ast;
use crate::errors::ParseError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub type Result<T> = result::Result<T, ParseError>;

#[derive(Debug, Clone, Copy)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token<'a>,
    peek_token: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            curr_token,
            peek_token,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Statement<'a>>> {
        let mut program = Vec::new();
        while self.curr_token.token_type != TokenType::EOF {
            program.push(self.parse_stmt()?);
        }
        Ok(program)
    }

    fn parse_stmt(&mut self) -> Result<ast::Statement<'a>> {
        match self.curr_token.token_type {
            TokenType::Let => self.parse_let_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<ast::Statement<'a>> {
        let let_token = self.bump();
        let mutability = self.parse_mutability()?;
        let ident = self.parse_ident()?;
        let _assign_token = self.expected_bump(TokenType::Assign)?;
        let expr = self.parse_expr(ast::Precedence::Lowest)?;
        let _semi_token = self.expected_bump(TokenType::Semicolon)?;
        let let_stmt = ast::LetStmt::new(let_token, mutability, ident, expr);
        Ok(ast::Statement::LetStmt(let_stmt))
    }

    fn parse_expr_stmt(&mut self) -> Result<ast::Statement<'a>> {
        let expr = self.parse_expr(ast::Precedence::Lowest)?;
        let _semi_token = self.expected_bump(TokenType::Semicolon);
        Ok(ast::Statement::ExprStmt(expr))
    }

    fn parse_expr(&mut self, precedence: ast::Precedence) -> Result<ast::Expression<'a>> {
        let mut expr = self.parse_left_expr()?;
        loop {
            let op = match self.curr_token.token_type {
                TokenType::Semicolon => break,
                TokenType::EOF => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "'+', '-', '/', '*', '==', '!=', '<', '>', '>=', '<=', or ';'"
                            .into(),
                        found: TokenType::EOF.to_string(),
                    })
                }
                TokenType::Add => ast::Operator::Add,
                TokenType::Sub => ast::Operator::Sub,
                TokenType::Div => ast::Operator::Div,
                TokenType::Mul => ast::Operator::Mul,
                TokenType::EQ => ast::Operator::EQ,
                TokenType::NEQ => ast::Operator::NEQ,
                TokenType::LT => ast::Operator::LT,
                TokenType::GT => ast::Operator::GT,
                TokenType::GTEQ => ast::Operator::GTEQ,
                TokenType::LTEQ => ast::Operator::LTEQ,
                TokenType::Assign => ast::Operator::Assign,
                _ => unimplemented!(),
            };
            if precedence >= op.into() {
                break;
            }
            expr = match op {
                ast::Operator::Add
                | ast::Operator::Sub
                | ast::Operator::Div
                | ast::Operator::Mul
                | ast::Operator::EQ
                | ast::Operator::LT
                | ast::Operator::GT
                | ast::Operator::NEQ
                | ast::Operator::Assign => self.parse_infix(expr, op)?,
                _ => unimplemented!(),
            };
        }
        Ok(expr)
    }

    fn parse_left_expr(&mut self) -> Result<ast::Expression<'a>> {
        Ok(match self.curr_token.token_type {
            TokenType::Identifier => ast::Expression::Ident(self.parse_ident()?),
            TokenType::If => unimplemented!(),
            TokenType::Bool => ast::Expression::Bool(self.parse_bool()?),
            TokenType::Number => ast::Expression::Number(self.parse_number()?),
            TokenType::Sub | TokenType::Not => ast::Expression::Prefix(self.parse_prefix()),
            token_type => return Err(ParseError::ExpectedExpression(token_type.to_string())),
        })
    }

    fn parse_prefix(&mut self) -> ast::Prefix<'a> {
        unimplemented!()
    }

    fn parse_infix(
        &mut self,
        left_expr: ast::Expression<'a>,
        op: ast::Operator,
    ) -> Result<ast::Expression<'a>> {
        let token = self.bump();
        let right_expr = self.parse_expr(op.into())?;
        Ok(ast::Expression::Infix(ast::Infix::new(
            token, op, left_expr, right_expr,
        )))
    }

    fn parse_ident(&mut self) -> Result<ast::Ident<'a>> {
        let ident_token = self.expected_bump(TokenType::Identifier)?;
        Ok(ast::Ident::new(ident_token))
    }

    fn parse_mutability(&mut self) -> Result<bool> {
        if self.peek_token.token_type == TokenType::Mut {
            self.bump();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn parse_number(&mut self) -> Result<ast::Number<'a>> {
        let num_token = self.expected_bump(TokenType::Number)?;
        let value = num_token.src.parse::<f64>()?;
        Ok(ast::Number::new(num_token, value))
    }

    fn parse_bool(&mut self) -> Result<ast::Bool<'a>> {
        let bool_token = self.expected_bump(TokenType::Bool)?;
        let value = bool_token.src.parse::<bool>()?;
        Ok(ast::Bool::new(bool_token, value))
    }

    fn bump(&mut self) -> Token<'a> {
        std::mem::swap(&mut self.curr_token, &mut self.peek_token);
        std::mem::replace(&mut self.peek_token, self.lexer.next_token())
    }

    fn expected_bump(&mut self, token_type: TokenType) -> Result<Token<'a>> {
        let token = self.bump();
        match token.token_type == token_type {
            true => Ok(token),
            false => Err(ParseError::UnexpectedToken {
                expected: format!("'{}'", token_type),
                found: token.src.into(),
            }),
        }
    }
}
