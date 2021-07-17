use crate::ast;
use crate::obejct::{Environment, Object};

pub fn eval(program: &[ast::Statement], env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for stmt in program {
        result = match stmt {
            ast::Statement::LetStmt(let_stmt) => eval_let_stmt(let_stmt, env),
            ast::Statement::ExprStmt(expr_stmt) => eval_expr(expr_stmt, env),
        };
    }
    result
}

fn eval_let_stmt(let_stmt: &ast::LetStmt, env: &mut Environment) -> Object {
    let obj = eval_expr(&let_stmt.expr, env);
    env.set(let_stmt.ident.ident, obj);
    Object::Null
}

fn eval_expr(expr_stmt: &ast::Expression, env: &mut Environment) -> Object {
    match expr_stmt {
        ast::Expression::Ident(ident) => env.get(ident.ident).unwrap(),
        ast::Expression::Number(num) => Object::Number(num.value),
        ast::Expression::Bool(boolean) => Object::Bool(boolean.value),
        ast::Expression::Infix(infix) => eval_infix(infix, env),
        _ => unimplemented!(),
    }
}

fn eval_infix(infix_expr: &ast::Infix, env: &mut Environment) -> Object {
    if infix_expr.op == ast::Operator::Assign {
        let ident = match *infix_expr.left {
            ast::Expression::Ident(ident) => ident,
            _ => unimplemented!(),
        };
        let right = eval_expr(&infix_expr.right, env);

        env.set_existing(ident.ident, right);
        Object::Null
    } else {
        let left = eval_expr(&infix_expr.left, env);
        let right = eval_expr(&infix_expr.right, env);
        match (left, right, infix_expr.op) {
            (Object::Number(a), Object::Number(b), ast::Operator::Add) => Object::Number(a + b),
            (Object::Number(a), Object::Number(b), ast::Operator::Sub) => Object::Number(a - b),
            (Object::Number(a), Object::Number(b), ast::Operator::Div) => Object::Number(a / b),
            (Object::Number(a), Object::Number(b), ast::Operator::Mul) => Object::Number(a * b),
            (Object::Number(a), Object::Number(b), ast::Operator::EQ) => {
                Object::Bool((a - b).abs() < f64::EPSILON)
            }
            (Object::Number(a), Object::Number(b), ast::Operator::NEQ) => {
                Object::Bool((a - b).abs() >= f64::EPSILON)
            }
            (Object::Number(a), Object::Number(b), ast::Operator::LT) => Object::Bool(a < b),
            (Object::Number(a), Object::Number(b), ast::Operator::GT) => Object::Bool(a > b),

            _ => unimplemented!(),
        }
    }
}
