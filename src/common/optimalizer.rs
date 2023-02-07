use crate::ast::{Expression, Op};
use crate::parser::{Operation, parseDataType};
use crate::vm::{DataType, Value};

fn canBeOptimized(expr: &Expression) -> bool {
    match expr {
        Expression::ArithmeticOp { .. } => true,
        Expression::IntLiteral(_) => true,
        Expression::LongLiteral(_) => true,
        Expression::FloatLiteral(_) => true,
        Expression::DoubleLiteral(_) => true,
        Expression::StringLiteral(_) => true,
        Expression::BoolLiteral(_) => true,
        Expression::FunctionCall(_) => false,
        Expression::Variable(_) => false,
        Expression::CharLiteral(_) => true,
        Expression::ArrayLiteral(_) => true,
        Expression::ArrayIndexing(_) => false
    }
}

fn optimizeExpr(exp: &mut Expression) {
    match exp {
        Expression::ArithmeticOp { left, right, op } => {

        }
        Expression::IntLiteral(_) => {}
        Expression::LongLiteral(_) => {}
        Expression::FloatLiteral(_) => {}
        Expression::DoubleLiteral(_) => {}
        Expression::StringLiteral(_) => {}
        Expression::BoolLiteral(_) => {}
        Expression::FunctionCall(_) => {}
        Expression::Variable(_) => {}
        Expression::CharLiteral(_) => {}
        Expression::ArrayLiteral(_) => {}
        Expression::ArrayIndexing(_) => {}
    }
}

pub fn evalExpr(exp: &Expression) -> Option<Value> {
    match exp {
        Expression::ArithmeticOp { left, right, op } => {
            let mut l = evalExpr(left)?;
            let r = evalExpr(right)?;

            match op {
                Op::Add => l.add(&r, &l.toDataType()),
                Op::Sub => l.sub(&r, &l.toDataType()),
                Op::Mul => l.mul(&r, &l.toDataType()),
                Op::Div => l.div(&r, &l.toDataType()),
                Op::Gt => l.refGt(&r, &l.toDataType()),
                Op::Less => l.refLess(&r, &l.toDataType()),
                Op::Eq => l.refEq(&r, &l.toDataType()),
                Op::And => l.and(&r),
                Op::Or => l.or(&r)
            };

            Some(l)
        }
        Expression::IntLiteral(n) => Some(Value::Num(n.parse::<isize>().unwrap())),
        Expression::LongLiteral(_) => None,
        Expression::FloatLiteral(f) => Some(Value::Flo(f.parse::<f32>().unwrap())),
        Expression::DoubleLiteral(d) => None,
        Expression::StringLiteral(_) => None,
        Expression::BoolLiteral(b) => Some(Value::Bol(b.clone())),
        Expression::FunctionCall(_) => None,
        Expression::Variable(_) => None,
        Expression::CharLiteral(c) => Some(Value::Chr(c.clone())),
        Expression::ArrayLiteral(_) => None,
        Expression::ArrayIndexing(_) => None
    }
}

fn optimizeOp(op: &mut Operation) {
    match op {
        Operation::FunctionDef(_) => {}
        Operation::Statement(_) => {}
        Operation::Expression(e) => {
            if canBeOptimized(e) {
                optimizeExpr(e)
            }
        }
    }
}

fn optimizeAst(ast: &mut Vec<Operation>) {
    for item in ast {

    }
}