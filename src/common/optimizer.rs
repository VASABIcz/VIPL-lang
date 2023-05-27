use crate::ast::{BinaryOp, Expression};
use crate::vm::value::Value;

pub fn evalE(exp: &Expression) -> Option<Expression> {
    return None;
}

pub fn evalExpr(exp: &Expression) -> Option<Value> {
    match exp {
        Expression::BinaryOperation { left, right, op } => {
            let mut l = evalExpr(left)?;
            let r = evalExpr(right)?;

            match op {
                BinaryOp::Add => todo!(), /*l.add(r, &l.toDataType())*/
                BinaryOp::Sub => l.sub(&r, &l.toDataType()),
                BinaryOp::Mul => l.mul(&r, &l.toDataType()),
                BinaryOp::Div => l.div(&r, &l.toDataType()),
                BinaryOp::Gt => l.refGt(&r, &l.toDataType()),
                BinaryOp::Less => l.refLess(&r, &l.toDataType()),
                BinaryOp::Eq => l.refEq(&r, &l.toDataType()),
                BinaryOp::And => l.and(&r),
                BinaryOp::Or => l.or(&r),
            };

            Some(l)
        }
        Expression::IntLiteral(n) => Some(n.parse::<isize>().unwrap().into()),
        Expression::LongLiteral(_) => None,
        Expression::FloatLiteral(f) => Some(f.parse::<f64>().unwrap().into()),
        Expression::DoubleLiteral(_d) => None,
        Expression::StringLiteral(_) => None,
        Expression::BoolLiteral(b) => Some((*b).into()),
        Expression::Variable(_) => None,
        Expression::CharLiteral(c) => Some((*c).into()),
        Expression::ArrayLiteral(_) => None,
        Expression::ArrayIndexing(_) => None,
        Expression::NotExpression(_, _) => Some(false.into()),
        Expression::NamespaceAccess(_) => None,
        Expression::Lambda(_, _, _) => None,
        Expression::Callable(_, _) => None,
        Expression::StructInit(_, _) => None,
        Expression::FieldAccess(_, _) => None,
        Expression::Null => None,
        Expression::TernaryOperator(_, _, _) => None,
    }
}
