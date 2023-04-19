use crate::ast::{Expression, BinaryOp};
use crate::vm::value::Value;

pub fn evalE(exp: &Expression) -> Option<Expression> {
    return None;
    match exp {
        Expression::BinaryOperation { left, right, op } => {
            let l = evalExpr(left);
            let r = evalExpr(right);

            if let Some(mut a) = l && let Some(b) = r {
                match op {
                    BinaryOp::Add => { todo!()/*a.add(b, &a.toDataType())*/ },
                    BinaryOp::Sub => a.sub(&b, &a.toDataType()),
                    BinaryOp::Mul => a.mul(&b, &a.toDataType()),
                    BinaryOp::Div => a.div(&b, &a.toDataType()),
                    BinaryOp::Gt => a.refGt(&b, &a.toDataType()),
                    BinaryOp::Less => a.refLess(&b, &a.toDataType()),
                    BinaryOp::Eq => a.refEq(&b, &a.toDataType()),
                    BinaryOp::And => a.and(&b),
                    BinaryOp::Or => a.or(&b)
                };
                Some(a.toExpression(&a.toDataType()))
            } else {
                Some(Expression::BinaryOperation {
                    left: Box::new(evalE(left).unwrap_or(*left.clone())),
                    right: Box::new(evalE(right).unwrap_or(*right.clone())),
                    op: op.clone(),
                })
            }
        }
        Expression::IntLiteral(_n) => Some(exp.clone()),
        Expression::FloatLiteral(_f) => Some(exp.clone()),
        Expression::BoolLiteral(_b) => Some(exp.clone()),
        Expression::CharLiteral(_c) => Some(exp.clone()),
        Expression::NotExpression(_) => Some(exp.clone()),
        Expression::LongLiteral(_) => None,
        Expression::DoubleLiteral(_d) => None,
        Expression::StringLiteral(_) => None,
        Expression::Variable(_) => None,
        Expression::ArrayLiteral(_) => None,
        Expression::ArrayIndexing(_) => None,
        Expression::NamespaceAccess(_) => None,
        Expression::Lambda(_, _) => None,
        Expression::Callable(_, _) => None,
        Expression::StructInit(_, _) => None,
        Expression::FieldAccess(_, _) => None
    }
}

pub fn evalExpr(exp: &Expression) -> Option<Value> {
    match exp {
        Expression::BinaryOperation { left, right, op } => {
            let mut l = evalExpr(left)?;
            let r = evalExpr(right)?;

            match op {
                BinaryOp::Add => todo!()/*l.add(r, &l.toDataType())*/,
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
        Expression::NotExpression(_) => Some(false.into()),
        Expression::NamespaceAccess(_) => None,
        Expression::Lambda(_, _) => None,
        Expression::Callable(_, _) => None,
        Expression::StructInit(_, _) => None,
        Expression::FieldAccess(_, _) => None
    }
}
