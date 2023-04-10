use crate::ast::{Expression, Op};
use crate::vm::value::Value;

pub fn evalE(exp: &Expression) -> Option<Expression> {
    match exp {
        Expression::ArithmeticOp { left, right, op } => {
            let l = evalExpr(left);
            let r = evalExpr(right);

            if let Some(mut a) = l && let Some(b) = r {
                match op {
                    Op::Add => { todo!()/*a.add(b, &a.toDataType())*/ },
                    Op::Sub => a.sub(&b, &a.toDataType()),
                    Op::Mul => a.mul(&b, &a.toDataType()),
                    Op::Div => a.div(&b, &a.toDataType()),
                    Op::Gt => a.refGt(&b, &a.toDataType()),
                    Op::Less => a.refLess(&b, &a.toDataType()),
                    Op::Eq => a.refEq(&b, &a.toDataType()),
                    Op::And => a.and(&b),
                    Op::Or => a.or(&b)
                };
                Some(a.toExpression(&a.toDataType()))
            } else {
                Some(Expression::ArithmeticOp {
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
        Expression::FunctionCall(_) => None,
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
        Expression::ArithmeticOp { left, right, op } => {
            let mut l = evalExpr(left)?;
            let r = evalExpr(right)?;

            match op {
                Op::Add => todo!()/*l.add(r, &l.toDataType())*/,
                Op::Sub => l.sub(&r, &l.toDataType()),
                Op::Mul => l.mul(&r, &l.toDataType()),
                Op::Div => l.div(&r, &l.toDataType()),
                Op::Gt => l.refGt(&r, &l.toDataType()),
                Op::Less => l.refLess(&r, &l.toDataType()),
                Op::Eq => l.refEq(&r, &l.toDataType()),
                Op::And => l.and(&r),
                Op::Or => l.or(&r),
            };

            Some(l)
        }
        Expression::IntLiteral(n) => Some(n.parse::<isize>().unwrap().into()),
        Expression::LongLiteral(_) => None,
        Expression::FloatLiteral(f) => Some(f.parse::<f64>().unwrap().into()),
        Expression::DoubleLiteral(_d) => None,
        Expression::StringLiteral(_) => None,
        Expression::BoolLiteral(b) => Some((*b).into()),
        Expression::FunctionCall(_) => None,
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
