use crate::vm::value::Value;
use crate::vm::vm::OpCode;

#[derive(Debug, Clone, Copy)]
pub enum ConstValue {
    I(isize),
    F(f64),
    B(bool),
}

impl From<isize> for ConstValue {
    fn from(v: isize) -> ConstValue {
        ConstValue::I(v)
    }
}

impl From<f64> for ConstValue {
    fn from(v: f64) -> ConstValue {
        ConstValue::F(v)
    }
}

impl From<bool> for ConstValue {
    fn from(v: bool) -> ConstValue {
        ConstValue::B(v)
    }
}

impl ConstValue {
    pub fn toFloat(self) -> Self {
        match self {
            ConstValue::I(v) => (v as f64).into(),
            ConstValue::B(v) => (v as isize as f64).into(),
            v => v,
        }
    }

    pub fn toBool(self) -> Self {
        match self {
            ConstValue::I(v) => (v != 0).into(),
            ConstValue::F(v) => (v != 0.0).into(),
            v => v,
        }
    }

    pub fn toInt(self) -> Self {
        match self {
            ConstValue::B(v) => (v as isize).into(),
            ConstValue::F(v) => (v as isize).into(),
            v => v,
        }
    }

    pub fn add(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *v += v1,
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *v += v1,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn div(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *self = ConstValue::F(*v as f64 / v1 as f64),
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *self = ConstValue::F(*v / v1),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn sub(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *v -= v1,
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *v -= v1,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn mul(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *v *= v1,
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *v *= v1,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn eq(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *self = (*v == v1).into(),
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *self = (*v == v1).into(),
                _ => panic!(),
            },
            ConstValue::B(v) => match other {
                ConstValue::B(v1) => *self = (*v == v1).into(),
                _ => panic!(),
            }
            _ => panic!(),
        }
    }

    pub fn gt(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *self = (*v > v1).into(),
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *self = (*v > v1).into(),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn less(&mut self, other: Self) {
        match self {
            ConstValue::I(v) => match other {
                ConstValue::I(v1) => *self = (*v < v1).into(),
                _ => panic!(),
            },
            ConstValue::F(v) => match other {
                ConstValue::F(v1) => *self = (*v < v1).into(),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn logAnd(&mut self, other: Self) {
        match self {
            ConstValue::B(v) => match other {
                ConstValue::B(v1) => *v = v1 && *v,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn logOr(&mut self, other: Self) {
        match self {
            ConstValue::B(v) => match other {
                ConstValue::B(v1) => *v = v1 || *v,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn logNot(&mut self) {
        match self {
            ConstValue::B(v) => *v = !*v,
            _ => panic!(),
        }
    }

    pub fn toValue(self) -> Value {
        match self {
            ConstValue::I(v) => Value::from(v),
            ConstValue::F(v) => Value::from(v),
            ConstValue::B(v) => Value::from(v),
        }
    }

    pub fn toOp(self) -> OpCode {
        match self {
            ConstValue::I(v) => OpCode::PushInt(v),
            ConstValue::F(v) => OpCode::PushFloat(v),
            ConstValue::B(v) => OpCode::PushBool(v),
        }
    }
}

pub fn constEvaluation(ops: Vec<OpCode>) -> Vec<OpCode> {
    let mut constStack: Vec<ConstValue> = vec![];
    let mut finaly = vec![];

    for op in ops {
        match op {
            OpCode::F2I => match constStack.pop() {
                Some(v) => constStack.push(v.toInt()),
                None => finaly.push(op),
            },
            OpCode::I2F => match constStack.pop() {
                Some(v) => constStack.push(v.toFloat()),
                None => finaly.push(op),
            }
            OpCode::PushInt(v) => constStack.push(v.into()),
            OpCode::PushFloat(v) => constStack.push(v.into()),
            OpCode::PushBool(v) => constStack.push(v.into()),
            OpCode::PushChar(v) => constStack.push((v as isize).into()),
            OpCode::Pop => match constStack.pop() {
                None => finaly.push(op),
                Some(_) => {}
            },
            OpCode::Dup if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let a = constStack.pop().unwrap();
                    let b = constStack.pop().unwrap();

                    constStack.push(b);
                    constStack.push(a);
                }
                1 => {
                    let a = constStack.pop().unwrap();
                    finaly.push(a.toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Swap => match constStack.pop() {
                None => finaly.push(op),
                Some(v) => todo!(),
            },
            OpCode::SetLocal { .. } => match constStack.pop() {
                Some(v) => {
                    finaly.push(v.toOp());
                    finaly.push(op);
                }
                None => finaly.push(op),
            },
            OpCode::Add(_) if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let b = constStack.pop().unwrap();
                    let mut a = constStack.pop().unwrap();

                    a.add(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Sub(_) if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let b = constStack.pop().unwrap();
                    let mut a = constStack.pop().unwrap();

                    a.sub(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Div(_) if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let b = constStack.pop().unwrap();
                    let mut a = constStack.pop().unwrap();

                    a.div(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Mul(_) => match constStack.len() {
                2.. => {
                    let b = constStack.pop().unwrap();
                    let mut a = constStack.pop().unwrap();

                    a.mul(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => finaly.push(op),
            },
            OpCode::Equals(_) if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let mut a = constStack.pop().unwrap();
                    let b = constStack.pop().unwrap();

                    a.eq(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Greater(_) if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let mut a = constStack.pop().unwrap();
                    let b = constStack.pop().unwrap();

                    a.gt(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Less(_) if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let mut a = constStack.pop().unwrap();
                    let b = constStack.pop().unwrap();

                    a.less(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Or if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let mut a = constStack.pop().unwrap();
                    let b = constStack.pop().unwrap();

                    a.logOr(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::And if !constStack.is_empty() => match constStack.len() {
                2.. => {
                    let mut a = constStack.pop().unwrap();
                    let b = constStack.pop().unwrap();

                    a.logAnd(b);

                    constStack.push(a);
                }
                1 => {
                    finaly.push(constStack.pop().unwrap().toOp());
                    finaly.push(op);
                }
                _ => unreachable!(),
            },
            OpCode::Not if !constStack.is_empty() => match constStack.pop() {
                Some(mut v) => {
                    v.logNot();

                    constStack.push(v);
                }
                _ => unreachable!(),
            },
            OpCode::Inc { .. } => finaly.push(op),
            OpCode::Dec { .. } => finaly.push(op),
            v => {
                finaly.extend(constStack.iter().map(|it| it.toOp()));
                constStack.clear();

                finaly.push(v);
            }
        }
    }
    finaly.extend(constStack.iter().map(|it| it.toOp()));
    constStack.clear();

    finaly
}
