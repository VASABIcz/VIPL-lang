use crate::vm::dataType::DataType::*;
use crate::vm::vm::OpCode;
use crate::vm::vm::OpCode::*;

pub fn optimizeBytecode(b: Vec<OpCode>) -> Vec<OpCode> {
    b.into_iter()
        .map(|it| match it {
            PushInt(0) => PushIntZero,
            PushInt(1) => PushIntOne,
            GetLocal { index: 0 } => GetLocalZero,
            SetLocal { index: 0 } => SetLocalZero,
            Mul(Int) => MulInt,
            Sub(Int) => SubInt,
            Less(Int) => LessInt,
            v => v,
        })
        .collect::<Vec<_>>()
}
