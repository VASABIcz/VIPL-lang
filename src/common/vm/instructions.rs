use crate::vm::dataType::RawDataType;
use crate::vm::vm::JmpType;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register {
    pub id: usize
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    F2I(Register, Register),
    I2F(Register, Register),
    PushInt(Register, isize),
    PushFunction {
        out: Register,
        namespaceId: u32,
        functionId: u32
    },
    PushFloat(Register, f64),
    PushBool(Register, bool),
    PushChar(Register, char),
    Jmp {
        i: Register,
        offset: i32,
        jmpType: JmpType,
    },
    SCall {
        id: usize,
    },
    LCall {
        namespace: u32,
        id: u32,
    },
    DynamicCall {
        returns: bool,
        argsCount: usize,
        nId: Register,
        fId: Register
    },
    Return(Register),

    Add(RawDataType, Register, Register, Register),
    Sub(RawDataType, Register, Register, Register),
    Div(RawDataType, Register, Register, Register),
    Mul(RawDataType, Register,Register ,Register),
    Modulo(RawDataType, Register, Register, Register),

    Equals(RawDataType, Register, Register, Register),
    Greater(RawDataType, Register, Register, Register),
    Less(RawDataType, Register, Register, Register),

    ShiftLeft(Register, Register, Register),
    ShiftRight(Register, Register, Register),
    BitwiseAnd(Register, Register, Register),
    BitwiseOr(Register, Register, Register),
    BitwiseNot(Register, Register),
    Xor(Register, Register, Register),

    Or(Register, Register, Register),
    And(Register, Register, Register),
    Not(Register, Register),
    New {
        namespaceID: u32,
        structID: u32,
        out: Register
    },
    GetField {
        fieldID: usize,
        out: Register,
        o: Register
    },
    SetField {
        fieldID: usize,
        i: Register,
        o: Register
    },
    ArrayNew(Register),
    ArrayStore(Register, Register, Register),
    ArrayLoad(Register, Register, Register),
    ArrayLength(Register, Register),
    StringLength(Register, Register),
    StrNew(usize, Register),
    GetChar(Register, Register),
    SetGlobal {
        namespaceID: u32,
        globalID: u32,
        i: Register
    },
    GetGlobal {
        namespaceID: u32,
        globalID: u32,
        out: Register
    },
    IsStruct {
        namespaceId: usize,
        structId: usize,
        i: Register,
        out: Register
    }
}

impl Instruction {
    pub fn getOut(&self) -> Option<Register> {
        todo!()
    }

    pub fn getIn(&self) -> &[Register] {
        todo!()
    }
}