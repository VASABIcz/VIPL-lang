use crate::asm::asmLib::{AsmGen, AsmValue, AsmLocation, Register};
use crate::asm::asmLib::Register::Rsi;
use crate::asm::optimizedGen::AsmJumpType::Zero;
use crate::asm::optimizedGen::AsmOpcode::Jmp;
use crate::vm::vm::JmpType;

#[derive(Debug)]
pub enum AsmOpcode {
    Mov(AsmLocation, AsmValue),
    Lea(AsmLocation, AsmValue),
    Push(AsmValue),
    Pop(Register),

    Or(AsmLocation, AsmValue),
    And(AsmLocation, AsmValue),
    Not(AsmLocation),

    Add(AsmLocation, AsmValue),
    Sub(AsmLocation, AsmValue),
    Mul(AsmLocation, AsmValue),
    Imul(AsmLocation, AsmValue),

    SysCall,
    MakeString,
    Ret,

    JmpLabel(String),
    Compare(AsmValue, AsmValue),
    Jmp(AsmLocation, AsmJumpType),
    JmpIfEqual,
    JmpIfNotEqual,
    JmpIfGt,
    JmpIfLess,
    JmpIfOne,
    JmpIfZero,

    Inc,
    Dec,

    Xor(AsmLocation, AsmValue),

    Call(AsmLocation),

    MakeLabel(String),
    NextLabel,

    Comment(String),

    NewLine,

    OffsetStack,

    Setle,
    Setge,

    Setg(Register),
    Setl(Register),
}

impl AsmOpcode {
    pub fn compile<T: AsmGen>(self, gen: &mut T) {
        match self {
            AsmOpcode::Mov(d, s) => gen.mov(d, s),
            AsmOpcode::Lea(d, s) => gen.lea(d, s),
            AsmOpcode::Xor(d, s) => gen.xor(d, s),
            AsmOpcode::Compare(d, s) => gen.compare(d, s),
            AsmOpcode::Setg(r) => gen.setg(r),
            AsmOpcode::Setl(r) => gen.setl(r),
            AsmOpcode::Sub(d, s) => gen.sub(d, s),
            AsmOpcode::Add(d, s) => gen.add(d, s),
            AsmOpcode::Imul(d, s) => gen.imul(d, s),
            AsmOpcode::Call(l) => gen.call(l),
            AsmOpcode::Ret => gen.ret(),
            AsmOpcode::MakeLabel(l) => gen.makeLabel(&l),
            AsmOpcode::Comment(m) => gen.comment(&m),
            AsmOpcode::NewLine => gen.newLine(),
            AsmOpcode::Jmp(loc, jmp) => {
                match jmp {
                    Zero => gen.jmpIfZero(loc),
                    AsmJumpType::Always => gen.jmp(loc),
                    _ => todo!()
                }
            }
            e => todo!("unimplemented opcode {:?}", e)
        }
    }
}

#[derive(Debug)]
pub enum AsmJumpType {
    One,
    Zero,
    True,
    False,
    Gt,
    Less,
    Equal,
    Always
}

pub struct OptimizingGen {
    pub data: Vec<AsmOpcode>
}

impl OptimizingGen {
    pub fn push(&mut self, v: AsmOpcode) {
        self.data.push(v)
    }

    pub fn optimize(self) -> OptimizingGen {
        let mut pushBuf = vec![];
        let mut resBuf = vec![];

        for op in self.data {
            if let AsmOpcode::Push(v) = op {
                if let Some(reg) = v.tryGetRegister() {
                    if reg == Register::Rbx {
                        pushBuf.push((None, true));
                        continue
                    }
                }

                pushBuf.push((Some(v), false))
            }
            else if let AsmOpcode::Pop(reg) = op {
                let v = match pushBuf.pop() {
                    None => {
                        continue
                    }
                    Some(v) => v.0.unwrap()
                };
                if let Some(reg) = v.tryGetRegister() {
                    if reg == Register::Rbx {
                        continue
                    }
                }

                resBuf.push(AsmOpcode::Mov(reg.into(), v))
            }
            else if let AsmOpcode::Add(dest, v) = op {
                let d = dest.clone();
                if let AsmLocation::Register(reg) = dest {
                    if reg == Register::Rsp {
                        if let Some(v) = v.tryGetAmount() {
                            let amount = v/8;
                            for _ in 0..amount {
                                println!("pooping");
                                pushBuf.pop();
                            }
                        }
                    }
                }
                resBuf.push(AsmOpcode::Add(d, v))
            }
            else if let AsmOpcode::Sub(dest, v) = op {
                let d = dest.clone();
                if let AsmLocation::Register(reg) = dest {
                    if reg == Register::Rsp {
                        if let Some(v) = v.tryGetAmount() {
                            let amount = v/8;
                            for _ in 0..amount {
                                println!("pooping");
                                pushBuf.push((None, false));
                            }
                        }
                    }
                }
                resBuf.push(AsmOpcode::Sub(d, v))
            }
            else {
                resBuf.push(op)
            }
        }

        OptimizingGen{ data: resBuf }
    }

    pub fn new() -> Self {
        Self {
            data: vec![],
        }
    }

    pub fn compile<T: AsmGen>(self, gen: &mut T) {
        for op in self.data {
            op.compile(gen)
        }
    }
}

impl AsmGen for OptimizingGen {
    fn mov(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Mov(dest, src))
    }

    fn lea(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Lea(dest, src))
    }

    fn push(&mut self, reg: AsmValue) {
        self.push(AsmOpcode::Push(reg))
    }

    fn pop(&mut self, reg: Register) {
        self.push(AsmOpcode::Pop(reg))
    }

    fn or(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Or(dest, src))
    }

    fn and(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::And(dest, src))
    }

    fn not(&mut self, dest: AsmLocation) {
        self.push(AsmOpcode::Not(dest))
    }

    fn add(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Add(dest, src))
    }

    fn sub(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Sub(dest, src))
    }

    fn mul(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Mul(dest, src))
    }

    fn imul(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Imul(dest, src))
    }

    fn sysCall(&mut self) {
        self.push(AsmOpcode::SysCall)
    }

    fn makeString(&mut self, str: &str) -> String {
        todo!()
    }

    fn ret(&mut self) {
        self.push(AsmOpcode::Ret)
    }

    fn jmpLabel(&mut self) -> String {
        todo!()
        //self.push(AsmOpcode::JmpLabel())
    }

    fn compare(&mut self, a: AsmValue, b: AsmValue) {
        self.push(AsmOpcode::Compare(a, b))
    }

    fn jmp(&mut self, dest: AsmLocation) {
        self.push(AsmOpcode::Jmp(dest, AsmJumpType::Always))
    }

    fn jmpIfEqual(&mut self, dest: AsmLocation) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn jmpIfNotEqual(&mut self, dest: AsmLocation) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn jmpIfGt(&mut self, dest: AsmLocation) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn jmpIfLess(&mut self, dest: AsmLocation) {
        self.push(AsmOpcode::Jmp(dest, AsmJumpType::Less))
    }

    fn jmpIfOne(&mut self, dest: AsmLocation) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn jmpIfZero(&mut self, dest: AsmLocation) {
        self.push(AsmOpcode::Jmp(dest, Zero))
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn inc(&mut self, reg: Register) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn dec(&mut self, reg: Register) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn xor(&mut self, dest: AsmLocation, src: AsmValue) {
        self.push(AsmOpcode::Xor(dest, src))
    }

    fn call(&mut self, location: AsmLocation) {
        self.push(AsmOpcode::Call(location))
    }

    fn makeLabel(&mut self, label: &str) {
        self.push(AsmOpcode::MakeLabel(label.to_string()))
    }

    fn nextLabel(&mut self) -> String {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn comment(&mut self, txt: &str) {
        self.push(AsmOpcode::Comment(txt.to_string()))
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn newLine(&mut self) {
        self.push(AsmOpcode::NewLine)
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn setle(&mut self, reg: Register) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn setge(&mut self, reg: Register) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }

    fn setg(&mut self, reg: Register) {
        self.push(AsmOpcode::Setg(reg))
    }

    fn setl(&mut self, reg: Register) {
        self.push(AsmOpcode::Setl(reg))
    }

    fn idiv(&mut self, dest: AsmLocation) {
        todo!()
        // self.push(AsmOpcode::Add(dest, src))
    }
}