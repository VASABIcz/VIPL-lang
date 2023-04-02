use std::collections::HashMap;
use crate::asm::asmLib::{AsmGen, AsmValue, Location, Register};
use crate::asm::asmLib::Register::{R10, R12, R13, R14, R15, Rax, Rbx, Rdi, Rdx, Rsi, Rsp};
use crate::value::Value;
use crate::vm::{DataType, JmpType, OpCode};


fn pushLocal<T: AsmGen>(this: &mut T, index: usize) {
    this.comment(&format!("pushLocal {}", index));
    this.mov(R10.into(), AsmValue::Indexing(Rbx.into(), index as isize));
    this.push(R10.into());
    this.newLine();
}

fn pushStr<T: AsmGen>(this: &mut T, s: &str) {
    this.comment(&format!("pushStr {}", s));
    let label = this.makeString(s);
    // 25*8
    this.mov(Rdi.into(), R15.into());
    this.mov(Rsi.into(), R14.into());
    this.mov(Rdx.into(), label.into());
    this.mov(R10.into(), AsmValue::Indexing(R15.into(), 5*16));
    this.call(R10.into());
    this.push(Rax.into());
    this.newLine();
}

fn setLocal<T: AsmGen>(this: &mut T, index: usize) {
    this.comment(&format!("setLocal {}", index));
    this.pop(R10.into());
    this.mov(Location::Indexing(Rbx.into(), index as isize), R10.into());
    this.newLine();
}

fn popStack<T: AsmGen>(this: &mut T) {
    this.comment("popStack");
    this.mov(Rdi.into(), R15.into());
    this.mov(R10.into(), AsmValue::Indexing(R15.into(), 5*8));
    this.call(R10.into());
    this.push(Rax.into());
    this.newLine();
}

fn asmCall<T: AsmGen>(this: &mut T, name: &str) {
    this.comment(&format!("asmCall {}", name));
    let label = this.makeString(name);
    this.mov(Rdi.into(), R15.into());
    this.mov(Rsi.into(), label.into());
    this.mov(Rdx.into(), Rsp.into());
    this.mov(R10.into(), AsmValue::Indexing(R15.into(), 22*8));
    this.call(R10.into());
    this.add(Rsp.into(), Rax.into());
    this.newLine();
}

pub fn generateAssembly<T: AsmGen>(generator: &mut T, opCodes: &Vec<OpCode>) {
    // INIT CODE
    generator.mov(R15.into(), Rdi.into()); // vm ptr
    generator.mov(R14.into(), Rsi.into()); // frame ptr
    generator.mov(Rbx.into(), AsmValue::Indexing(R14.into(), 0)); // locals ptr
    let mut jmpCounter = 0usize;

    let mut makeLabelsGreatAgain = HashMap::new();
    let mut jmpLookup = HashMap::new();

    for (i, op) in opCodes.iter().enumerate() {
        if let OpCode::Jmp { offset, jmpType } = op {
            let o = *offset;
            let xd = (i as isize+o) as usize;
            let label = match makeLabelsGreatAgain.get(&xd) {
                None => {
                    let a = format!("JMP{}", jmpCounter);
                    makeLabelsGreatAgain.insert(xd, a.clone());
                    jmpCounter += 1;
                    a
                }
                Some(v) => {
                    v.clone()
                }
            };
            jmpLookup.insert(i, label);
        }
    }

    for (i, op) in opCodes.iter().enumerate() {
        if let Some(v) = makeLabelsGreatAgain.get(&i) {
            generator.makeLabel(v)
        }
        match op {
            OpCode::F2I => todo!(),
            OpCode::I2F => todo!(),
            OpCode::PushInt(v) => generator.push((*v).into()),
            OpCode::PushIntOne() => generator.push(1.into()),
            OpCode::PushIntZero() => generator.push(0.into()),
            OpCode::PushFloat(_) => todo!(),
            OpCode::PushBool(b) => generator.push((*b as isize).into()),
            OpCode::PushChar(c) => generator.push((*c as isize).into()),
            OpCode::Pop => generator.pop(R12),
            OpCode::Dup => {
                generator.pop(R12);
                generator.push(R12.into());
                generator.push(R12.into());
            }
            OpCode::PushLocal { index } => pushLocal(generator, *index),
            OpCode::SetLocal { index, typ } => setLocal(generator, *index),
            OpCode::Jmp { offset, jmpType } => {
                let l = jmpLookup.get(&i).unwrap().clone();
                match jmpType {
                    JmpType::One => generator.jmpIfZero(l.into()),
                    JmpType::Zero => generator.jmpIfOne(l.into()),
                    JmpType::Jmp => generator.jmp(l.into()),
                    JmpType::Gt => generator.jmpIfGt(l.into()),
                    JmpType::Less => generator.jmpIfLess(l.into()),
                    JmpType::True => generator.jmpIfZero(l.into()),
                    JmpType::False => generator.jmpIfOne(l.into()),
                }
            }
            // FIXME not sure
            OpCode::Call { encoded } => asmCall(generator, encoded.as_str()),
            OpCode::Return => generator.ret(),
            OpCode::Add(t) => match t {
                DataType::Int => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.add(R12.into(), R13.into());
                    generator.push(R12.into());
                }
                _ => todo!()
            }
            OpCode::Sub(t) => match t {
                DataType::Int => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.sub(R12.into(), R13.into());
                    generator.push(R12.into());
                }
                _ => todo!()
            }
            OpCode::Div(_) => todo!(),
            OpCode::Mul(t) => match t {
                DataType::Int => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.mul(R12.into(), R13.into());
                    generator.push(R12.into());
                }
                _ => todo!()
            }
            OpCode::Equals(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.compare(R12.into(), R13.into());
                }
                _ => todo!()
            }
            OpCode::Greater(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.compare(R12.into(), R13.into());
                }
                _ => todo!()
            }
            OpCode::Less(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.compare(R12.into(), R13.into());
                }
                _ => todo!()
            }
            OpCode::Or => {
                generator.pop(R12);
                generator.pop(R13);
                generator.or(R12.into(), R13.into());
                generator.push(R12.into());
            }
            OpCode::And => {
                generator.pop(R12);
                generator.pop(R13);
                generator.and(R12.into(), R13.into());
                generator.push(R12.into());
            }
            OpCode::Not => {
                generator.pop(R12);
                generator.not(R12.into());
                generator.push(R12.into());
            }
            OpCode::New { .. } => todo!(),
            OpCode::ArrayNew(_) => todo!(),
            OpCode::ArrayStore(_) => todo!(),
            OpCode::ArrayLoad(_) => todo!(),
            OpCode::ArrayLength => todo!(),
            OpCode::Inc { .. } => todo!(),
            OpCode::Dec { .. } => todo!(),
            OpCode::StrNew(v) => pushStr(generator, v.as_str()),
            OpCode::GetChar => todo!(),
            _ => {}
        }
    }
}