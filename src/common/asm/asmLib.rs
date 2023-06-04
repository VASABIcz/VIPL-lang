use std::arch::x86_64::_rdrand64_step;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::fs;
use std::io::Stderr;
use std::ops::Deref;
use std::ptr::write;

use crate::asm::asmLib::Register::{Rax, Rdi, Rdx, Rsi, Rsp, R13};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rex,
    Rsi,
    Rdi,

    Rsp,
    Rbp,

    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Into<AsmLocation> for Register {
    fn into(self) -> AsmLocation {
        return AsmLocation::Register(self);
    }
}

impl Into<AsmValue> for Register {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Register(self));
    }
}

impl Register {
    pub fn toStr(&self) -> &'static str {
        match self {
            Register::Rax => "rax",
            Register::Rbx => "rbx",
            Register::Rcx => "rcx",
            Register::Rdx => "rdx",
            Register::Rex => "rex",
            Register::Rsi => "rsi",
            Register::Rdi => "rdi",
            Register::Rsp => "rsp",
            Register::Rbp => "rbp",
            Register::R8 => "r8",
            Register::R9 => "r9",
            Register::R10 => "r10",
            Register::R11 => "r11",
            Register::R12 => "r12",
            Register::R13 => "r13",
            Register::R14 => "r14",
            Register::R15 => "r15",
        }
    }
}

#[derive(Debug, Clone)]
pub enum AsmLocation {
    Register(Register),
    Indexing(Concrete, isize),
    Identifier(String),
}

#[derive(Clone, Debug)]
pub enum AsmValue {
    Concrete(Concrete),
    Indexing(Concrete, isize),
    Identifier(String),
}

impl From<u32> for AsmValue {
    fn from(value: u32) -> Self {
        AsmValue::Concrete(Concrete::Number(value as usize))
    }
}

impl AsmValue {
    pub fn tryGetAmount(&self) -> Option<usize> {
        match self {
            AsmValue::Concrete(Concrete::Number(v)) => Some(*v),
            _ => None,
        }
    }

    pub fn tryGetRegister(&self) -> Option<Register> {
        match self {
            AsmValue::Concrete(c) => match c {
                Concrete::Register(v) => Some(v.clone()),
                _ => None,
            },
            _ => None,
        }
    }
}

impl AsmLocation {
    pub fn toString(&self) -> String {
        match self {
            AsmLocation::Register(v) => v.toStr().to_string(),
            AsmLocation::Indexing(inner, offset) => format!("[{}+{}]", inner.toString(), offset),
            AsmLocation::Identifier(v) => v.to_string(),
        }
    }
}

#[derive(Clone)]
pub enum Concrete {
    Number(usize),
    Register(Register),
    Lejbl(String),
}

impl Debug for Concrete {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Concrete::Number(v) => write!(f, "{}", v),
            Concrete::Register(v) => write!(f, "{:?}", v),
            Concrete::Lejbl(v) => write!(f, "{:?}", v),
        }
    }
}

impl Concrete {
    pub fn toString(&self) -> String {
        match self {
            Concrete::Number(v) => v.to_string(),
            Concrete::Register(v) => v.toStr().to_string(),
            Concrete::Lejbl(v) => v.to_string(),
        }
    }
}

impl AsmValue {
    pub fn toString(&self) -> String {
        match self {
            AsmValue::Indexing(inner, offset) => format!("[{}+{}]", inner.toString(), offset),
            AsmValue::Concrete(v) => v.toString(),
            AsmValue::Identifier(v) => v.to_string(),
        }
    }
}

impl Into<Concrete> for i32 {
    fn into(self) -> Concrete {
        return Concrete::Number(self as usize);
    }
}

impl Into<Concrete> for Register {
    fn into(self) -> Concrete {
        return Concrete::Register(self);
    }
}

impl Into<AsmValue> for i32 {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Number(self as usize));
    }
}

impl Into<AsmValue> for isize {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Number(self as usize));
    }
}


impl From<usize> for AsmValue {
    fn from(value: usize) -> Self {
        AsmValue::Concrete(Concrete::Number(value as usize))
    }
}

impl Into<AsmValue> for Concrete {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(self);
    }
}

impl Into<AsmLocation> for &str {
    fn into(self) -> AsmLocation {
        return AsmLocation::Identifier(String::from(self));
    }
}

impl Into<AsmLocation> for String {
    fn into(self) -> AsmLocation {
        return AsmLocation::Identifier(self);
    }
}

impl Into<AsmValue> for String {
    fn into(self) -> AsmValue {
        return AsmValue::Identifier(self);
    }
}

pub trait AsmGen {
    fn mov(&mut self, dest: AsmLocation, src: AsmValue);
    fn lea(&mut self, dest: AsmLocation, src: AsmValue);
    fn push(&mut self, reg: AsmValue);
    fn pop(&mut self, reg: Register);

    fn or(&mut self, dest: AsmLocation, src: AsmValue);
    fn and(&mut self, dest: AsmLocation, src: AsmValue);
    fn not(&mut self, dest: AsmLocation);

    fn add(&mut self, dest: AsmLocation, src: AsmValue);
    fn sub(&mut self, dest: AsmLocation, src: AsmValue);
    fn mul(&mut self, dest: AsmLocation, src: AsmValue);
    fn imul(&mut self, dest: AsmLocation, src: AsmValue);
    fn idiv(&mut self, dest: AsmLocation);

    fn sysCall(&mut self);
    fn makeString(&mut self, str: &str) -> String;
    fn ret(&mut self);

    fn jmpLabel(&mut self) -> String;
    fn compare(&mut self, a: AsmValue, b: AsmValue);
    fn jmp(&mut self, dest: AsmLocation);
    fn jmpIfEqual(&mut self, dest: AsmLocation);
    fn jmpIfNotEqual(&mut self, dest: AsmLocation);
    fn jmpIfGt(&mut self, dest: AsmLocation);
    fn jmpIfLess(&mut self, dest: AsmLocation);
    fn jmpIfOne(&mut self, dest: AsmLocation);
    fn jmpIfZero(&mut self, dest: AsmLocation);

    fn inc(&mut self, reg: Register);
    fn dec(&mut self, reg: Register);

    fn xor(&mut self, dest: AsmLocation, src: AsmValue);

    fn call(&mut self, location: AsmLocation);

    fn makeLabel(&mut self, label: &str);
    fn nextLabel(&mut self) -> String;

    fn comment(&mut self, txt: &str);

    fn newLine(&mut self);

    fn offsetStack(&mut self, offset: isize) {
        if offset < 0 {
            self.sub(Rsp.into(), (8 * offset * -1).into())
        } else if offset > 0 {
            self.add(Rsp.into(), (8 * offset).into());
        }
    }

    fn setle(&mut self, reg: Register);
    fn setge(&mut self, reg: Register);

    fn setg(&mut self, reg: Register);
    fn setl(&mut self, reg: Register);
    fn sete(&mut self, reg: Register);

    fn beginIgnore(&mut self);
    fn endIgnore(&mut self);

    fn getStackOffset(&mut self) -> usize;
}

pub fn writeToFile(file: &str, s: &str) {
    fs::write(file, s).unwrap();
}
