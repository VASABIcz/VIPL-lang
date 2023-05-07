use std::arch::x86_64::_rdrand64_step;
use std::collections::HashMap;
use std::fs;
use std::io::Stderr;
use std::ops::Deref;
use std::ptr::write;
use crate::asm::asmLib::Register::{R13, Rax, Rdi, Rdx, Rsi, Rsp};

#[derive(Clone)]
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
    R15
}

impl Into<Location> for Register {
    fn into(self) -> Location {
        return Location::Register(self)
    }
}

impl Into<AsmValue> for Register {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Register(self))
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

pub enum Location {
    Register(Register),
    Indexing(Concrete, isize),
    Identifier(String)
}

impl Location {
    pub fn toString(&self) -> String {
        match self {
            Location::Register(v) => v.toStr().to_string(),
            Location::Indexing(inner, offset) => format!("[{}+{}]", inner.toString(), offset),
            Location::Identifier(v) => v.to_string()
        }
    }
}

#[derive(Clone)]
pub enum Concrete {
    Number(usize),
    Register(Register),
    Lejbl(String)
}

impl Concrete {
    pub fn toString(&self) -> String {
        match self {
            Concrete::Number(v) => v.to_string(),
            Concrete::Register(v) => v.toStr().to_string(),
            Concrete::Lejbl(v) => v.to_string()
        }
    }
}

#[derive(Clone)]
pub enum AsmValue {
    Indexing(Concrete, isize),
    Concrete(Concrete),
    Identifier(String)
}

impl AsmValue {
    pub fn toString(&self) -> String {
        match self {
            AsmValue::Indexing(inner, offset) => format!("[{}+{}]", inner.toString(), offset),
            AsmValue::Concrete(v) => v.toString(),
            AsmValue::Identifier(v) => v.to_string()
        }
    }
}

impl Into<Concrete> for i32 {
    fn into(self) -> Concrete {
        return Concrete::Number(self as usize)
    }
}

impl Into<Concrete> for Register {
    fn into(self) -> Concrete {
        return Concrete::Register(self)
    }
}

impl Into<AsmValue> for i32 {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Number(self as usize))
    }
}

impl Into<AsmValue> for isize {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Number(self as usize))
    }
}

impl Into<AsmValue> for usize {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(Concrete::Number(self as usize))
    }
}

impl Into<AsmValue> for Concrete {
    fn into(self) -> AsmValue {
        return AsmValue::Concrete(self)
    }
}

impl Into<Location> for &str {
    fn into(self) -> Location {
        return Location::Identifier(String::from(self))
    }
}

impl Into<Location> for String {
    fn into(self) -> Location {
        return Location::Identifier(self)
    }
}

impl Into<AsmValue> for String {
    fn into(self) -> AsmValue {
        return AsmValue::Identifier(self)
    }
}

pub trait AsmGen {
    fn mov(&mut self, dest: Location, src: AsmValue);
    fn lea(&mut self, dest: Location, src: AsmValue);
    fn push(&mut self, reg: AsmValue);
    fn pop(&mut self, reg: Register);

    fn or(&mut self, dest: Location, src: AsmValue);
    fn and(&mut self, dest: Location, src: AsmValue);
    fn not(&mut self, dest: Location);

    fn add(&mut self, dest: Location, src: AsmValue);
    fn sub(&mut self, dest: Location, src: AsmValue);
    fn mul(&mut self, dest: Location, src: AsmValue);
    fn imul(&mut self, dest: Location, src: AsmValue);
    fn idiv(&mut self, dest: Location);

    fn sysCall(&mut self);
    fn makeString(&mut self, str: &str) -> String;
    fn ret(&mut self);

    fn jmpLabel(&mut self) -> String;
    fn compare(&mut self, a: AsmValue, b: AsmValue);
    fn jmp(&mut self, dest: Location);
    fn jmpIfEqual(&mut self, dest: Location);
    fn jmpIfNotEqual(&mut self, dest: Location);
    fn jmpIfGt(&mut self, dest: Location);
    fn jmpIfLess(&mut self, dest: Location);
    fn jmpIfOne(&mut self, dest: Location);
    fn jmpIfZero(&mut self, dest: Location);

    fn inc(&mut self, reg: Register);
    fn dec(&mut self, reg: Register);

    fn xor(&mut self, dest: Location, src: AsmValue);

    fn call(&mut self, location: Location);

    fn makeLabel(&mut self, label: &str);
    fn nextLabel(&mut self) -> String;

    fn comment(&mut self, txt: &str);

    fn newLine(&mut self);

    fn offsetStack(&mut self, offset: isize);

    fn setle(&mut self, reg: Register);
    fn setge(&mut self, reg: Register);

    fn setg(&mut self, reg: Register);
    fn setl(&mut self, reg: Register);
}

pub enum AsmOpcode {
    Mov(Location, AsmValue),
    Lea(Location, AsmValue),
    Push(AsmValue),
    Pop(Register),

    Or,
    And,
    Not,

    Add,
    Sub,
    Mul,
    Imul,

    SysCall,
    MakeString,
    Ret,

    JmpLabel,
    Compare,
    Jmp,
    JmpIfEqual,
    JmpIfNotEqual,
    JmpIfGt,
    JmpIfLess,
    JmpIfOne,
    JmpIfZero,

    Inc,
    Dec,

    Xor,

    Call,

    MakeLabel,
    NextLabel,

    Comment,

    NewLine,

    OffsetStack,

    Setle,
    Setge,

    Setg,
    Setl
}

pub struct OptimizingGen {
    pub data: Vec<AsmOpcode>
}

impl OptimizingGen {
    pub fn push(&mut self, v: AsmOpcode) {
        self.data.push(v)
    }
}

impl AsmGen for OptimizingGen {
    fn mov(&mut self, dest: Location, src: AsmValue) {
        self.push(AsmOpcode::Mov(dest, src))
    }

    fn lea(&mut self, dest: Location, src: AsmValue) {
        self.push(AsmOpcode::Lea(dest, src))
    }

    fn push(&mut self, reg: AsmValue) {
        self.push(AsmOpcode::Push(reg))
    }

    fn pop(&mut self, reg: Register) {
        self.push(AsmOpcode::Pop(reg))
    }

    fn or(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn and(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn not(&mut self, dest: Location) {
        todo!()
    }

    fn add(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn sub(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn mul(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn imul(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn sysCall(&mut self) {
        todo!()
    }

    fn makeString(&mut self, str: &str) -> String {
        todo!()
    }

    fn ret(&mut self) {
        todo!()
    }

    fn jmpLabel(&mut self) -> String {
        todo!()
    }

    fn compare(&mut self, a: AsmValue, b: AsmValue) {
        todo!()
    }

    fn jmp(&mut self, dest: Location) {
        todo!()
    }

    fn jmpIfEqual(&mut self, dest: Location) {
        todo!()
    }

    fn jmpIfNotEqual(&mut self, dest: Location) {
        todo!()
    }

    fn jmpIfGt(&mut self, dest: Location) {
        todo!()
    }

    fn jmpIfLess(&mut self, dest: Location) {
        todo!()
    }

    fn jmpIfOne(&mut self, dest: Location) {
        todo!()
    }

    fn jmpIfZero(&mut self, dest: Location) {
        todo!()
    }

    fn inc(&mut self, reg: Register) {
        todo!()
    }

    fn dec(&mut self, reg: Register) {
        todo!()
    }

    fn xor(&mut self, dest: Location, src: AsmValue) {
        todo!()
    }

    fn call(&mut self, location: Location) {
        todo!()
    }

    fn makeLabel(&mut self, label: &str) {
        todo!()
    }

    fn nextLabel(&mut self) -> String {
        todo!()
    }

    fn comment(&mut self, txt: &str) {
        todo!()
    }

    fn newLine(&mut self) {
        todo!()
    }

    fn offsetStack(&mut self, offset: isize) {
        todo!()
    }

    fn setle(&mut self, reg: Register) {
        todo!()
    }

    fn setge(&mut self, reg: Register) {
        todo!()
    }

    fn setg(&mut self, reg: Register) {
        todo!()
    }

    fn setl(&mut self, reg: Register) {
        todo!()
    }

    fn idiv(&mut self, dest: Location) {
        todo!()
    }
}

pub struct NasmGen {
    pub executable: String,
    pub readOnly: String,
    pub stringCounter: usize,
    pub jmpCounter: usize,
    pub labelCounter: usize,
    pub stringCache: HashMap<String, String>,
    pub stackSize: usize
}

impl NasmGen {
    pub fn new() -> Self {
        Self {
            executable: String::new(),
            readOnly: String::new(),
            stringCounter: 0,
            jmpCounter: 0,
            labelCounter: 0,
            stringCache: Default::default(),
            stackSize: 10,
        }
    }

    pub fn createEscapedString(s: &str) -> String {
        let mut buf = String::with_capacity(s.len());
        let mut isInString = false;

        for c in s.bytes() {
            let chr = c as char;
            if chr.is_ascii_alphanumeric() || chr.is_ascii_graphic() || chr.is_ascii_hexdigit() || chr == ' ' {
                if !isInString {
                    if !buf.is_empty() {
                        buf.push(',');
                    }
                    buf.push('\"');
                    isInString = true;
                }
                buf.push(c as char);
            }
            else {
                if isInString {
                    buf.push('\"');
                    isInString = false;
                }
                if !buf.is_empty() {
                    buf.push(',');
                }
                buf += &c.to_string();
            }
        }
        if isInString {
            buf += "\"";
        }
        if buf.is_empty() {
            buf += "0";
        }
        else {
            buf += ", 0";
        }
        buf
    }

    pub fn generate(&self) -> String {
        let mut buf = String::with_capacity(self.readOnly.len()+self.executable.len());
        buf.push_str("BITS 64\n");
        buf.push_str("DEFAULT REL\n\n");
        buf.push_str("section .text\n");
        buf.push_str(&self.executable);
        buf.push_str("\nsection .rodata\n");
        buf.push_str(&self.readOnly);

        buf
    }

    pub fn writeLine(&mut self, data: &str) {
        self.executable.push_str(data);
        self.executable.push('\n');
    }

    pub fn writeComment(&mut self, data: &str) {
        self.executable.push('\n');
        self.executable.push_str("; ");
        self.executable.push_str(data);
        self.executable.push('\n');
    }

    pub fn writeOp2(&mut self, op: &str, arg1: &str, arg2: &str) {
        self.executable.push_str(op);
        self.executable.push(' ');
        self.executable.push_str(arg1);
        self.executable.push_str(", ");
        self.executable.push_str(arg2);
        self.executable.push('\n');
    }

    pub fn writeOp1(&mut self, op: &str, arg1: &str) {
        self.executable.push_str(op);
        self.executable.push(' ');
        self.executable.push_str(arg1);
        self.executable.push('\n');
    }
}

impl AsmGen for NasmGen {
    fn mov(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("mov", &dest.toString(), &src.toString())
    }

    fn lea(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("lea", &dest.toString(), &src.toString())
    }

    fn push(&mut self, reg: AsmValue) {
        self.stackSize += 1;
        self.writeOp1("push", &reg.toString());
    }

    fn pop(&mut self, reg: Register) {
        self.stackSize -= 1;
        self.writeOp1("pop", reg.toStr());
    }

    fn or(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("or", &dest.toString(), &src.toString())
    }

    fn and(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("and", &dest.toString(), &src.toString())
    }

    fn not(&mut self, dest: Location) {
        self.writeOp1("not", &dest.toString())
    }

    fn add(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("add", &dest.toString(), &src.toString())
    }

    fn sub(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("sub", &dest.toString(), &src.toString())
    }

    fn mul(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("mul", &dest.toString(), &src.toString())
    }

    fn imul(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("imul", &dest.toString(), &src.toString())
    }

    fn sysCall(&mut self) {
        self.writeLine("syscall")
    }

    fn makeString(&mut self, str: &str) -> String {
        match self.stringCache.get(str) {
            None => {
                // FIXME sanitaze str
                let id = format!("str{}", self.stringCounter);
                self.stringCounter += 1;

                self.readOnly.push_str(&id.clone());
                self.readOnly.push_str(": db ");
                self.readOnly += &NasmGen::createEscapedString(str);
                self.readOnly.push('\n');
                self.stringCache.insert(str.to_string(), id.clone());
                id
            }
            Some(v) => v.clone()
        }
    }

    fn ret(&mut self) {
        self.writeLine("ret");
    }

    fn jmpLabel(&mut self) -> String {
        let label = format!("jmp{}", self.jmpCounter);
        self.executable += &label;
        self.executable += ":\n";
        self.jmpCounter += 1;
        label
    }

    fn compare(&mut self, a: AsmValue, b: AsmValue) {
        self.writeOp2("cmp", &a.toString(), &b.toString());
    }

    fn jmp(&mut self, dest: Location) {
        self.writeOp1("jmp", &dest.toString())
    }

    fn jmpIfEqual(&mut self, dest: Location) {
        self.writeOp1("je", &dest.toString())
    }

    fn jmpIfNotEqual(&mut self, dest: Location) {
        self.writeOp1("jne", &dest.toString())
    }

    fn jmpIfGt(&mut self, dest: Location) {
        self.writeOp1("jg", &dest.toString())
    }

    fn jmpIfLess(&mut self, dest: Location) {
        self.writeOp1("jl", &dest.toString())
    }

    fn jmpIfOne(&mut self, dest: Location) {
        self.writeOp1("jne", &dest.toString())
    }

    fn jmpIfZero(&mut self, dest: Location) {
        self.writeOp1("jz", &dest.toString())
    }

    fn inc(&mut self, reg: Register) {
        self.writeOp1("inc", reg.toStr());
    }

    fn dec(&mut self, reg: Register) {
        self.writeOp1("dec", reg.toStr());
    }

    fn xor(&mut self, dest: Location, src: AsmValue) {
        self.writeOp2("xor", dest.toString().deref(), dest.toString().deref())
    }

    fn call(&mut self, location: Location) {
        println!("size: {}", self.stackSize);
        if self.stackSize % 2 == 0 {
            self.sub(Rsp.into(), 8.into());
        }
        self.writeOp1("call", &location.toString());
        if self.stackSize % 2 == 0 {
            self.add(Rsp.into(), 8.into());
        }
    }

    fn makeLabel(&mut self, label: &str) {
        self.executable += label;
        self.executable += ":\n";
    }

    fn nextLabel(&mut self) -> String {
        let s = format!("label{}", self.labelCounter);
        self.labelCounter += 1;
        s
    }

    fn comment(&mut self, txt: &str) {
        self.writeComment(txt);
    }

    fn newLine(&mut self) {
        self.executable += "\n";
    }

    fn offsetStack(&mut self, offset: isize) {
        if offset < 0 {
            self.stackSize += (offset*-1) as usize;
            self.sub(Rsp.into(), (8*offset*-1).into())
        }
        else if offset > 0 {
            self.stackSize -= offset as usize;
            self.add(Rsp.into(), (8*offset).into());
        }
    }

    fn setle(&mut self, reg: Register) {
        self.writeOp1("setle", "al");
        self.mov(reg.into(), Rax.into());
    }

    fn setge(&mut self, reg: Register) {
        self.writeOp1("setge", "al");
        self.mov(reg.into(), Rax.into());
    }

    fn setg(&mut self, reg: Register) {
        self.writeOp1("setg", "al");
        self.mov(reg.into(), Rax.into());
    }

    fn setl(&mut self, reg: Register) {
        self.writeOp1("setl", "al");
        self.mov(reg.into(), Rax.into());
    }

    fn idiv(&mut self, dest: Location) {
        self.writeOp1("div", &dest.toString())
    }
}

fn main() {
    let mut n = NasmGen::new();
    let msg = "Hello World!\nUwU\n";
    let str = n.makeString(msg);

    n.xor(R13.into(), R13.into());
    n.mov(R13.into(), 0.into());
    let loopLabel = n.jmpLabel();
    n.mov(Rax.into(), 1.into());
    n.mov(Rdi.into(), 1.into());
    n.lea(Rsi.into(), str.into());
    n.mov(Rdx.into(), msg.len().into());
    n.sysCall();
    n.inc(R13);
    n.compare(R13.into(), 10.into());
    n.jmpIfNotEqual(loopLabel.into());

    n.mov(Rax.into(), 60.into());
    n.mov(Rdi.into(), 420.into());
    n.sysCall();

    let g = n.generate();
    println!("{}", &g);
    writeToFile("sus.asm", &g);
}

pub fn writeToFile(file: &str, s: &str) {
    fs::write(file, s).unwrap();
}