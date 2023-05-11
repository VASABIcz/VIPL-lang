use std::collections::HashMap;
use std::ops::Deref;
use crate::asm::asmLib::{AsmGen, AsmValue, AsmLocation, Register, writeToFile};
use crate::asm::asmLib::Register::{R12, R13, Rax, Rdi, Rdx, Rsi, Rsp};

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
            stackSize: 0,
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
    fn mov(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("mov", &dest.toString(), &src.toString())
    }

    fn lea(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("lea", &dest.toString(), &src.toString())
    }

    fn push(&mut self, value: AsmValue) {
        self.stackSize += 1;
        self.writeOp1("push", &value.toString());
    }

    fn pop(&mut self, reg: Register) {
        self.stackSize -= 1;
        self.writeOp1("pop", reg.toStr());
    }

    fn or(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("or", &dest.toString(), &src.toString())
    }

    fn and(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("and", &dest.toString(), &src.toString())
    }

    fn not(&mut self, dest: AsmLocation) {
        self.writeOp1("not", &dest.toString())
    }

    fn add(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("add", &dest.toString(), &src.toString())
    }

    fn sub(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("sub", &dest.toString(), &src.toString())
    }

    fn mul(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("mul", &dest.toString(), &src.toString())
    }

    fn imul(&mut self, dest: AsmLocation, src: AsmValue) {
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

    fn jmp(&mut self, dest: AsmLocation) {
        self.writeOp1("jmp", &dest.toString())
    }

    fn jmpIfEqual(&mut self, dest: AsmLocation) {
        self.writeOp1("je", &dest.toString())
    }

    fn jmpIfNotEqual(&mut self, dest: AsmLocation) {
        self.writeOp1("jne", &dest.toString())
    }

    fn jmpIfGt(&mut self, dest: AsmLocation) {
        self.writeOp1("jg", &dest.toString())
    }

    fn jmpIfLess(&mut self, dest: AsmLocation) {
        self.writeOp1("jl", &dest.toString())
    }

    fn jmpIfOne(&mut self, dest: AsmLocation) {
        self.writeOp1("jne", &dest.toString())
    }

    fn jmpIfZero(&mut self, dest: AsmLocation) {
        self.writeOp1("jz", &dest.toString())
    }

    fn inc(&mut self, reg: Register) {
        self.writeOp1("inc", reg.toStr());
    }

    fn dec(&mut self, reg: Register) {
        self.writeOp1("dec", reg.toStr());
    }

    fn xor(&mut self, dest: AsmLocation, src: AsmValue) {
        self.writeOp2("xor", dest.toString().deref(), dest.toString().deref())
    }

    fn call(&mut self, location: AsmLocation) {
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

    fn idiv(&mut self, dest: AsmLocation) {
        self.writeOp1("div", &dest.toString())
    }

    fn beginIgnore(&mut self) {}

    fn endIgnore(&mut self) {}

    fn getStackOffset(&mut self) -> usize {
        self.stackSize
    }
}

#[test]
fn test() {
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