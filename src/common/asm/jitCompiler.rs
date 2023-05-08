use std::env::temp_dir;
use std::fs;
use std::mem::transmute;
use std::process::Command;
use std::time::{Instant, SystemTime, UNIX_EPOCH};
use crate::asm::asmExec::allocateBinFunction;
use crate::asm::asmGen::generateAssembly;
use crate::asm::asmLib::NasmGen;
use crate::vm::namespace::Namespace;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::{OpCode, VirtualMachine};

pub fn compileAssembly(asm: &str) -> Box<[u8]> {
    let dir = temp_dir();

    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    let name = format!("asm-temp-{}", since_the_epoch.as_micros());

    let p = dir.join(format!("{}.asm", name));
    fs::write(p, asm).unwrap();

    let cmd = format!("nasm -f bin {}", dir.join(format!("{}.asm", name)).to_str().unwrap());

    let e: Vec<&str> = cmd.split(' ').collect();

    let res = Command::new(e.first().unwrap())
        .args(&e[1..e.len()])
        .output().unwrap();

    println!("{}", cmd);
    println!("ERR: {}", String::from_utf8_lossy(&res.stderr));
    println!("{}", String::from_utf8_lossy(&res.stdout));

    let r = fs::read(dir.join(name)).unwrap();

    r.into_boxed_slice()
}

#[derive(Debug)]
pub struct JITCompiler {}

impl JITCompiler {
    pub fn compile(&self, ops: Vec<OpCode>, vm: &VirtualMachine, namespace: &Namespace) -> extern fn(&mut VirtualMachine, &mut StackFrame) -> Value {
        let mut nasm = NasmGen::new();
        generateAssembly(&mut nasm, ops, vm, namespace);

        let genAsm = nasm.generate();
        println!("genAsm");

        let mut bin = compileAssembly(&genAsm);

        let ptr = allocateBinFunction(&mut bin);

        ptr
    }
}