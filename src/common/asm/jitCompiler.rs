use crate::asm::asmExec::allocateBinFunction;
use crate::asm::asmGen::Jitter;
use crate::asm::nasmGen::NasmGen;
use crate::asm::optimizedGen::OptimizingGen;
use crate::vm::namespace::Namespace;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::{OpCode, VirtualMachine};
use std::env::temp_dir;
use std::fs;
use std::mem::transmute;
use std::process::Command;
use std::time::{Instant, SystemTime, UNIX_EPOCH};
use crate::bytecodeGen::SymbolicOpcode;

pub fn compileAssembly(asm: &str) -> Box<[u8]> {
    let dir = temp_dir();

    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    let name = format!("asm-temp-{}", since_the_epoch.as_micros());

    let p = dir.join(format!("{}.asm", name));
    fs::write(p, asm).unwrap();

    let cmd = format!(
        "nasm -f bin {}",
        dir.join(format!("{}.asm", name)).to_str().unwrap()
    );

    let e: Vec<&str> = cmd.split(' ').collect();

    let res = Command::new(e.first().unwrap())
        .args(&e[1..e.len()])
        .output()
        .unwrap();

    println!("{}", cmd);
    println!("ERR: {}", String::from_utf8_lossy(&res.stderr));
    println!("{}", String::from_utf8_lossy(&res.stdout));

    let r = fs::read(dir.join(name)).unwrap();

    r.into_boxed_slice()
}

#[derive(Debug, Default)]
pub struct JITCompiler {}

impl JITCompiler {
    pub fn compile(
        &self,
        ops: &[SymbolicOpcode],
        vm: &VirtualMachine,
        namespace: &Namespace,
        returns: bool,
    ) -> extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> Value {
        let nasmGen = NasmGen::new();
        let mut j = Jitter::new(nasmGen);

        j.generateAssembly(ops, vm, namespace, returns);

        let genAsm = j.gen.generate();

        let mut bin = compileAssembly(&genAsm);

        let ptr = allocateBinFunction(&mut bin);

        ptr
    }
}
