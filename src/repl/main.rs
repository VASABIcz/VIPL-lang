extern crate rust_vm;

use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::io::BufRead;

use rust_vm::ast;
use rust_vm::codegen::{bytecodeGen, complexBytecodeGen};
use rust_vm::lexer::{Token, tokenizeSource};
use rust_vm::parser::{Operation, parseTokens};
use rust_vm::vm::{bootStrapVM, DataType, evaluateBytecode, OpCode, run, SeekableOpcodes, StackFrame, Value, VirtualMachine};
use rust_vm::vm::RawOpCode::PushInt;

fn readInput() -> String {
    let stdin = io::stdin();
    let mut buf = String::new();
    stdin.lock().read_line(&mut buf);
    return buf;
}

fn handleError(err: Box<dyn Error>) {
    eprintln!("{}", err)
}

fn main() {
    let mut vm = bootStrapVM();
    let mut localTypes = vec![];
    let mut functionReturns = HashMap::new();
    let mut mainLocals = HashMap::new();
    let mut localValues = vec![];
    let mut lastLocalSize: usize = 0;
    let mut opcodeIndex: usize = 0;
    let mut opcodes = vec![];
    let mut bytecode = SeekableOpcodes {
        index: 0,
        opCodes: &[],
        start: None,
        end: None,
    };

    loop {
        vm.stack.clear();
        let str = readInput();

        let tokens = match tokenizeSource(&str) {
            Ok(v) => v,
            Err(e) => {
                handleError(e);
                continue;
            }
        };

        let res = match parseTokens(tokens) {
            Ok(v) => v,
            Err(e) => {
                handleError(e);
                continue;
            }
        };

        let bs = match complexBytecodeGen(res, &mut localTypes, &mut functionReturns, &mut mainLocals) {
            Ok(v) => v,
            Err(e) => {
                handleError(e);
                continue;
            }
        };
        if lastLocalSize < localTypes.len() {
            for x in &localTypes[lastLocalSize..localTypes.len()] {
                localValues.push(x.toDefaultValue())
            }
        }

        println!("{:?}", &bs);

        for _ in 0..bs.len() {
            vm.opCodeCache.push(None)
        }

        opcodes.extend(bs);

        let mut stack = StackFrame {
            previous: None,
            localVariables: &mut localValues,
            name: None,
        };

        let mut opCodes = SeekableOpcodes {
            index: opcodeIndex as isize,
            opCodes: &opcodes,
            start: None,
            end: None,
        };

        run(&mut opCodes, &mut vm, &mut stack);
        opcodeIndex = opCodes.index as usize - 1;
    }
}