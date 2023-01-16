extern crate core;

use std::mem::size_of;
use std::mem;
use crate::serialization::{deserialize, serialize};
use crate::vm::DataType::*;
use crate::vm::{DataType, JmpType, OpCode, SeekableOpcodes, StackFrame, Value};
use crate::vm::OpCode::*;
use crate::vm::Value::Flo;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod serialization;
mod vm;

fn main() {
    println!("{}", mem::size_of::<[OpCode; 1000]>());
    let mut vm = vm::bootStrapVM();
    let ops = [
        PushInt(10000000),
        SetLocal { index: 0, typ: Int },
        PushLocal { index: 0 },
        PushInt(0),
        Less(Int),
        Jmp {
            offset: 1,
            jmpType: JmpType::True,
        },
        Return,
        PushLocal { index: 0 },
        // Pop,
        Call { encoded: "print(int)".to_string() },
        Dec { typ: Int, index: 0 },
        Jmp {
            offset: -9,
            jmpType: JmpType::Jmp,
        },
    ];

    let _cc = [Call {
        encoded: "exec()".to_string(),
    }];

    let _x = vec![vec![
        PushInt(69),
        PushInt(1),
        Add(Int),
        Call {
            encoded: "print(int)".to_string(),
        },
    ]];

    let t= vec![
        PushInt(6),
        PushInt(3),
        Sub(Int),
        Call{ encoded: "print(int)".to_string() }
    ];

    let res = serialize(&ops);
    let xd = deserialize(res);

    let mut seek = SeekableOpcodes {
        index: 0,
        opCodes: &xd,
        start: None,
        end: None,
    };

    let mut stack = StackFrame {
        previous: None,
        localVariables: &mut [Flo(0.)],
        name: None,
    };

    vm.opCodeCache = std::iter::repeat_with(|| None)
        .take(seek.opCodes.len())
        .collect();
    vm::run(&mut seek, &mut vm, &mut stack);
    println!("{:?}", vm.stack.pop());
}
