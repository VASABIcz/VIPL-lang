extern crate core;

use std::mem::size_of;
use std::mem;
use crate::ast::Op;
use crate::lexer::{lexingUnits, SourceProvider, tokenize, TokenType};
use crate::parser::{ArithmeticParsingUnit, BoolParsingUnit, CallParsingUnit, FunctionParsingUnit, IfParsingUnit, NumericParsingUnit, parse, ParsingUnit, StatementVarCreateParsingUnit, TokenProvider, VariableParsingUnit, WhileParsingUnit};
use crate::parser::ParsingUnitSearchType::Ahead;
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
mod tests;

/*
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
*/

fn main() {
    let lexingUnits = lexingUnits();
    let input = "0 = lol fn main() { -420.69 = x print(69*x) while x == 1 { print(69) } if true { test(1) } else { kys(1) }}";

    let src = SourceProvider {
        data: input,
        index: 0,
    };

    let parsers: Vec<Box<dyn ParsingUnit>> = vec![
        Box::new(WhileParsingUnit),
        Box::new(FunctionParsingUnit),
        Box::new(StatementVarCreateParsingUnit),
        Box::new(NumericParsingUnit),
        Box::new(CallParsingUnit),
        Box::new(ArithmeticParsingUnit {
            op: Op::Mul,
            typ: TokenType::Mul,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Div,
            typ: TokenType::Div,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Add,
            typ: TokenType::Plus,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Sub,
            typ: TokenType::Minus,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Eq,
            typ: TokenType::Eq,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Less,
            typ: TokenType::Less,
        }),
        Box::new(ArithmeticParsingUnit {
            op: Op::Gt,
            typ: TokenType::Gt,
        }),
        Box::new(VariableParsingUnit),
        Box::new(IfParsingUnit),
        Box::new(BoolParsingUnit),
    ];
    let tokens = tokenize(&mut lexingUnits.into_boxed_slice(), src);
    println!("tokens {:?}", &tokens);
    let res = parse(
        &mut TokenProvider { tokens, index: 0 },
        Ahead,
        &parsers.into_boxed_slice(),
    );
    println!("{:?}", &res)
}