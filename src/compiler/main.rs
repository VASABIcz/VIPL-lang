extern crate rust_vm;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use rust_vm::bytecodeChecker::{AbstractStack, checkBytecode};
use rust_vm::codegen::bytecodeGen;
use rust_vm::lexer::tokenizeSource;
use rust_vm::parser::{Operation, parse, parseTokens, parsingUnits};
use rust_vm::parser::ParsingUnitSearchType::{Ahead, Around};
use rust_vm::vm::{bootStrapVM, DataType, evaluateBytecode, OpCode, SeekableOpcodes};

fn handleError(err: Box<dyn Error>) {
    eprintln!("ERROR: {}", err);
    eprintln!("ERROR: {:?}", err);
}

fn main() {
    let sourceFile = std::env::args().nth(1).expect("expected source field");

    let src = std::fs::read_to_string(sourceFile).expect("failed to read source");

    let mut vm = bootStrapVM();
    // let mut localTypes = vec![];
    let mut functionReturns = HashMap::new();

    for f in &vm.functions {
        functionReturns.insert(f.0.clone(), f.1.returnType.clone());
    }

    let mut tokens = match tokenizeSource(&src) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("tokenizer");
            handleError(e);
            return;
        }
    };

    if tokens.is_empty() {
        return;
    }

    // println!("tokens {:?}", &tokens);

    let ast = match parseTokens(tokens) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("parser");
            handleError(e);
            return;
        }
    };

    let bs = match bytecodeGen(ast) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("codegen");
            handleError(e);
            return;
        }
    };

    println!("{:?}", &bs);

    /*
    match checkBytecode(&mut SeekableOpcodes {
        index: 0,
        opCodes: &bs.0,
        start: None,
        end: None,
    }, &mut localTypes, &mut AbstractStack { stack: vec![] }, &mut vm, &mut HashSet::new()) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("bytecode check");
            handleError(e);
            return;
        }
    }

     */

    evaluateBytecode(bs.0, bs.1);
}