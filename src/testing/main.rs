extern crate rust_vm;

use std::collections::hash_map::Values;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::io::{BufRead, Write};
use std::mem::{ManuallyDrop, size_of};
use std::process::exit;
use std::rc::Rc;
use rust_vm::asm::asmGen::generateAssembly;
use rust_vm::asm::asmLib::NasmGen;

use rust_vm::codegen::{bytecodeGen2, complexBytecodeGen};
use rust_vm::fs::setupFs;
use rust_vm::lexer::tokenizeSource;
use rust_vm::objects::{Str, ViplObject};
use rust_vm::parser::{parse, parseOne, parseTokens, parsingUnits, TokenProvider};
use rust_vm::parser::ParsingUnitSearchType::{Ahead, Back};
use rust_vm::rice::Rice;
use rust_vm::std::bootStrapVM;
use rust_vm::vm::{evaluateBytecode2, run, SeekableOpcodes, StackFrame};

fn readInput() -> String {
    print!(">>> ");
    io::stdout().flush();
    let stdin = io::stdin();
    let mut buf = String::new();
    stdin.lock().read_line(&mut buf);

    if buf == "EXIT\n" {
        exit(0);
    }

    buf
}

fn handleError(err: Box<dyn Error>) {
    eprintln!("ERROR: {err}");
    eprintln!("ERROR: {err:?}");
}

fn main() {
    let sourceFile = std::env::args().nth(1).expect("expected source field");

    let src = std::fs::read_to_string(sourceFile).expect("failed to read source");

    let mut vm = bootStrapVM();
    // let mut localTypes = vec![];
    setupFs(&mut vm);

    let tokens = match tokenizeSource(&src) {
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

    // println!("{:?}", &vm.functions.keys());

    let mut rets = HashMap::new();

    for f in &vm.functions {
        rets.insert(f.0.clone(), f.1.returnType.clone());
    }

    let bs = match bytecodeGen2(ast, &mut rets) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("codegen");
            handleError(e);
            return;
        }
    };

    println!("{:?}", &bs.0);

    let mut nasm = NasmGen::new();
    generateAssembly(&mut nasm, &bs.0);
    println!("{}", nasm.generate());

    vm.addBytecode(bs.0);

    let mut locals = bs.1.iter().map(|it| { it.toDefaultValue() }).collect::<Vec<_>>();

    vm.pushFrame(StackFrame{
        localVariables: &mut locals,
        objects: None,
        previous: None,
        programCounter: 0,
    });
    vm.execute()
}
