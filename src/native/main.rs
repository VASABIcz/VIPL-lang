extern crate rust_vm;

use std::collections::HashMap;
use std::error::Error;
use std::fs;

use rust_vm::cGen::{bytecodeGen2, statementFi};
use rust_vm::fs::setupFs;
use rust_vm::lexer::tokenizeSource;
use rust_vm::parser::parseTokens;
use rust_vm::std::bootStrapVM;
use rust_vm::vm::evaluateBytecode2;

fn handleError(err: Box<dyn Error>) {
    eprintln!("ERROR: {err}");
    eprintln!("ERROR: {err:?}");
}

fn main() {
    let sourceFile = std::env::args().nth(1).expect("expected source field");

    let src = std::fs::read_to_string(sourceFile).expect("failed to read source");
    // let src = "fn a() { print(1) }";
    let mut vm = bootStrapVM();
    // let mut localTypes = vec![];
    setupFs(&mut vm);
    let mut functionReturns = HashMap::new();

    for f in &vm.functions {
        functionReturns.insert(f.0.clone(), f.1.returnType.clone());
    }

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

    // println!("{:?}", ast);

    // println!("{:?}", &vm.functions.keys());

    let mut rets = HashMap::new();

    for f in &vm.functions {
        rets.insert(f.0.clone(), f.1.returnType.clone());
    }

    // println!("{:?}", rets);

    let bs = match bytecodeGen2(statementFi(ast), &mut rets) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("codegen");
            handleError(e);
            return;
        }
    };
    // println!("{:?}", bs);

    fs::write("gen.c", bs);
}
