extern crate rust_vm;

use std::collections::hash_map::Values;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::io::{BufRead, Write};
use std::mem::{ManuallyDrop, size_of};
use std::process::exit;
use std::rc::Rc;

use rust_vm::codegen::complexBytecodeGen;
use rust_vm::fs::setupFs;
use rust_vm::lexer::tokenizeSource;
use rust_vm::objects::{Str, ViplObject};
use rust_vm::parser::{parse, parseOne, parsingUnits, TokenProvider};
use rust_vm::parser::ParsingUnitSearchType::{Ahead, Back};
use rust_vm::rice::Rice;
use rust_vm::std::bootStrapVM;
use rust_vm::vm::{run, SeekableOpcodes, StackFrame, Value};

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
    println!("{}", size_of::<Value>());
    /*
    println!("{}", size_of::<usize>());
    println!("{}", size_of::<Value>());
    // println!("{}", size_of::<ValueC>());
    let x = 111;
    let y = 111;
    println!("{:#?}", (&x as *const i32));
    println!("{:#?}", (&y as *const i32));
    println!("{}", size_of::<Option<Rice<ViplObject>>>());
    {
        for _ in 0..1000 {
            let xVal = ValueC{Num: 69};
        }
        let cVal = ValueC{Reference: ManuallyDrop::new(Rice::new(ViplObject::Str(Str::new("UwU".to_string()))))};
        let cVal1 = ValueC{Num: 69};
    }
    {
        println!("value should be droped already");
    }

     */
}
