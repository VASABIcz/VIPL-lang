use std::{env, io};
use std::collections::HashMap;
use std::error::Error;
use std::io::{BufRead, repeat, stdout, Write};
use std::process::exit;
use libc::{c_int, ICANON, putchar, termios};

use vipl::ast::{ASTNode, RawStatement, Statement};
use vipl::bytecodeGen::{Body, ExpressionCtx, StatementCtx};
use vipl::errors::{LoadFileError, VIPLError};
use vipl::lexingUnits::{lexingUnits, TokenType};
use vipl::parsingUnits::parsingUnits;
use vipl::std::std::bootStrapVM;
use vipl::utils::namespacePath;
use vipl::vm::dataType::DataType;
use vipl::vm::namespace::{loadSourceFile, Namespace, NamespaceState};
use vipl::vm::namespace::FunctionTypeMeta::Runtime;
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::value::Value;
use vipl::vm::vm::OpCode::{LCall, Pop, SCall};
use vipl::vm::vm::VirtualMachine;
use vipl::wss::WhySoSlow;

fn enableRawMode() {
    let mut t = termios{
        c_iflag: 0,
        c_oflag: 0,
        c_cflag: 0,
        c_lflag: 0,
        c_line: 0,
        c_cc: [0; 32],
        c_ispeed: 0,
        c_ospeed: 0,
    };


    unsafe { libc::tcgetattr(0, &mut t) };

    // disable ECHO and ICANON mode
    t.c_lflag &= !(libc::ECHO|ICANON);

    unsafe { libc::tcsetattr(0, libc::TCSANOW, &mut t); }
}

pub fn getChar() -> char {
    unsafe {
        libc::getchar() as u8 as char
    }
}

pub fn putChar(c: char) {
    unsafe {
        libc::putchar(c as c_int);
    }
}



pub fn putEscape(code: u8) {
    putChar(0x1B as char);
    putChar(91 as char);
    putChar(code as char);
}

pub fn putEscapeStr(code: &str) {
    putChar(0x1B as char);
    putChar(91 as char);

    for b in code.bytes() {
        putChar(b as char);
    }
}

pub fn getLocation() -> (usize, usize) {
    putEscapeStr("6n");

    let mut buf = vec![];

    while let char = getChar() {
        if char == 'R' {
            break
        }
        buf.push(char as u8);
    }
    buf.remove(0);
    buf.remove(0);
    let str = String::from_utf8_lossy(&buf);

    let v = str.split(';').collect::<Vec<_>>();

    let x = v[0].parse::<usize>().unwrap();
    let y = v[1].parse::<usize>().unwrap();

    (x, y)
}

fn clearFromTo(start: usize, end: usize) {
    for _ in start..end {
        putChar(' ');
    }
}

fn goRightBy(n: usize) {
    for _ in 0..n {
        putEscape('C' as u8);
    }
}

fn goLeftBy(n: usize) {
    for _ in 0..n {
        putChar(8 as char);
    }
}

fn putStr(s: &str) {
    for b in s.bytes() {
        putChar(b as char);
    }
}

fn readRaw(prev: &mut Vec<String>) -> String {
    let mut buf = String::new();
    let mut index = 0usize;
    let (startX, startY) = getLocation();

    loop {
        let c = getChar();

        // enter
        if c == '\n' {
            continue
        }
        // back space
        if c == 127 as char {
            if buf.is_empty() {
                continue
            }
            if index == buf.len() {
                buf.pop();
                putChar(8 as char);
                putChar(' ');
                putChar(8 as char);
            }
            index -= 1;
            continue
        }
        // escape sequence
        if c == 0x1B as char {
            getChar();
            let code = getChar();

            match code as u8 {
                // up
                65 => {
                    goLeftBy(index);
                    clearFromTo(0, buf.len());
                    goLeftBy(buf.len());

                    buf.clear();
                    index = 0;
                }
                // down
                66 => {
                    goLeftBy(index);
                    clearFromTo(0, buf.len());
                    goLeftBy(buf.len());

                    buf.clear();
                    index = 0;

                    putStr("UwU");
                    buf += "UwU";
                    goLeftBy(3);
                }
                // right
                67 => {
                    if index == buf.len() {
                        continue
                    }

                    putEscape('C' as u8);
                    index += 1;
                }
                // left
                68 => {
                    if index == 0 {
                        continue
                    }

                    putChar(8 as char);
                    index -= 1;
                }
                _ => {}
            }
            continue
        }

        if index == buf.len() {
            buf.push(c);
            putChar(c);
            index += 1;
        }
        else {
            let oldIndex = index;

            goLeftBy(index);
            clearFromTo(0, buf.len());
            goLeftBy(buf.len());

            buf.insert(index, c);
            putStr(&buf);

            index = buf.len();

            goLeftBy(index-(oldIndex+1));
            index = index-(index-(oldIndex+1));
        }
    }
}


fn readInput() -> String {
    let mut v = vec![];
    enableRawMode();
    print!("UwU");
    stdout().flush();
    readRaw(&mut v);
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

fn handleExpression(ctx: &mut StatementCtx, t: DataType) {
    let mut vm = unsafe { &mut *ctx.vm.get() };
    let (n, outNamespaceID) = vm.findNamespace("out").unwrap();

    let printInt = n.findFunction("print(int)").unwrap();
    let printBool = n.findFunction("print(bool)").unwrap();
    let printFloat = n.findFunction("print(float)").unwrap();
    let printChar = n.findFunction("print(char)").unwrap();
    let printString = n.findFunction("print(String)").unwrap();
    // let printArray = n.findFunction("print(Array)").unwrap();

    match t {
        DataType::Int => ctx.push(LCall {
            namespace: outNamespaceID as u32,
            id: printInt.1 as u32,
        }),
        DataType::Float => ctx.push(LCall {
            namespace: outNamespaceID as u32,
            id: printFloat.1 as u32,
        }),
        DataType::Bool => ctx.push(LCall {
            namespace: outNamespaceID as u32,
            id: printBool.1 as u32,
        }),
        DataType::Char => ctx.push(LCall {
            namespace: outNamespaceID as u32,
            id: printChar.1 as u32,
        }),
        DataType::Object(o) => {
            if o.name == "String" {
                ctx.push(LCall {
                    namespace: outNamespaceID as u32,
                    id: printString.1 as u32,
                })
            } else {
                ctx.push(Pop)
            }
        },
        DataType::Function { .. } => ctx.push(Pop),
        DataType::Void => {}
        DataType::Value => ctx.push(Pop)
    }
}

fn main() {
    let mut vm = bootStrapVM();
    let mut mainLocals = vec![];
    let mut localValues = vec![];
    let mut lexingUnits = lexingUnits();
    let mut parsingUnits = parsingUnits();

    let n = Namespace::constructNamespace(vec![], "UwU", &mut vm, mainLocals.clone());
    let generatedNamespace = vm.registerNamespace(n);

    let d = &mut vm as *mut VirtualMachine;

    loop {
        let userInput = readInput();

        let v = match loadSourceFile(&userInput, &mut vm, &mut lexingUnits, &mut parsingUnits) {
            Ok(v) => v,
            Err(e) => {
                match e {
                    LoadFileError::ParserError(a) => a.printUWU(&userInput, None),
                    LoadFileError::LexerError(a) => a.printUWU(&userInput, None)
                }
                continue;
            }
        };

        // println!("AST: {:?}", v);

        let nn = vm.getNamespaceMut(generatedNamespace);

        nn.state = NamespaceState::PartiallyLoaded;

        let mut buf = vec![];

        for i in &v {
            match i {
                ASTNode::Statement(s) => buf.push(s.clone()),
                ASTNode::Expr(e) => buf.push(Statement { exp: RawStatement::StatementExpression(e.clone()), loc: vec![] }),
                _ => {}
            }
        }

        nn.extendFunctionality(v);

        let (fMeta, f) = nn.getFunctionMut(0);

        fMeta.functionType = Runtime(Body::new(buf));

        unsafe {
            match (&mut *d).link(handleExpression) {
                Ok(_) => {}
                Err(es) => {
                    for e in es {
                        eprintln!();
                        e.printUWU(&userInput, None);
                    }
                    eprintln!();
                    continue;
                }
            }
        }

        if fMeta.localsMeta.len() > localValues.len() {
            for _ in 0..(fMeta.localsMeta.len() - localValues.len()) {
                localValues.push(Value::from(0))
            }
        }

        mainLocals = fMeta.localsMeta.clone().into_vec();

        unsafe {
            f.as_ref().unwrap().call(
                &mut *d,
                StackFrame {
                    localVariables: localValues.as_mut_ptr(),
                    programCounter: 0,
                    namespaceId: generatedNamespace,
                    functionId: 0,
                },
                false,
            );
        }

        if vm.stackSize() != 0 {
            panic!("something horrible went wrong :( stack size is {} {:?}", vm.stackSize(), vm.pop());
        }
    }
}
