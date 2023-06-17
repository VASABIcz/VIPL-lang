#![feature(let_chains)]
#![allow(non_snake_case)]
#![deny(unused_assignments)]

use std::{env, io};
use std::collections::HashMap;
use std::error::Error;
use std::io::{BufRead, repeat, stdout, Write};
use std::mem::{size_of, transmute};
use std::process::exit;
use std::thread::sleep;
use std::time::Duration;

use vipl::ast::{ASTNode, RawStatement, Statement};
use vipl::bytecodeGen::{Body, ExpressionCtx, StatementCtx, SymbolicOpcode};
use vipl::errors::{LoadFileError, VIPLError};
use vipl::lexingUnits::{lexingUnits, TokenType};
use vipl::parsingUnits::parsingUnits;
use vipl::std::std::bootStrapVM;
use vipl::termon::{clearScreen, enableRawMode, putChar, putStr, readRaw};
use vipl::utils::namespacePath;
use vipl::vm::dataType::{DataType, Generic};
use vipl::vm::namespace::{loadSourceFile, Namespace, NamespaceState};
use vipl::vm::namespace::FunctionTypeMeta::Runtime;
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::value::Value;
use vipl::vm::vm::{OpCode, VirtualMachine};
use vipl::vm::vm::OpCode::{LCall, Pop, SCall};

fn readInput(history: &[String]) -> Result<String, ()> {
    putStr(">>> ");

    let buf = readRaw(history);

    if buf == "exit" {
        return Err(())
    } else if buf == "clear" {
        clearScreen();

        return Ok(String::new())
    }

    Ok(buf)
}

fn handleExpression(ctx: &mut StatementCtx<SymbolicOpcode>, t: DataType) {
    let vm = unsafe { &mut *ctx.vm.get() };
    let (n, outNamespaceID) = vm.findNamespace("out").unwrap();

    let printInt = n.findFunction("print(int)").unwrap();
    let printBool = n.findFunction("print(bool)").unwrap();
    let printFloat = n.findFunction("print(float)").unwrap();
    let printChar = n.findFunction("print(char)").unwrap();
    let printString = n.findFunction("print(String)").unwrap();
    let printArrayString = n.findFunction("print(Array<String>)").unwrap();
    let printArray = n.findFunction("print(Array<*>)").unwrap();

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
        DataType::Reference(o) => {
            if o.name == "String" {
                ctx.push(LCall {
                    namespace: outNamespaceID as u32,
                    id: printString.1 as u32,
                })
            } else if o.name == "Array" && let Some(Generic::Type(v)) = o.generics.first() {
                if v.isString() {
                    ctx.push(LCall {
                        namespace: outNamespaceID as u32,
                        id: printArrayString.1 as u32,
                    })
                } else {
                    ctx.push(LCall {
                        namespace: outNamespaceID as u32,
                        id: printArray.1 as u32,
                    })
                }
            } else {
                ctx.push(Pop)
            }
        },
        DataType::Function { .. } => ctx.push(Pop),
        DataType::Void => {}
        DataType::Value => ctx.push(Pop),
        DataType::Object => todo!()
    }
}

extern "C" fn onExit(s: usize) {
    println!("UwU");
    exit(1);
}

fn main() -> Result<(), ()> {
    enableRawMode();

    let mut historyBuf = vec![];
    let mut vm = bootStrapVM();
    let mut localValues = vec![];
    let mut lexingUnits = lexingUnits();
    let mut parsingUnits = parsingUnits();

    let n = Namespace::constructNamespace(vec![], "UwU", &mut vm, vec![]);
    let generatedNamespace = vm.registerNamespace(n);

    let d = &mut vm as *mut VirtualMachine;

    loop {
        let userInput = match readInput(&historyBuf) {
            Ok(v) => v,
            Err(_) => {
                return Ok(())
            }
        };

        if userInput.trim().is_empty() {
            continue
        }

        historyBuf.push(userInput.clone());

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

        unsafe {
            // FIXME this is just a quick workaround
            if let Err(e) = (*d).buildSymbolTable(nn.getImportHints()) {
                nn.getImportHintsMut().clear();
                e.printUWU(&userInput, None);
                continue
            }
        }

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
