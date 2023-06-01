use std::{env, io};
use std::collections::HashMap;
use std::error::Error;
use std::io::{BufRead, Write};
use std::process::exit;

use vipl::ast::{ASTNode, Statement};
use vipl::bytecodeGen::Body;
use vipl::lexingUnits::lexingUnits;
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
    let mut vm = bootStrapVM();
    let mut mainLocals = vec![];
    let mut localValues = vec![];
    let mut lexingUnits = lexingUnits();
    let mut parsingUnits = parsingUnits();

    let n = Namespace::constructNamespace(vec![], "UwU", &mut vm, mainLocals.clone());
    let generatedNamespace = vm.registerNamespace(n);

    unsafe {
        vm.setHandleExpression(|ctx, t| {
            let mut vm = &mut *ctx.vm.get();
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
                    }
                    else {
                        ctx.push(Pop)
                    }
                },
                DataType::Function { .. } => ctx.push(Pop),
                DataType::Void => {}
                DataType::Value => ctx.push(Pop)
            }
        });
    }

    let d = &mut vm as *mut VirtualMachine;

    loop {
        let userInput = readInput();

        let v = match loadSourceFile(userInput, &mut vm, &mut lexingUnits, &mut parsingUnits) {
            Ok(v) => v,
            Err(e) => {
                println!("parse error {}", e);
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
                ASTNode::Expr(e) => buf.push(Statement::StatementExpression(e.clone())),
                _ => {}
            }
        }

        nn.extendFunctionality(v);

        let (fMeta, f) = nn.getFunctionMut(0);

        fMeta.functionType = Runtime(Body::new(buf));

        unsafe {
            match (&mut *d).link() {
                Ok(_) => {}
                Err(e) => {
                    println!("link error {:?}", e);
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
