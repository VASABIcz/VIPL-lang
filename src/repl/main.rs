use std::{env, io};
use std::collections::HashMap;
use std::error::Error;
use std::io::{BufRead, Write};
use std::process::exit;

use vipl::std::std::bootStrapVM;
use vipl::utils::namespacePath;
use vipl::vm::dataType::DataType;
use vipl::vm::namespace::{loadSourceFile, Namespace};
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::value::Value;
use vipl::vm::vm::OpCode::{LCall, Pop, SCall};
use vipl::vm::vm::VirtualMachine;

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
                    namespace: outNamespaceID,
                    id: printInt.1,
                }),
                DataType::Float => ctx.push(LCall {
                    namespace: outNamespaceID,
                    id: printFloat.1,
                }),
                DataType::Bool => ctx.push(LCall {
                    namespace: outNamespaceID,
                    id: printBool.1,
                }),
                DataType::Char => ctx.push(LCall {
                    namespace: outNamespaceID,
                    id: printChar.1,
                }),
                DataType::Object(o) => {
                    if o.name == "String" {
                        ctx.push(LCall {
                            namespace: outNamespaceID,
                            id: printString.1,
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
        // println!("locals {:?}", mainLocals);
        let v = match loadSourceFile(userInput, &mut vm) {
            Ok(v) => v,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };

        let n = Namespace::constructNamespace(v, "UwU", &mut vm, mainLocals.clone());
        let generatedNamespace = vm.registerNamespace(n);

        match vm.link() {
            Ok(_) => {}
            Err(e) => {
                println!("{:?}", e);
                continue;
            }
        }

        let nn = vm.getNamespace(generatedNamespace);
        let fId = nn.getFunctions().len() - 1;
        let (fMeta, f) = nn.getFunctions().last().unwrap();

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
                    namespaceId: nn.id,
                    functionId: fId,
                },
                false
            );
        }
    }
}
