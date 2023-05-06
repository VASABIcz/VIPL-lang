use std::collections::HashMap;
use std::error::Error;
use std::{env, io};
use std::io::{BufRead, Write};
use std::process::exit;
use vipl::parser::Operation;
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

    vm.handleStatementExpression = |ctx, t| {
        let outNamespaceID = *ctx.vm.namespaceLookup.get("out").unwrap();
        let outNamespace = ctx.vm.namespaces.get(outNamespaceID).unwrap();

        let printInt = *outNamespace.functionsLookup.get("print(int)").unwrap();
        let printBool = *outNamespace.functionsLookup.get("print(bool)").unwrap();
        let printFloat = *outNamespace.functionsLookup.get("print(float)").unwrap();
        let printChar = *outNamespace.functionsLookup.get("print(char)").unwrap();
        let printString = *outNamespace.functionsLookup.get("print(String)").unwrap();
        let printArray = *outNamespace.functionsLookup.get("print(Array)").unwrap();

        match t {
            DataType::Int => {
                ctx.push(LCall { namespace: outNamespaceID, id: printInt })
            }
            DataType::Float => {
                ctx.push(LCall { namespace: outNamespaceID, id: printFloat })
            }
            DataType::Bool => {
                ctx.push(LCall { namespace: outNamespaceID, id: printBool })
            }
            DataType::Char => {
                ctx.push(LCall { namespace: outNamespaceID, id: printChar })
            }
            DataType::Object(_) => {
                ctx.push(Pop)
            }
            DataType::Function { .. } => {
                ctx.push(Pop)
            }
            DataType::Void => {}
        }
    };

    let d = &mut vm as *mut VirtualMachine;

    loop {
        let userInput = readInput();
        println!("locals {:?}", mainLocals);
        let v = match loadSourceFile(userInput, &mut vm) {
            Ok(v) => v,
            Err(e) => {
                handleError(e);
                continue
            }
        };

        let n = Namespace::constructNamespace(v, "UwU".to_string(), &mut vm, mainLocals.clone());
        let generatedNamespace = vm.registerNamespace(n);

        match vm.link() {
            Ok(_) => {}
            Err(e) => {
                handleError(e);
                continue
            }
        }

        let nn = vm.namespaces.get(generatedNamespace).unwrap();
        let f = nn.functions.last().unwrap();
        let fMeta = nn.functionsMeta.last().unwrap();

        if fMeta.localsMeta.len() > localValues.len() {
            for _ in 0..(fMeta.localsMeta.len()-localValues.len()) {
                localValues.push(Value::from(0))
            }
        }

        mainLocals = fMeta.localsMeta.clone().into_vec();

        unsafe {
            f.as_ref().unwrap().call(&mut *d, StackFrame {
                localVariables: &mut localValues,
                objects: None,
                previous: None,
                programCounter: 0,
                namespaceId: nn,
                functionId: nn.functions.len()-1,
            })
        }
    }
}
