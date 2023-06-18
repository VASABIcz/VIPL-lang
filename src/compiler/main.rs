#![feature(slice_ptr_get)]

use std::{env, fs};
use std::mem::size_of;
use std::process::exit;
use std::time::Instant;

use vipl::errors::{LoadFileError, VIPLError};
use vipl::lexingUnits::{lexingUnits, TokenType};
use vipl::parsingUnits::parsingUnits;
use vipl::std::std::bootStrapVM;
use vipl::utils::namespacePath;
use vipl::vm::namespace::{loadSourceFile, Namespace};
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::value::Value;
use vipl::vm::vm::{OpCode, VirtualMachine};
use vipl::vm::vm::OpCode::Pop;

fn main() -> Result<(), ()> {
    let mut vm = bootStrapVM();

    let mut lexingUnits = lexingUnits();
    let mut parsingUnits = parsingUnits();

    let sourceFile = env::args().nth(1).expect("expected source field");
    let name = namespacePath(&sourceFile);
    let file = fs::read_to_string(&sourceFile).unwrap();


    let res = match loadSourceFile(&file, &mut vm, &mut lexingUnits, &mut parsingUnits) {
        Ok(v) => v,
        Err(e) => {
            match e {
                LoadFileError::ParserError(a) => a.printUWU(&file, Some(&sourceFile)),
                LoadFileError::LexerError(a) => a.printUWU(&file, Some(&sourceFile))
            }
            return Err(())
        }
    };

    // println!("AST: {:#?}", res);

    let n = Namespace::constructNamespace(res, &name.join("::"), &mut vm, vec![]);
    let id = vm.registerNamespace(n);


    if let Err(ret) = vm.link(|c, t| {
        if !t.isVoid() {
            c.push(Pop)
        }
    }) {
        for e in ret {
            println!();
            e.printUWU(&file, Some(&sourceFile));
        }
        println!();
        return Err(())
    }

    let vm1 = &vm as *const VirtualMachine as *mut VirtualMachine;

    let nn = vm.getNamespace(id);

    let (fMeta, f) = nn.getFunctions().last().unwrap();
    let xd = fMeta
        .localsMeta
        .iter()
        .map(|it| it.typ.toDefaultValue())
        .collect::<Vec<_>>();
    let now = Instant::now();

    let ptr = Box::into_raw(xd.into_boxed_slice());

    unsafe {
        f.as_ref().unwrap().call(&mut *vm1,
            StackFrame {
                localVariables: ptr.as_mut_ptr(),
                programCounter: 0,
                namespaceId: nn.id,
                functionId: nn.getFunctions().len() - 1,
            },
            false,
        );
    }
    let elapsed = now.elapsed();
    unsafe { Box::from_raw(ptr) };
    println!("Elapsed: {:.2?}", elapsed);
    if vm.stackSize() != 0 {
        panic!("something went wrong :(")
    }

    Ok(())
}
