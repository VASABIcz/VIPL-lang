#![feature(slice_ptr_get)]
#![allow(non_snake_case)]

use std::{env, fs, thread};
use std::time::{Duration, Instant};

use vipl::errors::{LoadFileError, VIPLError};
use vipl::std::bootStrapVM;
use vipl::utils::namespacePath;
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::vm::OpCode::Pop;
use vipl::vm::vm::VirtualMachine;

fn main() -> Result<(), ()> {
    let mut vm = bootStrapVM();

    let mut r = &mut vm as *mut VirtualMachine as usize;

    thread::spawn(move || {
        loop {
            thread::sleep(Duration::from_millis(10));
            unsafe { (*(r as *mut VirtualMachine)).triggerInterrupt(); }
        }
    });

    vm.loadNamespace("draft/core.vipl", &["core".into()]).unwrap();

    let sourceFile = env::args().nth(1).expect("expected source field");
    let name = namespacePath(&sourceFile);
    let file = fs::read_to_string(&sourceFile).unwrap();

    let id = match vm.loadNamespaceFromString(&file, &name) {
        Ok(v) => v,
        Err(e) => {
            match e {
                LoadFileError::ParserError(a) => a.printUWU(&file, Some(&sourceFile)),
                LoadFileError::LexerError(a) => a.printUWU(&file, Some(&sourceFile))
            }
            return Err(());
        }
    };

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
        return Err(());
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
                                 StackFrame::new(
                                     ptr.as_mut_ptr(),
                                     nn.id,
                                     nn.getFunctions().len() - 1,
                                 ),
                                 false,
        );
    }
    let elapsed = now.elapsed();
    unsafe { Box::from_raw(ptr) };
    println!("Elapsed: {:.2?}", elapsed);
    if vm.stackSize() != 0 {
        for (ff, f) in nn.getFunctions() {
            println!("{} bytecode: {:?}", ff.name, f.as_ref().unwrap().getBytecode().unwrap());
        }
        while vm.stackSize() != 0 {
            println!("{:?}", vm.pop());
        }
        panic!("something went wrong :(")
    }

    Ok(())
}
