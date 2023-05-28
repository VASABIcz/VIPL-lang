#![feature(slice_ptr_get)]

use std::{env, fs};
use std::process::exit;
use std::time::Instant;

use vipl::std::std::bootStrapVM;
use vipl::utils::namespacePath;
use vipl::vm::namespace::{loadSourceFile, Namespace};
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::vm::VirtualMachine;

fn main() {
    let mut vm = bootStrapVM();

    let sourceFile = env::args().nth(1).expect("expected source field");
    let name = namespacePath(&sourceFile);

    let res = match loadSourceFile(fs::read_to_string(&sourceFile).unwrap(), &mut vm) {
        Ok(v) => v,
        Err(e) => {
            e.printUWU(&sourceFile);
            exit(0);
        }
    };

    println!("AST: {:?}", res);

    let n = Namespace::constructNamespace(res, &name.join("::"), &mut vm, vec![]);
    let id = vm.registerNamespace(n);


    vm.link().unwrap();

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
        f.as_ref().unwrap().call(
            &mut *vm1,
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
    println!("vm: {}", vm.stackSize());
}
