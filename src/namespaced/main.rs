use std::{env, fs};
use std::mem::ManuallyDrop;

use libc::link;

use vipl::std::std::bootStrapVM;
use vipl::utils::namespacePath;
use vipl::vm::dataType::DataType;
use vipl::vm::namespace::{loadSourceFile, Namespace};
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::vm::OpCode::{LCall, Pop};
use vipl::vm::vm::VirtualMachine;

fn main() {
    let mut vm = bootStrapVM();

    let sourceFile = env::args().nth(1).expect("expected source field");
    let name = namespacePath(&sourceFile);

    let res = loadSourceFile(fs::read_to_string(sourceFile).unwrap(), &mut vm).unwrap();

    let n = Namespace::constructNamespace(res, &name.join("::"), &mut vm, vec![]);
    let id = vm.registerNamespace(n);
    vm.link().unwrap();

    let c = &vm as *const VirtualMachine as *mut VirtualMachine;


    let nn = vm.namespaces.get(id).unwrap();
    let f = nn.functions.last().unwrap();
    let fMeta = nn.functionsMeta.last().unwrap();
    let mut xd = fMeta.localsMeta.iter().map(|it| {it.typ.toDefaultValue()}).collect::<Vec<_>>();
    unsafe {
        f.as_ref().unwrap().call(&mut *c, StackFrame {
            localVariables: xd.into_boxed_slice(),
            programCounter: 0,
            namespaceId: nn.id,
            functionId: nn.functions.len()-1,
        })
    }
    println!("vm: {}", vm.stack.len());
}