use std::{env, fs};
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
    println!("{:?}", name);

    println!("123");
    let res = loadSourceFile(fs::read_to_string(sourceFile).unwrap(), &mut vm).unwrap();

    println!("xd");
    let n = Namespace::constructNamespace(res, name.join("::"), &mut vm);
    let id = vm.registerNamespace(n);
    vm.link();

    let c = &vm as *const VirtualMachine as *mut VirtualMachine;

    let nn = vm.namespaces.get(id).unwrap();
    let f = nn.functions.last().unwrap();
    let fMeta = nn.functionsMeta.last().unwrap();
    let mut xd = fMeta.localsMeta.iter().map(|it| {it.typ.toDefaultValue()}).collect::<Vec<_>>();
    unsafe {
        f.as_ref().unwrap().call(&mut *c, StackFrame {
            localVariables: &mut xd,
            objects: None,
            previous: None,
            programCounter: 0,
            namespace: nn,
            functionID: nn.functions.len()-1,
        })
    }
    println!("vm: {}", vm.stack.len());
}