use libc::link;
use rust_vm::namespace::{loadSourceFile, Namespace};
use rust_vm::std::std::bootStrapVM;
use rust_vm::vm::{StackFrame, VirtualMachine};

fn main() {
    let mut vm = bootStrapVM();
    let sourceFile = std::env::args().nth(1).expect("expected source field");

    println!("123");
    let res = loadSourceFile(&sourceFile, &mut vm).unwrap();

    println!("xd");
    let n = Namespace::constructNamespace(res, sourceFile.strip_suffix(".vipl").unwrap().to_string(), &mut vm);
    let id = vm.registerNamespace(n);
    println!("sus");
    vm.link();

    let c = &vm as *const VirtualMachine as *mut VirtualMachine;

    println!("{:?}", vm.namespaces);
    println!("{}", id);
    let nn = vm.namespaces.get(id).unwrap();
    println!("{:?}", nn.functions);
    let f = nn.functions.last().unwrap();
    let fMeta = nn.functionsMeta.last().unwrap();
    println!("KYS {} {}", fMeta.localsMeta.len(), fMeta.name);
    let mut xd = fMeta.localsMeta.iter().map(|it| {it.typ.toDefaultValue()}).collect::<Vec<_>>();
    unsafe {
        f.call(&mut *c, StackFrame {
            localVariables: &mut xd,
            objects: None,
            previous: None,
            programCounter: 0,
            namespace: nn,
        })
    }
}