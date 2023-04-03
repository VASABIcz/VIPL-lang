use rust_vm::namespace::{link, loadSourceFile, Namespace};
use rust_vm::std::bootStrapVM;
use rust_vm::vm::{StackFrame, VirtualMachine};

fn main() {
    let mut vm = bootStrapVM();
    let sourceFile = std::env::args().nth(1).expect("expected source field");

    let res = loadSourceFile(&sourceFile, &mut vm).unwrap();

    let n = Namespace::constructNamespace(res, sourceFile.strip_suffix(".vipl").unwrap().to_string(), &mut vm);
    let id = vm.registerNamespace(n);
    link(&mut vm);

    let c = &vm as *const VirtualMachine as *mut VirtualMachine;

    let nn = vm.namespaces.get(id).unwrap();
    let f = nn.functions.last().unwrap();
    let fMeta = nn.functionsMeta.last().unwrap();
    let mut xd = fMeta.args.iter().map(|it| {it.typ.toDefaultValue()}).collect::<Vec<_>>();
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