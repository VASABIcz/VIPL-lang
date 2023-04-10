use std::{env, fs};
use libc::link;
use vipl::std::std::bootStrapVM;
use vipl::vm::namespace::{loadSourceFile, Namespace};
use vipl::vm::stackFrame::StackFrame;
use vipl::vm::vm::VirtualMachine;

pub fn namespacePath(path: &str) -> Vec<String> {
    let mut con123 = fs::canonicalize(path).unwrap();
    let mut con = con123.iter();
    let mut cwd123 = env::current_dir().unwrap();
    let mut cwd = cwd123.iter();
    let mut hasResolved = false;
    let mut strBuf = vec![];

    loop {
        let c = con.next();
        let r = cwd.next();
        if c.is_none() {
            break
        }
        if hasResolved || c != r {
            strBuf.push(c.unwrap().to_str().unwrap().to_string());
            hasResolved = true;
        }
    }
    let id = strBuf.len()-1;
    strBuf.get_mut(id).map(|it| {
       *it = it.strip_suffix(".vipl").unwrap().to_string()
    });
    strBuf
}

fn main() {
    let mut vm = bootStrapVM();
    let sourceFile = env::args().nth(1).expect("expected source field");
    let name = namespacePath(&sourceFile);
    println!("{:?}", name);

    println!("123");
    let res = loadSourceFile(&sourceFile, &mut vm).unwrap();

    println!("xd");
    let n = Namespace::constructNamespace(res, name.join("::"), &mut vm);
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
    println!("calling {} {}", fMeta.localsMeta.len(), fMeta.name);
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
}