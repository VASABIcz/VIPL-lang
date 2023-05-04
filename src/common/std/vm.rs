use crate::vm::dataType::DataType;
use crate::vm::namespace::Namespace;
use crate::vm::vm::VirtualMachine;

pub fn registerVm(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("vm");

    n.makeNative("gc", &[], |vm, s| {
       vm.gc();
    }, DataType::Void);

    vm.registerNamespace(n);
}