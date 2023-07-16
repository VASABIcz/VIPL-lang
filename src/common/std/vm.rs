use crate::vm::namespace::Namespace;
use crate::vm::vm::VirtualMachine;

pub fn registerVm(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("vm", vm);

    n.makeNativeNoRat(
        "gc",
        &[],
        |vm, s| {
            vm.gc();
        },
        false,
    );

    vm.registerNamespace(n);
}
