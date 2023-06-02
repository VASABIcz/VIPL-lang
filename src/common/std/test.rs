use crate::vm::dataType::DataType;
use crate::vm::namespace::Namespace;
use crate::vm::vm::VirtualMachine;

pub fn registerTest(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("test", vm);

    n.makeNativeNoRat(
        "assert",
        &[DataType::Bool],
        |_, s| {
            let a = s.get(0).asBool();

            assert!(a)
        },
        false,
    );

    vm.registerNamespace(n);
}
