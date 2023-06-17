use crate::vm::dataType::DataType;
use crate::vm::namespace::Namespace;
use crate::vm::vm::VirtualMachine;

pub fn registerTest(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("test", vm);

    n.makeNativeNoRat(
        "assert",
        &[DataType::Bool],
        |_, s| {
            let condition = s.get(0).asBool();

            assert!(condition)
        },
        false,
    );

    vm.registerNamespace(n);
}
