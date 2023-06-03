use crate::vm::dataType::{DataType, Generic};
use crate::vm::namespace::Namespace;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

pub fn registerUtils(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("utils", vm);

    n.makeNative(
        "makeArray",
        &[DataType::Int],
        |vm, s| {
            let size = s.get(0).asNum() as usize;

            let ar = vec![Value::null(); size];

            vm.allocateArray(ar).into()
        },
        DataType::arr(Generic::Type(DataType::Value)),
        false,
    );

    vm.registerNamespace(n);
}
