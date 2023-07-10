use crate::vm::dataType::{DataType, Generic};
use crate::vm::namespace::Namespace;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

pub fn registerStrings(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("strings", vm);

    n.makeNative(
        "concat",
        &[DataType::str(), DataType::str()],
        |vm, s| {
            let a = s.getString(0);
            let b = s.getString(1);
            let mut c = String::with_capacity(a.len() + b.len());

            c += a;
            c += b;

            vm.allocateString(c).into()
        },
        DataType::str(),
        false,
    );

    n.makeNative(
        "split",
        &[DataType::str(), DataType::str()],
        |vm, s| {
            let a = s.getString(0);
            let b = s.getString(1);

            let res = a.split(b).collect::<Vec<_>>();

            let mut b: Vec<Value> = vec![];

            for item in res {
                b.push(vm.allocateString(item.to_string()).into());
            }

            vm.allocateArray(b).into()
        },
        DataType::arr(Generic::Type(DataType::str())),
        false
    );

    n.makeNative(
        "toString",
        &[DataType::Int],
        |vm, s| {
            let i = s.getInt(0);

            vm.allocateString(i.to_string()).into()
        },
        DataType::str(),
        false
    );

    n.makeNative(
        "toString",
        &[DataType::Float],
        |vm, s| {
            let i = s.getFloat(0);

            vm.allocateString(i.to_string()).into()
        },
        DataType::str(),
        false
    );

    n.makeNative(
        "toString",
        &[DataType::Bool],
        |vm, s| {
            let i = s.getBool(0);

            vm.allocateString(i.to_string()).into()
        },
        DataType::str(),
        false
    );

    n.makeNative(
        "toString",
        &[DataType::Char],
        |vm, s| {
            let i = s.getChar(0);

            vm.allocateString(i.to_string()).into()
        },
        DataType::str(),
        false
    );

    n.makeNative(
        "toString",
        &[DataType::Value],
        |vm, s| {
            let i = s.getUInt(0);

            vm.allocateString(i.to_string()).into()
        },
        DataType::str(),
        false
    );

    n.makeNative("endsWith", &[DataType::str(), DataType::str()], |vm, s| {
        let a = s.getString(0);
        let b = s.getString(1);

        a.ends_with(b).into()
    }, DataType::Bool, true);

    vm.registerNamespace(n);
}
