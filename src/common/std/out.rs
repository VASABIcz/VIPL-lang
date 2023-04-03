use crate::namespace::Namespace;
use crate::vm::{DataType, MyStr, VariableMetadata, VirtualMachine};
use crate::vm::DataType::{Bool, Float, Int};

pub fn registerOut(vm: &mut VirtualMachine) {
    let mut namespace = Namespace::new("out".to_string());

    namespace.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static("Value"),
            typ: Int,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getNumRef()),
        None,
    );

    namespace.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static("Value"),
            typ: Bool,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getBool()),
        None,
    );

    namespace.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static("Value"),
            typ: Float,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getFlo()),
        None,
    );

    namespace.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::str(),
        }]),
        |_a, b| {
            let c = b.localVariables.get(0).unwrap();
            let str = c.asRef().getStr();
            println!("{}", str.string);
        },
        None,
    );

    vm.registerNamespace(namespace);
}