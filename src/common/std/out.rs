use crate::ast::Expression;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::{DataType, Generic};
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};
use crate::vm::myStr::MyStr;
use crate::vm::namespace::{GlobalMeta, Namespace};
use crate::vm::namespace::NamespaceState::Loaded;
use crate::vm::objects::Str;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

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
            name: MyStr::Static("Value"),
            typ: Char,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getChar()),
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
            let str = c.asRef::<Str>().data;
            println!("{}", str.string);
        },
        None,
    );

    namespace.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::arr(Generic::Any),
        }]),
        |_a, b| {
            let c = b.localVariables.get(0).unwrap();
            let str = c.asRef::<Str>().data;
            println!("{}", str.string);
        },
        None,
    );

    let index = namespace.registerGlobal(GlobalMeta{
        name: "newLine".to_string(),
        default: Expression::CharLiteral('\n'),
        typ: DataType::Char,
    });

    *namespace.globals.get_mut(index).unwrap() = Value::from('\n');

    namespace.state = Loaded;
    vm.registerNamespace(namespace);
}