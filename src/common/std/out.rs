use crate::ast::Expression;
use crate::lexer::TokenType::In;
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
    let mut namespace = Namespace::new("out");

    namespace.makeNative(
        "print",
        &[Int],
        |_a, b| println!("{}", b.localVariables[0].getNumRef()),
        DataType::Void,
    );

    namespace.makeNative(
        "print",
        &[Bool],
        |_a, b| println!("{}", b.localVariables[0].getBool()),
        DataType::Void,
    );

    namespace.makeNative(
        "print",
        &[Float],
        |_a, b| println!("{}", b.localVariables[0].getFlo()),
        DataType::Void,
    );

    namespace.makeNative(
        "print",
        &[Char],
        |_a, b| println!("{}", b.localVariables[0].getChar()),
        DataType::Void,
    );

    namespace.makeNative(
        "print",
        &[DataType::str()],
        |_a, b| {
            let c = b.localVariables.get(0).unwrap();
            let str = &c.asRef::<Str>().data;
            println!("{}", str.string);
        },
        DataType::Void,
    );

    namespace.makeNative(
        "print",
        &[DataType::arr(Generic::Any)],
        |_a, b| {
            let c = b.localVariables.get(0).unwrap();
            let str = &c.asRef::<Str>().data;
            println!("{}", str.string);
        },
        DataType::Void,
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