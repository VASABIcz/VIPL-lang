use crate::ast::{Expression, RawExpression};
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};
use crate::vm::dataType::{DataType, Generic};
use crate::vm::namespace::NamespaceState::Loaded;
use crate::vm::namespace::{GlobalMeta, Namespace};
use crate::vm::objects::{Array, Str};
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

pub fn registerOut(vm: &mut VirtualMachine) {
    let mut namespace = Namespace::new("out", vm);

    namespace.makeNativeNoRat(
        "print",
        &[Int],
        |_a, b| println!("{}", b.get(0).getNumRef()),
        false,
    );

    namespace.makeNativeNoRat(
        "println",
        &[Int],
        |_a, b| println!("{}", b.get(0).getNumRef()),
        false,
    );

    namespace.makeNativeNoRat(
        "println",
        &[Float],
        |_a, b| println!("{}", b.get(0).getFlo()),
        false,
    );

    namespace.makeNativeNoRat(
        "println",
        &[DataType::str()],
        |_a, b| println!("{}", b.get(0).getString()),
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[Bool],
        |_a, b| println!("{}", b.get(0).getBool()),
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[Float],
        |_a, b| println!("{}", b.get(0).getFlo()),
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[Char],
        |_a, b| println!("{}", b.get(0).getChar()),
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[DataType::str()],
        |_a, b| {
            let c = b.get(0);
            let str = &c.asRef::<Str>().data;
            println!("{}", str.string);
        },
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[DataType::arr(Generic::Any)],
        |_a, b| {
            let c = b.get(0);
            let ar = &c.asRef::<Array>();


            let d = ar.data.internal.iter().map(|it| format!("{:?}", it)).collect::<Vec<_>>().join(", ");
            println!("[{}]", d)
        },
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[DataType::arr(Generic::Type(DataType::str()))],
        |_a, b| {
            let c = b.get(0);
            let ar = &c.asRef::<Array>();

            let d = ar.data.internal.iter().map(|it| format!("{:?}", it.getString())).collect::<Vec<_>>().join(", ");
            println!("[{}]", d)
        },
        false,
    );

    namespace.makeNativeNoRat(
        "print",
        &[DataType::Value],
        |_a, b| {
            let c = b.get(0).asUnsigned();

            println!("{:?}", c)
        },
        false,
    );

    namespace.makeNativeNoRat(
        "write",
        &[DataType::Char],
        |a, b| {
            let c = b.get(0).asChar();

            print!("{}", c);
        },
        false
    );

    let index = namespace.registerGlobal(GlobalMeta {
        name: "newLine".to_string(),
        default: Expression{ exp: RawExpression::CharLiteral('\n'), loc: vec![] },
        typ: DataType::Char,
    });

    namespace.getGlobalMut(index).1 = Value::from('\n');

    namespace.state = Loaded;
    vm.registerNamespace(namespace);
}
