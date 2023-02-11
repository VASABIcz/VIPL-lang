use std::rc::Rc;

use crate::objects::Str;
use crate::vm::*;
use crate::vm::DataType::*;
use crate::vm::OpCode::*;
use crate::vm::Value::*;

pub fn bootStrapVM() -> VirtualMachine {
    let mut vm = VirtualMachine::new();

    vm.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static("Value"),
            typ: Int,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getNum()),
        None,
    );

    vm.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: MyStr::Static("Value"),
            typ: Float,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getFlo()),
        None,
    );

    vm.makeNative(
        String::from("assert"),
        Box::new([VariableMetadata::i(MyStr::Static("left")), VariableMetadata::i(MyStr::Static("right"))]),
        |_a, b| {
            let left = b.localVariables[1].getNum();
            let right = b.localVariables[0].getNum();
            if left != right {
                panic!("assert {left} != {right}")
            }
        },
        None,
    );

    vm.makeNative(
        String::from("exec"),
        Box::default(),
        |a, b| {
            /*
            let stack = match b.previous {
                None => b,
                Some(v) => v
            };
             */
            let genOps = [
                PushInt(1),
                Pop,
                PushInt(69),
                Call {
                    encoded: MyStr::Static("print(int)"),
                },
            ];

            let mut seek = SeekableOpcodes {
                index: 0,
                opCodes: &genOps,
                start: None,
                end: None,
            };

            run(&mut seek, a, b);
        },
        None,
    );

    vm.makeNative(String::from("print"), Box::new([VariableMetadata { name: MyStr::Static(""), typ: Object(Box::new(ObjectMeta { name: MyStr::Static("String"), generics: Box::new([]) })) }]), |_a, b| {
        let c = b.localVariables.get(0).unwrap();
        match c {
            Num(_) => {}
            Flo(_) => {}
            Bol(_) => {}
            Chr(_) => {}
            Reference { instance } => {
                match instance {
                    None => {}
                    Some(ee) => unsafe {
                        let mut clon = ee.clone();
                        let ne = Rc::get_mut_unchecked(&mut clon);
                        match ne.downcast_ref::<Str>() {
                            None => {}
                            Some(ff) => {
                                println!("{}", ff.string);
                            }
                        }
                    }
                }
            }
        }
    }, None);

    vm.makeNative(String::from("makeString"), Box::new([]), |a, _b| {
        a.stack.push(Value::Reference { instance: Some(Rc::new(Str { string: "".to_string() })) })
    }, Some(Object(Box::new(ObjectMeta { name: MyStr::Static("String"), generics: Box::new([]) }))));

    vm.makeNative(
        String::from("appendChar"),
        Box::new([
            VariableMetadata { name: MyStr::Static("str"), typ: DataType::str() },
            VariableMetadata { name: MyStr::Static("chr"), typ: DataType::Char }
        ]), |_a, b| {
            let chr = match b.localVariables.get(1).unwrap() {
                Chr(c) => *c,
                n => {
                    panic!("{n:?}")
                }
            };
            let str = b.localVariables.get_mut(0).unwrap();
            match str {
                Reference { instance } => {
                    match instance {
                        None => {
                            panic!()
                        }
                        Some(v) => unsafe {
                            let ne = Rc::get_mut_unchecked(v);
                            let e = ne.downcast_mut::<Str>().unwrap();
                            e.string.push(chr);
                        }
                    }
                }
                ee => {
                    panic!("{ee:?}");
                }
            }
        }, None);

    vm.makeNative("arrayLen".to_string(), Box::new([VariableMetadata { name: MyStr::Static(""), typ: DataType::arr(Generic::Any) }]), |vm, locals| {
        match locals.localVariables.get_mut(0).unwrap() {
            Reference { instance } => {
                match instance {
                    None => panic!(),
                    Some(v) => unsafe {
                        match v.downcast_ref::<crate::objects::Array>() {
                            None => panic!(),
                            Some(v) => {
                                vm.stack.push(Value::Num(v.internal.len() as isize))
                            }
                        }
                    }
                }
            }
            _ => panic!()
        }
    }, Some(DataType::Int));

    vm.makeNative("strLen".to_string(), Box::new([VariableMetadata { name: MyStr::Static(""), typ: DataType::str() }]), |vm, locals| {
        match locals.localVariables.get_mut(0).unwrap() {
            Reference { instance } => {
                match instance {
                    None => panic!(),
                    Some(v) => unsafe {
                        match v.downcast_ref::<Str>() {
                            None => panic!(),
                            Some(v) => {
                                vm.stack.push(Value::Num(v.string.len() as isize))
                            }
                        }
                    }
                }
            }
            _ => panic!()
        }
    }, Some(DataType::Int));

    vm.makeNative("getChar".to_string(), Box::new([VariableMetadata { name: MyStr::Static(""), typ: DataType::str() }, VariableMetadata { name: MyStr::Static(""), typ: DataType::Int }]), |vm, locals| {
        let index = locals.localVariables.get(1).unwrap().getNum();
        match locals.localVariables.get_mut(0).unwrap() {
            Reference { instance } => {
                match instance {
                    None => panic!(),
                    Some(v) => unsafe {
                        match v.downcast_ref::<Str>() {
                            None => panic!(),
                            Some(v) => {
                                vm.stack.push(Value::Chr(*v.string.as_bytes().get_unchecked(index as usize) as char))
                            }
                        }
                    }
                }
            }
            _ => panic!()
        }
    }, Some(DataType::Char));

    vm.makeNative("endsWith".to_string(), Box::new([VariableMetadata { name: MyStr::Static(""), typ: DataType::str() }, VariableMetadata { name: MyStr::Static(""), typ: DataType::str() }]), |vm, locals| {
        let sec = locals.localVariables.get(1).unwrap().clone();
        match locals.localVariables.get_mut(0).unwrap() {
            Reference { instance } => {
                match instance {
                    None => panic!(),
                    Some(v) => unsafe {
                        match v.downcast_ref::<Str>() {
                            None => panic!(),
                            Some(v) => {
                                match sec {
                                    Reference { instance } => {
                                        match instance {
                                            None => panic!(),
                                            Some(k) => {
                                                match k.downcast_ref::<Str>() {
                                                    None => panic!(),
                                                    Some(c) => {
                                                        vm.stack.push(Value::Bol(v.string.ends_with(&c.string)))
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    _ => panic!()
                                }
                            }
                        }
                    }
                }
            }
            _ => panic!()
        }
    }, Some(DataType::Char));

    vm
}