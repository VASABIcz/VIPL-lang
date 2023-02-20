use std::env::args;
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
            let mut genOps = [
                PushInt(1),
                Pop,
                PushInt(69),
                Call {
                    encoded: MyStr::Static("print(int)"),
                },
            ];

            let mut seek = SeekableOpcodes {
                index: 0,
                opCodes: &mut genOps
            };

            run(&mut seek, a, b);
        },
        None,
    );

    vm.makeNative(String::from("print"), Box::new([VariableMetadata { name: MyStr::Static(""), typ: DataType::str() }]), |_a, b| {
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
                        let ff = ne.getStr();
                        println!("{}", ff.string);
                    }
                }
            }
        }
    }, None);

    vm.makeNative(String::from("makeString"), Box::new([]), |a, _b| {
        a.stack.push(Value::Reference { instance: Some(Rc::new(Str { string: "".to_string() }.into())) })
    }, Some(DataType::str()));

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
                            let e = ne.getMutStr();
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
                        let v = v.getArr();
                        vm.stack.push(Value::Num(v.internal.len() as isize))
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
                        let a = v.getStr();
                        vm.stack.push(Value::Num(a.string.len() as isize))
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
                        let a = v.getStr();
                        vm.stack.push(Value::Chr(*a.string.as_bytes().get_unchecked(index as usize) as char))
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
                        let e = v.getStr();
                        match sec {
                            Reference { instance } => {
                                match instance {
                                    None => panic!(),
                                    Some(k) => {
                                        let c = k.getStr();
                                        vm.stack.push(Value::Bol(e.string.ends_with(&c.string)))
                                    }
                                }
                            }
                            _ => panic!()
                        }
                    }
                }
            }
            _ => panic!()
        }
    }, Some(DataType::Bool));

    vm.makeNative(String::from("loadNative"), box [VariableMetadata::from(DataType::str())], |vm, locals| {
        let path = locals.localVariables.get(0).unwrap().getString();
    }, None);

    vm
}