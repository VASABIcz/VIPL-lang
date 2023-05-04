use crate::std::json::registerJson;
use crate::std::out::registerOut;
use crate::std::regex::registerRegex;
use crate::std::vm::registerVm;
use crate::vm::vm::VirtualMachine;

pub fn registerStd(vm: &mut VirtualMachine) {
    /*
    vm.makeNative(
        String::from("assert"),
        Box::new([
            VariableMetadata::i(MyStr::Static("left")),
            VariableMetadata::i(MyStr::Static("right")),
        ]),
        |_a, b| {
            let left = b.localVariables[1].getNumRef();
            let right = b.localVariables[0].getNumRef();
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
                opCodes: &mut genOps,
            };

            run(&mut seek, a, b);
        },
        None,
    );

    vm.makeNative(
        String::from("makeString"),
        Box::new([]),
        |vm, _b| {
            vm.stack.push(Value{Reference: vm.heap.allocate(ViplObject::Str(Str::new("".to_string())))});
        },
        Some(DataType::str()),
    );

    vm.makeNative(
        String::from("gc"),
        Box::new([]),
        |vm, frame| {
            vm.gc(frame);
        },
        Some(DataType::str()),
    );

    vm.makeNative(
        String::from("appendChar"),
        Box::new([
            VariableMetadata {
                name: MyStr::Static("str"),
                typ: DataType::str(),
            },
            VariableMetadata {
                name: MyStr::Static("chr"),
                typ: DataType::Char,
            },
        ]),
        |_a, b| {
            let chr = b.localVariables.get(1).unwrap().asChar();
            let str = b.localVariables.get_mut(0).unwrap();
            str.asMutRef().getMutStr().string.push(chr);
        },
        None,
    );

    vm.makeNative(
        "arrayLen".to_string(),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::arr(Generic::Any),
        }]),
        |vm, locals| {
            let size = locals.localVariables.get_mut(0).unwrap().asRef().getArr().internal.len();
            vm.stack.push(Value{Num: size as isize})
        },
        Some(DataType::Int),
    );

    vm.makeNative(
        "strLen".to_string(),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::str(),
        }]),
        |vm, locals| {
            let size = locals.localVariables.get_mut(0).unwrap().asRef().getStr().string.len();
            vm.stack.push(Value{Num: size as isize})
        },
        Some(DataType::Int),
    );

    unsafe {
        vm.makeNative(
            "getChar".to_string(),
            Box::new([
                VariableMetadata {
                    name: MyStr::Static(""),
                    typ: DataType::str(),
                },
                VariableMetadata {
                    name: MyStr::Static(""),
                    typ: DataType::Int,
                },
            ]),
            |vm, locals| {
                let index = locals.localVariables.get(1).unwrap().getNumRef();
                let c = locals.localVariables.get(0).unwrap().Reference.getStr().string.as_bytes().get_unchecked(index as usize);
                vm.stack.push(Value { Chr: *c as char })
            },
            Some(DataType::Char),
        );
    }

    vm.makeNative(
        "endsWith".to_string(),
        Box::new([
            VariableMetadata {
                name: MyStr::Static(""),
                typ: DataType::str(),
            },
            VariableMetadata {
                name: MyStr::Static(""),
                typ: DataType::str(),
            },
        ]),
        |vm, locals| {
            let sec = locals.localVariables.get(1).unwrap().asRef().getStr();
            let str = locals.localVariables.get(0).unwrap().asRef().getStr();
            vm.stack.push(Value{Bol: str.string.ends_with(&sec.string)});
        },
        Some(DataType::Bool),
    );

    vm.makeNative(
        String::from("loadNative"),
        Box::new([
            VariableMetadata::from(DataType::str()), // path
            VariableMetadata::from(DataType::str()), // name
            VariableMetadata::from(DataType::Int),   // arg count
        ]),
        |vm, locals| unsafe {
            let path = locals.localVariables.get(0).unwrap().getString();
            let name = locals.localVariables.get(1).unwrap().getString();
            let argCount = locals.localVariables.get(2).unwrap().getNumRef();

            vm.loadRawNative(path, name, None, argCount as usize)
        },
        None,
    );

     */
}

pub fn bootStrapVM() -> VirtualMachine<'static> {
    let mut vm = VirtualMachine::new();

    registerOut(&mut vm);
    registerVm(&mut vm);
    registerRegex(&mut vm);
    registerJson(&mut vm);

    vm
}