
use std::fs;

use std::os::unix::fs::DirEntryExt2;

use crate::vm::{DataType, Generic, MyStr, Value, VariableMetadata, VirtualMachine};

pub fn setupFs(vm: &mut VirtualMachine) {
    vm.makeNative(
        "ls".to_string(),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::str(),
        }]),
        #[inline(always)] |vm, locals| {
            let path = locals.localVariables.first().unwrap().getString();


            match fs::read_dir(path) {
                Ok(v) => unsafe {
                    let arr = v.map(#[inline(always)] |it|{
                        let refName = it.unwrap();
                        Value::makeString(String::from(refName.file_name_ref().to_str().unwrap()), vm)
                    }).collect();
                    let a = Value::makeArray(arr, DataType::str(), vm);
                    vm.stack.push(a)
                }
                Err(e) => panic!("{}", e)
            }
        },
        Some(DataType::arr(Generic::Type(DataType::str()))),
    );

    vm.makeNative(
        "readFile".to_string(),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::str(),
        }]),
        |vm, locals| {
            let path = locals.localVariables.first().unwrap().getString();

            let str = fs::read_to_string(path).unwrap_or_default();
            let a = Value::makeString(str, vm);
            vm.stack.push(a)
        },
        Some(DataType::str()),
    );

    vm.makeNative(
        "fileType".to_string(),
        Box::new([VariableMetadata {
            name: MyStr::Static(""),
            typ: DataType::str(),
        }]),
        |vm, locals| {
            let path = locals.localVariables.first().unwrap().getString();

            let val = match fs::metadata(path) {
                Ok(v) => {
                    let mut buf = 0;
                    if v.is_file() {
                        buf = 1
                    }
                    else if v.is_dir() {
                        buf = 2
                    }
                    else if v.is_symlink() {
                        buf = 4
                    }
                    buf
                }
                Err(_) => 0,
            };

            vm.stack.push(Value::from(val))
        },
        Some(DataType::Int),
    );
}
