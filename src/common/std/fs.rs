use std::fs;
use crate::vm::dataType::{DataType, Generic};
use crate::vm::namespace::Namespace;
use crate::vm::value::Value;

use crate::vm::vm::VirtualMachine;

pub fn registerFs(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("fs", vm);

    n.makeNative("ls", &[DataType::str()], |vm, s| {
        let path = s.getString(0);
        println!("path {}", path);

        match fs::read_dir(&path) {
            Ok(v) => unsafe {
                let arr = v.map(#[inline(always)] |it|{
                    let refName = it.unwrap();
                    Value::makeString(String::from(refName.file_name().as_os_str().to_str().unwrap()), vm)
                }).collect();
                Value::makeArray(arr, vm)
            }
            Err(e) => panic!("{} {}", e, path)
        }
    }, DataType::arr(Generic::Type(DataType::str())), false);


    n.makeNative("readFile", &[DataType::str()], |vm, s| {
        let path = s.getString(0);

        let str = fs::read_to_string(path).unwrap_or_default();

        Value::makeString(str, vm)
    }, DataType::str(), false);

    n.makeNative("fileType", &[DataType::str()], |vm, s| {
        let path = s.getString(0);

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

        Value::from(val)
    }, DataType::Int, false);

    vm.registerNamespace(n);
}
