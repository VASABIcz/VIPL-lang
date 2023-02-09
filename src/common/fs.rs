use std::fs;

use crate::vm::{DataType, Generic, Value, VariableMetadata, VirtualMachine};

pub fn setupFs(vm: &mut VirtualMachine) {
    vm.makeNative("ls".to_string(), Box::new([VariableMetadata { name: "".to_string().into(), typ: DataType::str() }]), |vm, locals| {
        let path = locals.localVariables.first().unwrap().getString();

        let mut buf = vec![];

        match fs::read_dir(&path) {
            Ok(v) => {
                for p in v {
                    buf.push(Value::makeString(String::from(p.unwrap().file_name().to_str().unwrap())))
                }
            }
            Err(_) => {}
        }

        vm.stack.push(Value::makeArray(buf, DataType::str()))
    }, Some(DataType::arr(Generic::Type(DataType::str()))));

    vm.makeNative("readFile".to_string(), Box::new([VariableMetadata { name: "".to_string().into(), typ: DataType::str() }]), |vm, locals| {
        let path = locals.localVariables.first().unwrap().getString();

        let str = fs::read_to_string(path).unwrap_or_default();
        vm.stack.push(Value::makeString(str))
    }, Some(DataType::str()));

    vm.makeNative("fileType".to_string(), Box::new([VariableMetadata { name: "".to_string().into(), typ: DataType::str() }]), |vm, locals| {
        let path = locals.localVariables.first().unwrap().getString();

        let val = match fs::metadata(path) {
            Ok(v) => {
                let mut buf = 0;
                if v.is_file() {
                    buf += 1
                }
                if v.is_dir() {
                    buf += 2
                }
                if v.is_symlink() {
                    buf += 4
                }
                buf
            }
            Err(_) => 0
        };

        vm.stack.push(Value::Num(val))
    }, Some(DataType::Int));
}