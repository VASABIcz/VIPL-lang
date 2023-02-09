use std::fs;

use crate::objects::{Array, Str};
use crate::vm::{DataType, Generic, Value, VariableMetadata, VirtualMachine};
use crate::vm::DataType::Int;
use crate::vm::Value::Reference;

pub fn setupFs(vm: &mut VirtualMachine) {
    vm.makeNative("ls".to_string(), Box::new([VariableMetadata { name: "".to_string().into(), typ: DataType::Str() }]), |vm, locals| {
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

        vm.stack.push(Value::makeArray(buf, DataType::Str()))
    }, Some(DataType::Arr(Generic::Type(DataType::Str()))));

    vm.makeNative("readFile".to_string(), Box::new([VariableMetadata { name: "".to_string().into(), typ: DataType::Str() }]), |vm, locals| {
        let path = locals.localVariables.first().unwrap().getString();

        let str = fs::read_to_string(path).unwrap_or_default();
        vm.stack.push(Value::makeString(str))
    }, Some(DataType::Str()));

    vm.makeNative("fileType".to_string(), Box::new([VariableMetadata { name: "".to_string().into(), typ: DataType::Str() }]), |vm, locals| {
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