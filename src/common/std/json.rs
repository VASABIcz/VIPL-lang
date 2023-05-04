use crate::vm::dataType::DataType;
use crate::vm::namespace::{Namespace, StructMeta};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

pub fn registerJson(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("js");

    n.registerStruct(StructMeta{
        name: "JBool".to_string(),
        fieldsLookup: Default::default(),
        fields: vec![VariableMetadata{ name: "v".into(), typ: DataType::Bool }],
    });

    n.makeNative("load", &[DataType::str()], |vm, s| {
        todo!()
    }, DataType::obj("Json"));

    n.makeNative("save", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::str());

    n.makeNative("getIndex", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("Json"));

    n.makeNative("length", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("JInt"));

    n.makeNative("hasKey", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("JBool"));

    n.makeNative("getField", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("Json"));

    n.makeNative("asString", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::str());

    n.makeNative("asInt", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("JInt"));

    n.makeNative("asBool", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("JBool"));

    n.makeNative("asNull", &[DataType::obj("Json")], |vm, s| {
        todo!()
    }, DataType::obj("JNull"));

    vm.registerNamespace(n);
}