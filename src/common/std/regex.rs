use crate::ast::FunctionDef;
use crate::ffi::stringNew;
use crate::regix::Regix;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta, Namespace};
use crate::vm::nativeObjects::{blankCollect, blankDestroy, blankGetField, blankSetField, ObjectType, ViplNativeObject, ViplObject, ViplObjectMeta};
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

#[derive(Debug)]
struct RegixData {
    pub reg: Regix,
    pub capturesCount: usize
}

impl Drop for RegixData {
    fn drop(&mut self) {
        println!("being dropped")
    }
}

impl Allocation for RegixData {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        todo!()
    }
}

pub fn registerRegex(vm: &mut VirtualMachine) {
    let mut n = Namespace::new("re");

    n.makeNative("compile", &[DataType::str()], |vm, s| {
        let str = s.localVariables.get(0).unwrap().getString();

        let res = Regix::parse(str);

        let a = ViplObject{
            meta: ViplObjectMeta {
            namespaceId: s.namespaceId,
            structId: 0,
            objectType: ObjectType::Native(ViplNativeObject{
                getField: blankGetField,
                setField: blankSetField,
                destroy: blankDestroy,
                collect: blankCollect,
            }),
        }, data: RegixData{ reg: res, capturesCount: 0 }
        };

        let ptr = vm.heap.allocate(a);

        vm.push(ptr.into())

    }, DataType::obj("Regex"));

    n.makeNative("doesMatch", &[DataType::obj("Regex"), DataType::str()], |vm, s| {
        let reg = s.localVariables.get(0).unwrap().getReference::<RegixData>();
        let str = s.localVariables.get(1).unwrap().getString();

        let mut buf = vec![];

        let res = reg.data.reg.matchStr(str, &mut buf);

        let isMatch = match res {
            None => false,
            Some(v) => str.len() == v
        };

        vm.push(isMatch.into());

    }, DataType::Bool);

    n.makeNative("match", &[DataType::obj("Regex"), DataType::str()], |vm, s| {
        let reg = s.localVariables.get(0).unwrap().getReference::<RegixData>();
        let str = s.localVariables.get(1).unwrap().getString();

        let mut buf = vec![];

        reg.data.reg.matchStr(str, &mut buf);

        println!("matched {:?}", buf);

        let mut result: Vec<Value> = vec![];

        for item in buf {
            let a = item.iter().map(|it| vm.allocateString(it).into()).collect::<Vec<Value>>();
            result.push(vm.allocateArray(a, DataType::str()).into())
        }

        let aloc = vm.allocateArray(result, DataType::arr(Generic::Type(DataType::str())));

        vm.push(aloc.into())

    },  DataType::arr(Generic::Type(DataType::arr(Generic::Type(DataType::str())))));

    let namespaceId = vm.registerNamespace(n);
}