use crate::ast::FunctionDef;
use crate::ffi::stringNew;
use crate::std::regix::Regix;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta, Namespace};
use crate::vm::nativeObjects::{
    blankCollect, blankDestroy, blankGetField, blankSetField, ObjectType, ViplNativeObject,
    ViplObject, ViplObjectMeta,
};
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

#[derive(Debug)]
struct RegixData {
    pub reg: Regix,
    pub capturesCount: usize,
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
    let mut n = Namespace::new("re", vm);

    n.makeNative(
        "compile",
        &[DataType::str()],
        |vm, s| {
            let str = s.getRef(0).getString();

            let res = Regix::parse(str);

            let a = ViplObject {
                meta: ViplObjectMeta {
                    namespaceId: s.namespaceId,
                    structId: 0,
                    objectType: ObjectType::Native(ViplNativeObject {
                        getField: blankGetField,
                        setField: blankSetField,
                        destroy: blankDestroy,
                        collect: blankCollect,
                    }),
                },
                data: RegixData {
                    reg: res,
                    capturesCount: 0,
                },
            };

            let ptr = vm.allocate(a);

            Value::from(ptr)
        },
        DataType::obj("Regex"),
        false,
    );

    n.makeNative(
        "doesMatch",
        &[DataType::obj("Regex"), DataType::str()],
        |vm, s| {
            let reg = s.getRef(0).getReference::<RegixData>();
            let str = s.getRef(1).getString();

            let mut buf = vec![];

            let res = reg.data.reg.matchStr(str, &mut buf);

            let isMatch = match res {
                None => false,
                Some(v) => str.len() == v,
            };

            isMatch.into()
        },
        DataType::Bool,
        false,
    );

    n.makeNative(
        "match",
        &[DataType::obj("Regex"), DataType::str()],
        |vm, s| {
            let reg = s.getRef(0).getReference::<RegixData>();
            let str = s.getRef(1).getString();

            let mut buf = vec![];

            reg.data.reg.matchStr(str, &mut buf);

            println!("matched {:?}", buf);

            let mut result: Vec<Value> = vec![];

            for item in buf {
                let a = item
                    .iter()
                    .map(|it| vm.allocateString(it).into())
                    .collect::<Vec<Value>>();
                result.push(vm.allocateArray(a).into())
            }

            let aloc = vm.allocateArray(result);

            aloc.into()
        },
        DataType::arr(Generic::Type(DataType::arr(Generic::Type(DataType::str())))),
        false,
    );

    let namespaceId = vm.registerNamespace(n);
}
