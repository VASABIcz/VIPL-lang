use std::collections::HashMap;
use std::fmt::Debug;
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::namespace::StructMeta;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

pub trait NativeObject: Allocation + Debug {
    fn getField(&mut self, field: usize, vm: &mut VirtualMachine) -> Option<Value>;

    fn setField(&mut self, field: usize, value: Value, vm: &mut VirtualMachine);

    fn destroy(&mut self, vm: &mut VirtualMachine);
}

enum ObjectType {
    Simple,
    Complex
}

struct SimpleVipleObject {
    namespaceId: usize,
    structId: usize,
    typ: ObjectType,
    data: [Value]
}

struct ComplexVipleObject {
    namespaceId: usize,
    structId: usize,
    typ: ObjectType,
    obj: dyn NativeObject
}

#[derive(Debug)]
enum JSON {
    JObject(HashMap<String, JSON>),
    JArray(Vec<JSON>),
    JBool(bool),
    JChar(char),
    JInt(isize),
    JFloat(f64),
    JString(String),
    JNull
}

#[derive(Debug)]
struct JsonObject {
    data: JSON
}

impl Allocation for JsonObject {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        todo!()
    }
}

impl NativeObject for JsonObject {
    fn getField(&mut self, field: usize, vm: &mut VirtualMachine) -> Option<Value> {
        todo!()
    }

    fn setField(&mut self, field: usize, value: Value, vm: &mut VirtualMachine) {
        todo!()
    }

    fn destroy(&mut self, vm: &mut VirtualMachine) {
        todo!()
    }
}

struct RegixObject {
    regix: String,
    v: StructMeta
}


impl RegixObject {

}

/*
cant be constructed
are constructed by a function call

regix object
- captureCount

tcp connection object
-

json support


struct JInt {
    v: int
}

struct JBool {
    v: int
}

struct JChar {
    v: int
}

struct JFloat {
    v: int
}

struct JSON {
    isObject: bool
    isArray: bool
    isString: bool
    isNumber: bool
    isBool: bool
    isNull: bool
}

j: JSON = js::jsonLoad("{"uwu": "owo"}")
val = js::getKey(j, "uwu") // currentyl possible
val = j.getKey("uwu").asString() // goal
val = j["uwu"].asString() // goal


if val == null {
    out::print("exit")
    os::exit()
}

file object


sock = tcp::connect("localhost:6000")
msg = [1, 2, 3, 4, 5, 6, 7, 8]
tcp::write(sock, msg)
tcp::close(sock)
 */