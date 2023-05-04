use std::collections::HashMap;
use std::fmt::Debug;
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::namespace::StructMeta;
use crate::vm::objects::{Array, Str};
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

fn blankDestroy<T>(this: &mut T, vm: &mut VirtualMachine) {}

fn blankGetField<T>(this: &mut T, vm: &mut VirtualMachine, index: usize) -> Option<Value> { return None }

fn blankSetField<T>(this: &mut T, vm: &mut VirtualMachine, index: usize, value: Value) {}

fn blankCollect<T>(this: &mut T, vm: &mut VirtualMachine, allocations: &mut HayCollector) {

}

pub trait NativeObject: Allocation + Debug {
    fn getField(&mut self, field: usize, vm: &mut VirtualMachine) -> Option<Value>;

    fn setField(&mut self, field: usize, value: Value, vm: &mut VirtualMachine);

    fn destroy(&mut self, vm: &mut VirtualMachine);
}

pub struct ViplNativeObject<T: Debug> {
    pub getField: fn (&mut T, vm: &mut VirtualMachine, index: usize) -> Option<Value>,
    pub setField: fn (&mut T, vm: &mut VirtualMachine, index: usize, value: Value) -> (),
    pub destroy: fn (&mut T, vm: &mut VirtualMachine) -> (),
    pub collect: fn (&mut T, vm: &mut VirtualMachine, allocations: &mut HayCollector) -> ()
}

impl<T: Allocation + Debug> Default for ViplNativeObject<T> {
    fn default() -> Self {
        Self {
            getField: blankGetField,
            setField: blankSetField,
            destroy: blankDestroy,
        }
    }
}

pub struct SimpleObjectWrapper<const N: usize> {
    pub fields: [Value; N]
}

impl<const N: usize> Allocation for SimpleObjectWrapper<N> {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        for c in self.fields {
            allocations.visitHay(c.asHay())
        }
    }
}

pub enum ObjectType<T: Debug> {
    Simple,
    Native(ViplNativeObject<T>)
}

pub struct ObjectMeta<T: Debug> {
    pub namespaceId: usize,
    pub structId: usize,
    pub objectType: ObjectType<T>,
}

impl<T: Allocation + Debug> Clone for ObjectMeta<T> {
    fn clone(&self) -> Self {
        Self {
            namespaceId: *self.namespaceId,
            structId: *self.structId,
            objectType: ObjectType::Simple,
        }
    }
}

#[derive(Debug)]
pub struct ViplObject<T: Allocation + Debug> {
    pub meta: ObjectMeta<T>,
    pub data: T
}

impl<T: Allocation + Debug> ViplObject<T> {
    pub fn arr(arr: Array) -> ViplObject<Array> {
        ViplObject{ meta: ObjectMeta {
            namespaceId: 0,
            structId: 1,
            objectType: ObjectType::Native(ViplNativeObject {
                getField: blankGetField,
                setField: blankSetField,
                destroy: blankDestroy,
            }),
        }, data: arr }
    }

    pub fn str(str: Str) -> ViplObject<Str> {
        ViplObject{ meta: ObjectMeta {
            namespaceId: 0,
            structId: 1,
            objectType: ObjectType::Native(ViplNativeObject {
                getField: blankGetField,
                setField: blankSetField,
                destroy: blankDestroy,
            }),
        }, data: str
        }
    }
}

pub type UntypedObject = ObjectMeta<()>;

impl<T: Allocation + Debug> Allocation for ViplObject<T> {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        self.data.collectAllocations(allocations)
    }
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