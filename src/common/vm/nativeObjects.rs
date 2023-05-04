use std::collections::HashMap;
use std::fmt::Debug;
use crate::errors::Errorable;
use crate::lexer::{AlphabeticKeywordLexingUnit, IdentifierLexingUnit, KeywordLexingUnit, LexingUnit, RangeLexingUnit, SourceProvider, tokenize, WhitespaceLexingUnit};
use crate::lexer::TokenType::Identifier;
use crate::parser::NumericParsingUnit;
use crate::vm::dataType::DataType;
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::namespace::StructMeta;
use crate::vm::objects::{Array, Str};
use crate::vm::value::{Value, Xd};
use crate::vm::vm::VirtualMachine;

pub fn blankDestroy<T>(this: &mut T) {
    println!("blankDestroy called")
}

pub fn blankGetField<T>(this: &mut T, vm: &mut VirtualMachine, index: usize) -> Option<Value> { return None }

pub fn blankSetField<T>(this: &mut T, vm: &mut VirtualMachine, index: usize, value: Value) {
    println!("blankSetField called")
}

pub fn blankCollect<T>(this: &mut T, allocations: &mut HayCollector) {
    println!("blankCollect called")
}

pub trait NativeObject: Allocation + Debug {
    fn getField(&mut self, field: usize, vm: &mut VirtualMachine) -> Option<Value>;

    fn setField(&mut self, field: usize, value: Value, vm: &mut VirtualMachine);

    fn destroy(&mut self, vm: &mut VirtualMachine);
}

#[derive(Debug, Copy, Clone)]
pub struct ViplNativeObject<T: Debug> {
    pub getField: fn (&mut T, vm: &mut VirtualMachine, index: usize) -> Option<Value>,
    pub setField: fn (&mut T, vm: &mut VirtualMachine, index: usize, value: Value) -> (),
    pub destroy: fn (&mut T) -> (),
    pub collect: fn (&mut T, allocations: &mut HayCollector) -> ()
}

impl<T: Allocation + Debug> Default for ViplNativeObject<T> {
    fn default() -> Self {
        Self {
            getField: blankGetField,
            setField: blankSetField,
            destroy: blankDestroy,
            collect: blankCollect,
        }
    }
}

#[derive(Debug)]
pub struct SimpleObjectWrapper<const N: usize> {
    pub fields: [Value; N]
}

impl<const N: usize> Allocation for SimpleObjectWrapper<N> {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        for c in self.fields {
            allocations.visitHay(c.asHay::<Xd>())
        }
    }
}

#[derive(Debug, Clone)]
pub enum ObjectType<T: Debug> {
    Simple(usize),
    Native(ViplNativeObject<T>)
}

impl<T: Debug + Copy> Copy for ObjectType<T> {

}

#[derive(Debug)]
pub struct ViplObjectMeta<T: Debug> {
    pub namespaceId: usize,
    pub structId: usize,
    pub objectType: ObjectType<T>,
}

impl<T: Debug + Copy> Clone for ViplObjectMeta<T> {
    fn clone(&self) -> ViplObjectMeta<T> {
        Self {
            namespaceId: self.namespaceId,
            structId: self.structId,
            objectType: self.objectType,
        }
    }
}

#[derive(Debug)]
pub struct ViplObject<T: Allocation + Debug> {
    pub meta: ViplObjectMeta<T>,
    pub data: T
}

impl<T: Allocation + Debug> ViplObject<T> {
    pub fn arr(arr: Array) -> ViplObject<Array> {
        ViplObject{ meta: ViplObjectMeta {
            namespaceId: 0,
            structId: 1,
            objectType: ObjectType::Native(ViplNativeObject::default()),
        }, data: arr }
    }

    pub fn str(str: Str) -> ViplObject<Str> {
        ViplObject{ meta: ViplObjectMeta {
            namespaceId: 0,
            structId: 1,
            objectType: ObjectType::Native(ViplNativeObject::default()),
        }, data: str
        }
    }
}

pub type UntypedObject = ViplObjectMeta<()>;

impl<T: Allocation + Debug> Allocation for ViplObject<T> {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        self.data.collectAllocations(allocations)
    }
}

impl<T: Debug> ViplObjectMeta<T> {
    pub fn free(&mut self) {
        match &self.objectType {
            ObjectType::Simple(_) => {}
            ObjectType::Native(v) => unsafe {
                let offsetPtr = unsafe { (self as *const ViplObjectMeta<T>).add(1) };

                v.destroy.call((&mut *(offsetPtr as *mut T), ))
            }
        }
    }
}

impl<T: Debug> Allocation for ViplObjectMeta<T> {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        let offsetPtr = unsafe { (self as *const ViplObjectMeta<T>).add(1) };

        match &self.objectType {
            ObjectType::Simple(count) => {
                for i in 0..*count {
                    let valPtr = offsetPtr as *const Value;
                    let v = unsafe { valPtr.add(i).read() };
                    v.collectAllocations(allocations)
                }
            }
            ObjectType::Native(n) => unsafe {
                n.collect.call((&mut *(offsetPtr as *mut T), allocations));
            }
        }
    }
}

#[derive(Debug)]
pub enum JSON {
    JObject(HashMap<String, JSON>),
    JArray(Vec<JSON>),
    JBool(bool),
    JChar(char),
    JInt(isize),
    JFloat(f64),
    JString(ViplObject<Str>),
    JNull
}

#[derive(Debug, Copy, Clone)]
pub enum JsonToken {
    OCB,
    CCB,
    OSB,
    CSB,
    Colon,

    String,
    Number,

    Null,
    True,
    False,

    Identifier
}

pub fn jsonTokenizingUnits() -> Vec<Box<dyn LexingUnit<JsonToken>>> {
    vec![
        AlphabeticKeywordLexingUnit::new("false", JsonToken::False),
        AlphabeticKeywordLexingUnit::new("true", JsonToken::False),
        AlphabeticKeywordLexingUnit::new("null", JsonToken::False),

        KeywordLexingUnit::new("{", JsonToken::OCB),
        KeywordLexingUnit::new("}", JsonToken::CCB),
        KeywordLexingUnit::new("[", JsonToken::OSB),
        KeywordLexingUnit::new("]", JsonToken::CSB),
        KeywordLexingUnit::new(",", JsonToken::Colon),

        RangeLexingUnit::new("\"", "\"", Some(JsonToken::String)),
        WhitespaceLexingUnit::new(),
        IdentifierLexingUnit::new(JsonToken::Identifier)
    ]
}

impl JSON {
    pub fn parse(s: &str) -> Errorable<JSON> {
        let res = tokenize(&mut jsonTokenizingUnits(), SourceProvider{ data: s, index: 0 })?;

        None.ok_or("fsdfsd")?
    }
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