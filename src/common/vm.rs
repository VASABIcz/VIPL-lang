use std::{ptr, rc};
use std::borrow::BorrowMut;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::{forget, transmute};
use std::rc::Rc;
use std::thread::sleep;
use std::time::Duration;

use libloading::{Library, Symbol};

use crate::ast::{Expression, Op};
use crate::ffi::NativeWrapper;
use crate::objects::{ObjectDefinition, Str, ViplObject};
use crate::parser::Operation::Expr;
use crate::parser::parseDataType;
use crate::std::bootStrapVM;
use crate::vm::DataType::*;
use crate::vm::FuncType::*;
use crate::vm::OpCode::*;
use crate::vm::Value::*;

#[derive(Debug, Clone, Eq)]
pub enum MyStr {
    Static(&'static str),
    Runtime(Box<str>),
}

impl PartialEq for MyStr {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Hash for MyStr {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl From<Box<str>> for MyStr {
    #[inline]
    fn from(value: Box<str>) -> Self {
        Self::Runtime(value)
    }
}

impl From<String> for MyStr {
    #[inline]
    fn from(value: String) -> Self {
        Self::Runtime(value.into_boxed_str())
    }
}

impl From<&'static str> for MyStr {
    #[inline]
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

impl Display for MyStr {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MyStr::Static(v) => {
                write!(f, "{v}")
            }
            MyStr::Runtime(v) => {
                write!(f, "{v}")
            }
        }
    }
}

impl MyStr {
    #[inline]
    pub fn as_str(&self) -> &str {
        match self {
            MyStr::Static(s) => s,
            MyStr::Runtime(v) => v,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Object(ObjectMeta),
}

impl From<DataType> for Value {
    #[inline]
    fn from(val: DataType) -> Self {
        val.toDefaultValue()
    }
}

impl From<isize> for Value {
    #[inline]
    fn from(val: isize) -> Self {
        Num(val)
    }
}

impl From<char> for Value {
    #[inline]
    fn from(val: char) -> Self {
        Chr(val)
    }
}

impl From<f32> for Value {
    #[inline]
    fn from(val: f32) -> Self {
        Flo(val)
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(val: bool) -> Self {
        Bol(val)
    }
}

impl DataType {
    pub fn str() -> Self {
        Object(ObjectMeta {
            name: MyStr::Static("String"),
            generics: Box::new([]),
        })
    }
    pub fn arr(inner: Generic) -> Self {
        Object(ObjectMeta {
            name: MyStr::Static("Array"),
            generics: Box::new([inner]),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum Generic {
    Any,
    Type(DataType),
}

impl Generic {
    pub fn ok_or<E>(self, err: E) -> Result<DataType, E> {
        match self {
            Generic::Any => Err(err),
            Generic::Type(v) => Ok(v),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct ObjectMeta {
    pub name: MyStr,
    pub generics: Box<[Generic]>,
}

#[repr(u8)]
#[derive(Debug)]
pub enum RawDataType {
    Int,
    Float,
    Bool,
    Object,
}

impl DataType {
    pub fn toString(&self) -> &str {
        match self {
            Int => "int",
            Float => "float",
            Bool => "bool",
            Object(x) => x.name.as_str(),
            Char => "char",
        }
    }

    pub fn toCString(&self) -> &str {
        match self {
            Int => "long",
            Float => "float",
            Bool => "bool",
            Object(_) => "ViplObject*",
            Char => "char",
        }
    }
}

impl DataType {
    #[inline]
    pub fn toDefaultValue(&self) -> Value {
        match self {
            Int => Num(0),
            Float => Flo(0.),
            Bool => Bol(false),
            Object { .. } => Reference { instance: None },
            Char => Chr(0u8 as char),
        }
    }
}

#[derive(Clone, Debug)]
pub enum JmpType {
    One,
    Zero,
    Jmp,
    Gt,
    Less,
    True,
    False,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct VariableMetadata {
    pub name: MyStr,
    pub typ: DataType,
}

impl From<DataType> for VariableMetadata {
    fn from(value: DataType) -> Self {
        Self {
            name: "unknown".into(),
            typ: value,
        }
    }
}

impl VariableMetadata {
    pub fn f(name: MyStr) -> Self {
        Self { name, typ: Float }
    }

    pub fn i(name: MyStr) -> Self {
        Self { name, typ: Int }
    }

    pub fn c(name: MyStr) -> Self {
        Self { name, typ: Char }
    }

    pub fn b(name: MyStr) -> Self {
        Self { name, typ: Bool }
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum OpCode {
    FunBegin,
    FunName {
        name: MyStr,
    },
    FunReturn {
        typ: Option<DataType>,
    },
    LocalVarTable {
        typ: Box<[VariableMetadata]>,
        argsCount: usize,
    },
    FunEnd,
    F2I,
    I2F,
    PushInt(isize),
    PushFloat(f32),
    PushBool(bool),
    PushChar(char),
    Pop,
    Dup,
    PushLocal {
        index: usize,
    },
    SetLocal {
        index: usize,
        typ: DataType,
    },
    Jmp {
        offset: isize,
        jmpType: JmpType,
    },
    Call {
        encoded: MyStr,
    },
    Return,

    Add(DataType),
    Sub(DataType),
    Div(DataType),
    Mul(DataType),

    Equals(DataType),
    Greater(DataType),
    Less(DataType),

    Or,
    And,
    Not,

    ClassBegin,
    ClassName {
        name: MyStr,
    },
    ClassField {
        name: MyStr,
        typ: DataType,
    },
    ClassEnd,
    New {
        name: MyStr,
    },
    GetField {
        name: MyStr,
        typ: DataType,
    },
    SetField {
        name: MyStr,
        typ: DataType,
    },
    ArrayNew(DataType),
    ArrayStore(DataType),
    ArrayLoad(DataType),
    ArrayLength,
    Inc {
        typ: DataType,
        index: usize,
    },
    Dec {
        typ: DataType,
        index: usize,
    },
    StrNew(MyStr),
    GetChar,
}

#[repr(C)]
#[derive(Debug)]
pub enum RawOpCode {
    FunBegin,
    FunName,
    FunReturn,
    LocalVarTable,
    FunEnd,
    F2I,
    I2F,
    PushInt,
    PushFloat,
    PushBool,
    Pop,
    Dup,
    PushLocal,
    SetLocal,
    Jmp,
    Call,
    Return,
    Add,
    Sub,
    Div,
    Mul,

    Equals,
    Greater,
    Less,

    Or,
    And,
    Not,

    ClassBegin,
    ClassName,
    ClassField,
    ClassEnd,
    New,
    GetField,
    SetField,

    ArrayNew,
    ArrayStore,
    ArrayLoad,
    ArrayLength,
    Inc,
    Dec,
}

pub struct MyObjectField {
    pub typ: DataType,
    pub value: Value,
}

pub enum MyObject {
    ArrayObj {
        values: Vec<Value>,
        typ: DataType,
        size: usize,
    },
    RuntimeObj {
        name: String,
        fields: Option<HashMap<String, MyObjectField>>,
    },
}

#[derive(Clone)]
pub struct MyClassField {
    pub name: String,
    pub typ: DataType,
}

#[derive(Clone)]
pub struct MyClass {
    pub name: String,
    pub fields: HashMap<String, MyClassField>,
}

// TODO maybe null still retains object info
#[derive(Clone, Debug)]
#[repr(C)]
pub enum Value {
    Num(isize),
    Flo(f32),
    Bol(bool),
    Chr(char),
    Reference { instance: Option<Rc<ViplObject>> },
}

impl Value {
    #[inline]
    pub fn tryValueAsFloat(&self) -> Option<f32> {
        match self {
            Num(v) => Some(*v as f32),
            Flo(v) => Some(*v),
            Bol(v) => Some(*v as isize as f32),
            Chr(v) => Some(*v as isize as f32),
            Reference { .. } => None,
        }
    }
}

impl Into<Expression> for Value {
    fn into(self) -> Expression {
        match self {
            Num(it) => Expression::IntLiteral(format!("{}", it)),
            Flo(it) => Expression::FloatLiteral(format!("{}", it)),
            Bol(it) => Expression::BoolLiteral(it),
            Chr(it) => Expression::CharLiteral(it),
            Reference { .. } => panic!(),
        }
    }
}

impl Value {
    #[inline]
    pub fn getString(&self) -> &String {
        match self {
            Reference { instance } => match instance {
                None => panic!(),
                Some(v) => &v.getStr().string,
            },
            e => panic!("{e:?}"),
        }
    }

    #[inline]
    pub fn makeString(str: String) -> Value {
        Reference {
            instance: Some(Rc::new(Str { string: str }.into())),
        }
    }

    #[inline]
    pub fn makeObject(obj: Box<dyn crate::objects::Object>) -> Value {
        Reference {
            instance: Some(Rc::from(ViplObject::Runtime(obj))),
        }
    }

    #[inline]
    pub fn makeArray(arr: Vec<Value>, typ: DataType) -> Value {
        Reference {
            instance: Some(Rc::new(crate::objects::Array { internal: arr, typ }.into())),
        }
    }

    #[inline]
    pub fn valueStr(&self) -> String {
        match self {
            Num(it) => format!("{it}"),
            Flo(it) => format!("{it}"),
            Bol(it) => format!("{it}"),
            Chr(it) => format!("{it}"),
            Reference { instance } => match instance {
                None => String::from("null"),
                Some(v) => match &**v {
                    ViplObject::Arr(a) => {
                        format!("{:?}", a.internal)
                    }
                    ViplObject::Str(v) => {
                        format!("{:?}", v.string)
                    }
                    ViplObject::Runtime(r) => {
                        format!("{:?}", r)
                    }
                },
            },
        }
    }
}

impl Value {
    #[inline]
    pub fn getNum(&self) -> isize {
        match self {
            Num(v) => *v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn getFlo(&self) -> f32 {
        match self {
            Flo(v) => *v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn getRefFlo(&mut self) -> &mut f32 {
        match self {
            Flo(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn getRefNum(&mut self) -> &mut isize {
        match self {
            Num(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn getRefBol(&mut self) -> &mut bool {
        match self {
            Bol(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn getBool(&self) -> bool {
        match self {
            Bol(v) => *v,
            _ => panic!(),
        }
    }

    // FIXME c is sending some junk data fix ffi
    #[inline]
    pub fn getReference(&self) -> &Option<Rc<ViplObject>> {
        match self {
            Reference { instance } => instance,
            v => panic!("{:?}", v),
        }
    }

    #[inline]
    pub fn getMutReference(&mut self) -> &mut Option<Rc<ViplObject>> {
        match self {
            Reference { instance } => instance,
            v => panic!("{:?}", v),
        }
    }

    #[inline]
    pub fn getReferenceValue(self) -> Option<Rc<ViplObject>> {
        match self {
            Reference { instance } => instance,
            _ => panic!(),
        }
    }
}

impl Value {
    #[inline]
    pub fn or(&mut self, val: &Value) {
        let r = self.getRefBol();
        *r = *r || val.getBool();
    }

    #[inline]
    pub fn and(&mut self, val: &Value) {
        let r = self.getRefBol();
        *r = *r && val.getBool();
    }

    #[inline]
    pub fn not(&mut self) {
        let r = self.getRefBol();
        *r = !*r;
    }
}

impl Value {
    #[inline]
    pub fn gt(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNum() > val.getNum(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
        }
    }

    #[inline]
    pub fn inc(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() += 1;
            }
            Float => {
                *self.getRefFlo() += 1.;
            }
            _ => panic!(),
        }
    }

    #[inline]
    pub fn dec(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() -= 1;
            }
            Float => {
                *self.getRefFlo() -= 1.;
            }
            _ => panic!(),
        }
    }

    #[inline]
    pub fn less(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNum() < val.getNum(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
        }
    }

    #[inline]
    pub fn refLess(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNum() > val.getNum(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
        };

        *self = Bol(l)
    }

    #[inline]
    pub fn refGt(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNum() < val.getNum(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
        };

        *self = Bol(l)
    }

    #[inline]
    pub fn eq(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNum() == val.getNum(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object { .. } => panic!(),
        }
    }

    #[inline]
    pub fn refEq(&mut self, val: &Value, typ: &DataType) {
        let x = match typ {
            Int => self.getNum() == val.getNum(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object(a) => panic!("{:?}", a),
        };
        *self = Bol(x)
    }

    #[inline]
    pub fn getChar(&self) -> char {
        match self {
            Chr(c) => *c,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn toDataType(&self) -> DataType {
        match self {
            Num(_) => Int,
            Flo(_) => Float,
            Bol(_) => Bool,
            Chr(_) => Char,
            Reference { instance: v } => match v {
                None => panic!(),
                Some(v) => Object(ObjectMeta {
                    name: MyStr::Runtime(v.asObj().getName().into_boxed_str()),
                    generics: Box::new([]),
                }),
            },
        }
    }
}

#[repr(C)]
struct RcBox<T: ?Sized> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    value: T,
}

impl Value {
    #[inline]
    pub fn add(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() += value.getNum();
            }
            Float => {
                *self.getRefFlo() += value.getFlo();
            }
            Bool => {}
            Object(it) => {
                match it.name.as_str() {
                    "String" => {
                        match self {
                            Reference { instance } => {
                                match instance {
                                    None => panic!(),
                                    Some(v) => {
                                        let str1 = &v.getStr().string;
                                        let str2 = value.getString();

                                        let mut buf = String::with_capacity(str1.len() + str2.len());

                                        buf.push_str(str1);
                                        buf.push_str(str2);

                                        *v = Rc::new(ViplObject::Str(Str { string: buf }))
                                    }
                                }
                            }
                            _ => panic!()
                        }
                    }
                    _ => panic!()
                }
            }
            Char => panic!(),
        }
    }

    #[inline]
    pub fn sub(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() -= value.getNum();
            }
            Float => {
                *self.getRefFlo() -= value.getFlo();
            }
            Bool => {}
            Object { .. } => {}
            Char => panic!(),
        }
    }

    #[inline]
    pub fn mul(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() *= value.getNum();
            }
            Float => {
                *self.getRefFlo() *= value.getFlo();
            }
            Bool => {}
            Object { .. } => {}
            Char => panic!(),
        }
    }

    #[inline]
    pub fn div(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() /= value.getNum();
            }
            Float => {
                *self.getRefFlo() /= value.getFlo();
            }
            Bool => {}
            Object { .. } => {}
            Char => panic!(),
        }
    }

    #[inline]
    pub fn f2i(&mut self) -> Value {
        Num(self.getFlo() as isize)
    }

    #[inline]
    pub fn i2f(&mut self) -> Value {
        Flo(self.getNum() as f32)
    }
}

impl Value {
    #[inline]
    pub fn isType(&self, typ: &DataType) -> bool {
        match self {
            Num(_) => {
                matches!(typ, Int)
            }
            Flo(_) => {
                matches!(typ, Float)
            }
            Bol(_) => {
                matches!(typ, Bool)
            }
            Reference { .. } => panic!(),
            Chr(_) => matches!(typ, Char),
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct StackFrame<'a> {
    pub localVariables: &'a mut [Value],
    pub name: Option<&'a str>,
    pub objects: Option<Vec<Rc<ViplObject>>>,
}

impl Drop for StackFrame<'_> {
    fn drop(&mut self) {
        if let Some(objects) = &self.objects {
            for o in objects {
                let raw = Rc::into_raw(o.clone());
                unsafe {
                    Rc::decrement_strong_count(raw);
                    Rc::decrement_strong_count(raw);
                }
            }
        }
    }
}

impl StackFrame<'_> {
    #[inline]
    pub fn addObject(&mut self, obj: Rc<ViplObject>) {
        match &mut self.objects {
            None => panic!(),
            Some(v) => v.push(obj)
        }
    }
}

impl StackFrame<'_> {
    pub fn new(localVariables: &mut [Value]) -> StackFrame {
        StackFrame {
            localVariables,
            name: Option::from("root"),
            objects: None,
        }
    }
}

pub fn parseDataTypeFromStr(s: &str) {
    // Array<Array<Array<int>>>
    // Array<Array<Array
    panic!()
}

pub fn decodeFunctionString(s: &str) {
    panic!()
}

impl VirtualMachine {
    pub unsafe fn loadNative(
        &mut self,
        path: &str,
        name: &str,
        returnType: Option<DataType>,
        args: Box<[VariableMetadata]>,
    ) {
        self.loadRawNative(path, name, returnType, args.len())
    }

    pub unsafe fn loadRawNative(
        &mut self,
        path: &str,
        name: &str,
        returnType: Option<DataType>,
        argCount: usize,
    ) {
        let l = Library::new(path).unwrap();

        self.nativeLibraries.push(l);
        let lib = self.nativeLibraries.last().unwrap();

        let a: Symbol<extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> ()> =
            lib.get(b"call\0").unwrap();

        self.functions.insert(
            MyStr::from(name.to_owned().into_boxed_str()),
            Func {
                name: name.to_owned(),
                returnType,
                argAmount: argCount,
                varTable: vec![VariableMetadata::b(MyStr::Static("")); argCount].into_boxed_slice(),
                typ: Extern {
                    callback: *a.into_raw(),
                },
            },
        );
    }
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub returnType: Option<DataType>,
    pub varTable: Box<[VariableMetadata]>,
    pub argAmount: usize,
    pub typ: FuncType,
}

#[derive(Clone)]
pub enum FuncType {
    Runtime {
        rangeStart: usize,
        rangeStop: usize,
    },
    Native {
        callback: fn(&mut VirtualMachine, &mut StackFrame) -> (),
    },
    Extern {
        callback: extern fn(&mut VirtualMachine, &mut StackFrame) -> (),
    },
}

impl Debug for FuncType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Runtime { rangeStart, rangeStop } => {
                write!(f, "runtime func {:?}", rangeStart)
            }
            Native { callback } => {
                write!(f, "builtin func {:?}", *callback as *const ())
            }
            Extern { callback } => {
                write!(f, "extern func {:?}", *callback as *const ())
            }
        }
    }
}

#[derive(Debug)]
pub enum CachedOpCode {
    CallCache {
        locals: Box<[Value]>,
        typ: FuncType,
        argCount: usize,
    },
}

#[repr(C)]
#[derive(Debug)]
pub struct VirtualMachine {
    pub nativeWrapper: NativeWrapper,
    pub functions: HashMap<MyStr, Func>,
    pub classes: HashMap<MyStr, ObjectDefinition>,
    pub stack: Vec<Value>,
    pub opCodes: Vec<OpCode>,
    pub opCodeCache: Vec<Option<CachedOpCode>>,
    pub nativeLibraries: Vec<Library>
}

impl VirtualMachine {
    #[inline]
    pub fn call(&mut self, name: MyStr) {
        let f = self.functions.get(&name).unwrap();
        let mut locals = vec![Num(-1); f.varTable.len()];

        for i in 0..(f.argAmount) {
            let arg = self.stack.pop().unwrap();
            locals[(f.argAmount - 1) - i] = arg;
        }

        let mut stack = StackFrame {
            localVariables: &mut locals,
            name: None,
            objects: None,
        };

        let t = f.typ.clone();

        // FIXME this is so much cursed
        // FIXME i am bypassing all rust safety guaranties :)

        let ptr = self as *mut VirtualMachine;

        match t {
            Runtime {
                rangeStart: s,
                rangeStop: _e,
            } => unsafe {
                let mut seekable = SeekableOpcodes {
                    index: s as isize,
                    opCodes: &mut (*ptr).opCodes,
                };
                run(&mut seekable, &mut *ptr, &mut stack);
            },
            Native { callback } => callback(self, &mut stack),
            Extern { callback } => {
                stack.objects = Some(vec![]);
                callback(self, &mut stack);
            },
        }
    }
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
            stack: vec![],
            classes: Default::default(),
            opCodes: vec![],
            opCodeCache: vec![],
            nativeWrapper: NativeWrapper::new(),
            nativeLibraries: vec![],
        }
    }

    pub fn makeNative(
        &mut self,
        name: String,
        args: Box<[VariableMetadata]>,
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> (),
        ret: Option<DataType>,
    ) {
        let genName = genFunNameMeta(&name, &args, args.len());
        let l = args.len();
        self.functions.insert(
            MyStr::Runtime(genName.into_boxed_str()),
            Func {
                name,
                returnType: ret,
                varTable: args,
                argAmount: l,
                typ: Native { callback: fun },
            },
        );
    }

    pub fn makeExtern(
        &mut self,
        name: String,
        args: Box<[VariableMetadata]>,
        fun: extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> (),
        ret: Option<DataType>,
    ) {
        let genName = genFunNameMeta(&name, &args, args.len());
        let l = args.len();
        self.functions.insert(
            MyStr::Runtime(genName.into_boxed_str()),
            Func {
                name,
                returnType: ret,
                varTable: args,
                argAmount: l,
                typ: Extern { callback: fun },
            },
        );
    }

    pub fn makeRuntime(
        &mut self,
        name: String,
        args: Box<[VariableMetadata]>,
        begin: usize,
        argsCount: usize,
        ret: Option<DataType>,
        end: usize,
    ) {
        let genName = genFunNameMeta(&name, &args, argsCount);

        let fun = Func {
            name,
            returnType: ret,
            varTable: args,
            argAmount: argsCount,
            typ: Runtime {
                rangeStart: begin,
                rangeStop: end,
            },
        };

        self.functions
            .insert(MyStr::Runtime(genName.into_boxed_str()), fun);
    }
}

pub struct SeekableOpcodes<'a> {
    pub index: isize,
    pub opCodes: &'a mut [OpCode],
}

impl SeekableOpcodes<'_> {
    #[inline]
    pub fn seek(&mut self, offset: isize) {
        // FIXME boundary check
        self.index += offset;
    }

    #[inline]
    pub fn nextOpcode(&mut self) -> (Option<&mut OpCode>, usize) {
        let n = self.opCodes.get_mut(self.index as usize);
        let i = self.index;
        self.index += 1;

        (n, i as usize)
    }

    #[inline]
    pub fn getOpcode(&self, index: usize) -> Option<&OpCode> {
        self.opCodes.get(index)
    }
}

#[inline]
pub fn argsToString(args: &[DataType]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn argsToStringMeta(args: &[VariableMetadata]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.typ.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn genFunName(name: &str, args: &[DataType]) -> String {
    format!("{}({})", name, argsToString(args))
}

#[inline]
pub fn genFunNameMeta(name: &str, args: &[VariableMetadata], argsLen: usize) -> String {
    format!("{}({})", name, argsToStringMeta(&args[0..argsLen]))
}

#[inline]
pub fn run(opCodes: &mut SeekableOpcodes, vm: &mut VirtualMachine, stackFrame: &mut StackFrame) {
    loop {
        let (op, index) = match opCodes.nextOpcode() {
            (None, _) => {
                return;
            }
            (Some(v), i) => (v, i),
        };
        // println!("evaluating {:?}", op);
        match op {
            FunBegin => {
                let mut index = opCodes.index as usize;
                let name = match opCodes.getOpcode(index).unwrap() {
                    FunName { name } => name,
                    v => panic!("{v:?}"),
                };
                index += 1;
                let (vars, argCount) = match opCodes.getOpcode(index).unwrap() {
                    LocalVarTable { typ, argsCount } => (typ, argsCount),
                    v => panic!("{v:?}"),
                };
                index += 1;
                let ret = match opCodes.getOpcode(index).unwrap() {
                    FunReturn { typ } => typ,
                    v => panic!("{v:?}"),
                };
                index += 1;
                let startIndex = index;

                'a: loop {
                    let peek = opCodes.getOpcode(index).unwrap();
                    // println!("eee {:?}", peek);
                    match peek {
                        FunEnd => {
                            index += 1;
                            break 'a;
                        }
                        _ => {
                            index += 1;
                        }
                    }
                }

                vm.makeRuntime(
                    name.to_string(),
                    vars.clone(),
                    startIndex,
                    *argCount,
                    ret.clone(),
                    0,
                );
                opCodes.index = index as isize;
            }
            F2I => {
                let mut x = vm.stack.pop().unwrap();
                vm.stack.push(x.f2i())
            }
            I2F => {
                let mut x = vm.stack.pop().unwrap();
                vm.stack.push(x.i2f())
            }
            PushInt(v) => vm.stack.push(Num(*v)),
            PushFloat(v) => vm.stack.push(Flo(*v)),
            PushBool(v) => vm.stack.push(Bol(*v)),
            Pop => {
                vm.stack.pop();
            }
            Dup => unsafe {
                let val = vm.stack.get_unchecked(vm.stack.len() - 1);
                vm.stack.push(val.clone());
            },
            PushLocal { index } => {
                // println!("{:?}", stackFrame.localVariables.get(*index));
                // println!("loclas size {}", stackFrame.localVariables.len());
                vm.stack
                    .push(unsafe { stackFrame.localVariables.get_unchecked(*index) }.clone())
            }
            SetLocal { index, typ: _ } => {
                let x = vm.stack.pop().unwrap();
                *unsafe { stackFrame.localVariables.get_unchecked_mut(*index) } = x;
                // println!("{:?}", stackFrame.localVariables.get(*index));
                // stackFrame.get_mut().localVariables.insert(*index, x);
            }
            Jmp { offset, jmpType } => match jmpType {
                JmpType::One => {
                    vm.stack.pop().unwrap();
                    let _b = vm.stack.pop().unwrap();
                    panic!()
                }
                JmpType::Zero => {}
                JmpType::Jmp => {
                    let x = *offset;
                    opCodes.seek(x);
                }
                JmpType::Gt => {
                    let a = vm.stack.pop().unwrap();
                    let b = vm.stack.pop().unwrap();
                    if a.gt(&b, &DataType::Float) {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
                JmpType::Less => {
                    let a = vm.stack.pop().unwrap();
                    let b = vm.stack.pop().unwrap();
                    if a.less(&b, &DataType::Float) {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
                JmpType::True => {
                    let a = vm.stack.pop().unwrap();
                    if a.getBool() {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
                JmpType::False => {
                    let a = vm.stack.pop().unwrap();
                    if !a.getBool() {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
            },
            Call { encoded } => unsafe {
                // println!("function call {}", encoded);
                // println!("stack size {}", vm.stack.len());
                let cached = match &vm.opCodeCache.get_unchecked(index) {
                    Some(v) => match v {
                        CachedOpCode::CallCache {
                            locals: stack,
                            typ,
                            argCount,
                        } => (stack, typ, argCount),
                    },
                    None => {
                        // println!("{:?}", &vm.functions.keys());
                        // println!("{}", encoded);
                        let f = vm.functions.get(&encoded).expect(&format!("function {} not found", &encoded));
                        // println!("meta {:?}", f.varTable);
                        let localVars = vec![Value::Num(-1); f.varTable.len()]; // Vec::with_capacity(f.varTable.len());

                        /*
                        for i in &*f.varTable {
                            localVars.push(i.typ.toDefaultValue())
                        }
                        */

                        vm.opCodeCache[index] = Some(CachedOpCode::CallCache {
                            locals: localVars.into(),
                            typ: f.typ.clone(),
                            argCount: f.argAmount,
                        });
                        match vm.opCodeCache.get_unchecked(index) {
                            None => panic!(),
                            Some(v) => match v {
                                CachedOpCode::CallCache {
                                    locals: ref stack,
                                    ref typ,
                                    ref argCount,
                                } => (stack, typ, argCount),
                            },
                        }
                    }
                };

                let mut cahedLocals = cached.0.clone();

                // println!("{}", encoded);
                for i in 0..(*cached.2) {
                    let arg = vm.stack.pop().unwrap();
                    cahedLocals[(cached.2 - 1) - i] = arg;
                }

                let mut stack = StackFrame {
                    localVariables: &mut cahedLocals,
                    name: None,
                    objects: None,
                };

                match cached.1 {
                    Runtime {
                        rangeStart: s,
                        rangeStop: _e,
                    } => {
                        let old = index + 1;

                        opCodes.index = *s as isize;
                        run(opCodes, vm, &mut stack);
                        opCodes.index = old as isize;
                    }
                    Native { callback } => callback(vm, &mut stack),
                    Extern { callback } => {
                        stack.objects = Some(vec![]);
                        callback(vm, &mut stack);
                    }
                }
            },
            Return => return,
            Add(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).add(&a, v);
            },
            Sub(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).sub(&a, v);
            },
            Div(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).div(&a, v);
            },
            Mul(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                // println!("aaa {:?}", vm.stack.get(l));
                vm.stack.get_unchecked_mut(l).mul(&a, v);
            },
            Equals(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).refEq(&a, v);
            },
            Greater(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).refGt(&a, &v);
            },
            Less(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).refLess(&a, v);
            },
            Or => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).or(&a);
            },
            And => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).and(&a);
            },
            Not => unsafe {
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).not();
            },
            ArrayNew(d) => {
                // println!("{}", vm.stack.len());
                let _size = vm.stack.pop().unwrap();
                vm.stack.push(Value::Reference {
                    instance: Some(Rc::new(
                        crate::objects::Array {
                            internal: vec![],
                            typ: d.clone(),
                        }
                            .into(),
                    )),
                })
            }
            ArrayStore(_) => {
                let index = vm.stack.pop().unwrap().getNum();
                let val = vm.stack.pop().unwrap();
                match vm.stack.pop().unwrap() {
                    Reference { instance } => unsafe {
                        let mut clon = instance.unwrap();
                        let ne = Rc::get_mut_unchecked(&mut clon);
                        let v = ne.getMutArr();
                        if index as usize == v.internal.len() {
                            v.internal.push(val)
                        } else {
                            v.internal[index as usize] = val
                        }
                    },
                    _ => panic!(),
                };
            }
            ArrayLoad(_) => {
                let index = vm.stack.pop().unwrap().getNum();
                match vm.stack.pop().unwrap() {
                    Reference { instance } => {
                        let clon = instance.unwrap();
                        let e = clon.getArr();
                        vm.stack.push(e.internal.get(index as usize).unwrap().clone())
                    },
                    _ => panic!(),
                };
            }
            ArrayLength => match vm.stack.pop().unwrap().getReference() {
                None => {}
                Some(v) => {
                    vm.stack.push(Num(v.getArr().internal.len() as isize));
                }
            },
            Inc { typ, index } => unsafe {
                stackFrame.localVariables.get_unchecked_mut(*index).inc(typ)
            },
            Dec { typ, index } => unsafe {
                stackFrame.localVariables.get_unchecked_mut(*index).dec(typ)
            },
            PushChar(c) => vm.stack.push(Chr(*c)),
            StrNew(s) => vm.stack.push(Value::makeString(s.clone().to_string())),
            GetChar => {
                let index = vm.stack.pop().unwrap().getNum();

                let opIndex = vm.stack.len()-1;

                let r = vm.stack.get_mut(opIndex).unwrap();
                let c = *r.getString().as_bytes().get(index as usize).unwrap() as char;
                *r = Chr(c);
            }
            o => panic!("unimplemented opcode {:?}", o)
        }
    }
}

impl VirtualMachine {
    pub fn eval(&mut self, mut bytecode: Vec<OpCode>, locals: Vec<DataType>) {
        let mut vals = vec![];
        for b in &locals {
            vals.push(b.toDefaultValue())
        }
        for _ in &bytecode {
            self.opCodeCache.push(None);
        }
        run(
            &mut SeekableOpcodes {
                index: 0,
                opCodes: &mut bytecode,
            },
            self,
            &mut StackFrame::new(&mut vals),
        );
    }
}

impl Drop for VirtualMachine {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}

pub fn evaluateBytecode(mut bytecode: Vec<OpCode>, locals: Vec<DataType>) -> VirtualMachine {
    let mut vals = vec![];
    for b in &locals {
        vals.push(b.toDefaultValue())
    }
    let mut vm = bootStrapVM();
    for _ in &bytecode {
        vm.opCodeCache.push(None);
    }
    run(
        &mut SeekableOpcodes {
            index: 0,
            opCodes: &mut bytecode,
        },
        &mut vm,
        &mut StackFrame::new(&mut vals),
    );

    vm
}

pub fn evaluateBytecode2(
    mut bytecode: Vec<OpCode>,
    locals: Vec<DataType>,
    vm: &mut VirtualMachine,
) {
    let mut vals = vec![];
    for b in &locals {
        vals.push(b.toDefaultValue())
    }
    for _ in &bytecode {
        vm.opCodeCache.push(None);
    }
    run(
        &mut SeekableOpcodes {
            index: 0,
            opCodes: &mut bytecode,
        },
        vm,
        &mut StackFrame::new(&mut vals),
    );
}
