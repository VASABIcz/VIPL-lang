#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]

use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::fmt::Debug;
use std::intrinsics::transmute;
use std::ops::Deref;
use std::pin::Pin;
use std::rc::Rc;

use crate::lexer::TokenType::Var;
use crate::objects::{ObjectDefinition, Str};
use crate::std::bootStrapVM;
use crate::vm::DataType::*;
use crate::vm::FuncType::*;
use crate::vm::JmpType::True;
use crate::vm::OpCode::*;
use crate::vm::Value::*;

#[derive(Clone, Debug, PartialEq)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Array { inner: Box<DataType> },
    Object(Box<ObjectMeta>),
}

impl DataType {
    pub fn Str() -> Self {
        Object(
            Box::new(ObjectMeta { name: "String".to_string().into_boxed_str(), generics: Box::new([]) })
        )
    }
    pub fn Arr(inner: Generic) -> Self {
        Object(
            Box::new(ObjectMeta { name: "Array".to_string().into_boxed_str(), generics: Box::new([inner]) })
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Generic {
    Any,
    Type(DataType),
}

impl Generic {
    pub fn ok_or<E>(self, err: E) -> Result<DataType, E> {
        match self {
            Generic::Any => Err(err),
            Generic::Type(v) => Ok(v)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectMeta {
    pub name: Box<str>,
    pub generics: Box<[Generic]>,
}

impl DataType {
    /*
    pub fn fromString(s: &str) -> Self {
        if s.ends_with("[]") {
            return DataType::Array {
                inner: Box::new(DataType::fromString(s.strip_suffix("[]").unwrap())),
            };
        }

        match s {
            "int" => DataType::Int,
            "float" => DataType::Float,
            "bool" => DataType::Bool,
            _ => DataType::Object(Box::new(ObjectMeta { name: s.to_string(), generics: Box::new([]) })),
        }
    }

     */
}

#[repr(u8)]
#[derive(Debug)]
pub enum RawDataType {
    Int,
    Float,
    Bool,
    Array,
    Object,
}

impl DataType {
    pub fn toBytes(&self, bytes: &mut Vec<u8>) {
        let opId: [u8; 16] = unsafe { transmute((*self).clone()) };
        bytes.push(opId[0]);
        match self {
            Int => {}
            Float => {}
            Bool => {}
            Array { inner } => inner.toBytes(bytes),
            Object(x) => {
                let bs = x.name.escape_default().to_string();
                bytes.extend(bs.len().to_ne_bytes());
                bytes.extend(bs.as_bytes())
            }
            Char => {}
        }
    }
}

impl DataType {
    fn toString(&self) -> &str {
        match self {
            Int => "int",
            Float => "float",
            Bool => "bool",
            Array { inner: _ } => "",
            Object(x) => &x.name,
            Char => "char"
        }
    }
}

impl DataType {
    pub fn toDefaultValue(&self) -> Value {
        match self {
            Int => Num(0),
            Float => Flo(0.),
            Bool => Bol(false),
            Array { .. } => panic!(),
            Object { .. } => Value::Reference { instance: None },
            Char => Chr('a')
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
    False
}

impl JmpType {
    pub fn toBytes(&self, bytes: &mut Vec<u8>) {
        let opId: [u8; 1] = unsafe { std::mem::transmute((*self).clone()) };
        bytes.push(opId[0]);
    }
}

#[derive(Clone, Debug)]
pub struct VariableMetadata {
    pub name: Box<str>,
    pub typ: DataType,
}

impl VariableMetadata {
    pub fn f(name: String) -> Self {
        Self {
            name: name.into_boxed_str(),
            typ: Float,
        }
    }

    pub fn i(name: String) -> Self {
        Self {
            name: name.into_boxed_str(),
            typ: Int,
        }
    }
}

impl VariableMetadata {
    pub fn toBytes(&self, bytes: &mut Vec<u8>) {
        let bs = self.name.escape_default().to_string();
        bytes.extend(bs.len().to_ne_bytes());
        bytes.extend(bs.as_bytes());
        self.typ.toBytes(bytes);
    }
}

#[derive(Clone, Debug)]
pub enum OpCode {
    FunBegin,
    FunName {
        name: Box<str>,
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
        encoded: Box<str>,
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
        name: Box<str>,
    },
    ClassField {
        name: Box<str>,
        typ: DataType,
    },
    ClassEnd,
    New {
        name: Box<str>,
    },
    GetField {
        name: Box<str>,
        typ: DataType,
    },
    SetField {
        name: Box<str>,
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
}

#[repr(u8)]
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

#[derive(Clone, Debug)]
pub enum Value {
    Num(isize),
    Flo(f32),
    Bol(bool),
    Chr(char),
    Reference {
        instance: Option<Rc<dyn crate::objects::Object>>,
    },
}

impl Value {
    pub fn getString(&self) -> String {
        match self {
            Reference { instance } => {
                match instance {
                    None => panic!(),
                    Some(v) => unsafe {
                        match Rc::get_mut_unchecked(&mut v.clone()).downcast_mut::<Str>() {
                            None => panic!(),
                            Some(v) => {
                                v.string.clone()
                            }
                        }
                    }
                }
            }
            e => panic!("{:?}", e)
        }
    }

    pub fn makeString(str: String) -> Value {
        Value::Reference { instance: Some(Rc::new(Str { string: str })) }
    }

    pub fn makeObject(obj: Box<dyn crate::objects::Object>) -> Value {
        Value::Reference { instance: Some(Rc::from(obj)) }
    }

    pub fn makeArray(arr: Vec<Value>, typ: DataType) -> Value {
        Value::Reference { instance: Some(Rc::new(crate::objects::Array { internal: arr, typ })) }
    }

    pub fn valueStr(&self) -> String {
        match self {
            Num(it) => format!("{}", it),
            Flo(it) => format!("{}", it),
            Bol(it) => format!("{}", it),
            Chr(it) => format!("{}", it),
            Reference { instance } => {
                match instance {
                    None => String::from("null"),
                    Some(v) => unsafe {
                        let mut clon = v.clone();
                        let res = Rc::get_mut_unchecked(&mut clon);
                        let e = res.downcast_mut_unchecked::<Str>();
                        format!("{:?}", e)
                    }
                }
            }
        }
    }
}

impl Value {
    #[inline(always)]
    pub fn getNum(&self) -> isize {
        match self {
            Num(v) => *v,
            Flo(_) => panic!(),
            Bol(_) => panic!(),
            Reference { .. } => panic!(),
            Chr(_) => panic!()
        }
    }

    #[inline(always)]
    pub fn getFlo(&self) -> f32 {
        match self {
            Num(_) => panic!(),
            Flo(v) => *v,
            Bol(_) => panic!(),
            Reference { .. } => panic!(),
            Chr(_) => panic!()
        }
    }

    #[inline(always)]
    pub fn getRefFlo(&mut self) -> &mut f32 {
        match self {
            Num(_) => panic!(),
            Flo(v) => v,
            Bol(_) => panic!(),
            Reference { .. } => panic!(),
            Chr(_) => panic!()
        }
    }

    #[inline(always)]
    pub fn getRefNum(&mut self) -> &mut isize {
        match self {
            Num(v) => v,
            Flo(_) => panic!(),
            Bol(_) => panic!(),
            Reference { .. } => panic!(),
            Chr(_) => panic!()
        }
    }

    #[inline(always)]
    pub fn getRefBol(&mut self) -> &mut bool {
        match self {
            Num(_) => panic!(),
            Flo(_) => panic!(),
            Bol(v) => v,
            Reference { .. } => panic!(),
            Chr(_) => panic!()
        }
    }

    #[inline(always)]
    pub fn getBool(&self) -> bool {
        match self {
            Num(_) => panic!(),
            Flo(_) => panic!(),
            Bol(v) => *v,
            Reference { .. } => panic!(),
            Chr(_) => panic!()
        }
    }
}

impl Value {
    #[inline(always)]
    pub fn or(&mut self, val: &Value) {
        let r = self.getRefBol();
        *r = *r || val.getBool();
    }

    #[inline(always)]
    pub fn and(&mut self, val: &Value) {
        let r = self.getRefBol();
        *r = *r && val.getBool();
    }

    #[inline(always)]
    pub fn not(&mut self) {
        let r = self.getRefBol();
        *r = !*r;
    }
}

impl Value {
    #[inline(always)]
    pub fn gt(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNum() > val.getNum(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn inc(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() += 1;
            }
            Float => {
                *self.getRefFlo() += 1.;
            }
            Bool => panic!(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn dec(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() -= 1;
            }
            Float => {
                *self.getRefFlo() -= 1.;
            }
            Bool => panic!(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn less(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNum() < val.getNum(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn refLess(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNum() > val.getNum(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        };

        *self = Bol(l)
    }

    #[inline(always)]
    pub fn refGt(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNum() < val.getNum(),
            Float => self.getFlo() < val.getFlo(),
            Bool => self.getBool() < val.getBool(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        };

        *self = Bol(l)
    }

    #[inline(always)]
    pub fn eq(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNum() == val.getNum(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn refEq(&mut self, val: &Value, typ: &DataType) {
        let x = match typ {
            Int => self.getNum() == val.getNum(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
            Char => self.getChar() == val.getChar()
        };
        *self = Bol(x)
    }

    pub fn getChar(&self) -> char {
        match self {
            Chr(c) => *c,
            _ => panic!()
        }
    }

    pub fn toDataType(&self) -> DataType {
        match self {
            Num(_) => DataType::Int,
            Flo(_) => DataType::Float,
            Bol(_) => DataType::Bool,
            Chr(_) => DataType::Char,
            Reference { instance } => DataType::Object(Box::new(ObjectMeta { name: "".to_string().into_boxed_str(), generics: Box::new([]) }))
        }
    }
}

impl Value {
    #[inline(always)]
    pub fn add(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() += value.getNum();
            }
            Float => {
                *self.getRefFlo() += value.getFlo();
            }
            Bool => {}
            Array { .. } => {}
            Object(it) => {
                if &*it.name == "String" {
                    let mut buf = String::new();

                    match self {
                        Reference { instance } => {
                            match instance {
                                None => {
                                    panic!()
                                }
                                Some(v) => unsafe {
                                    let da = Rc::get_mut_unchecked(v);
                                    match da.downcast_mut::<Str>() {
                                        None => panic!(),
                                        Some(ev) => {
                                            match value {
                                                Reference { instance } => {
                                                    match instance {
                                                        None => {
                                                            panic!()
                                                        }
                                                        Some(va) => {
                                                            let mut clon = va.clone();
                                                            let ce = Rc::get_mut_unchecked(&mut clon);
                                                            match ce.downcast_mut::<Str>() {
                                                                None => panic!(),
                                                                Some(ve) => {
                                                                    buf.push_str(&ev.string);
                                                                    buf.push_str(&ve.string);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => panic!()
                                            }
                                        }
                                    }
                                    *v = Rc::new(Str { string: buf })
                                }
                            }
                        }
                        _ => panic!()
                    }
                }
            }
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn sub(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() -= value.getNum();
            }
            Float => {
                *self.getRefFlo() -= value.getFlo();
            }
            Bool => {}
            Array { .. } => {}
            Object { .. } => {}
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn mul(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() *= value.getNum();
            }
            Float => {
                *self.getRefFlo() *= value.getFlo();
            }
            Bool => {}
            Array { .. } => {}
            Object { .. } => {}
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn div(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() /= value.getNum();
            }
            Float => {
                *self.getRefFlo() /= value.getFlo();
            }
            Bool => {}
            Array { .. } => {}
            Object { .. } => {}
            Char => panic!()
        }
    }

    #[inline(always)]
    pub fn f2i(&mut self) -> Value {
        Num(self.getFlo() as isize)
    }

    #[inline(always)]
    pub fn i2f(&mut self) -> Value {
        Flo(self.getNum() as f32)
    }
}

impl Value {
    #[inline(always)]
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
            Chr(_) => matches!(typ, Char)
        }
    }
}

#[derive(Debug)]
pub struct StackFrame<'a> {
    pub previous: Option<&'a StackFrame<'a>>,
    pub localVariables: &'a mut [Value],
    pub name: Option<&'a str>,
}

impl StackFrame<'_> {
    pub fn new(localVariables: &mut [Value]) -> StackFrame {
        StackFrame {
            previous: None,
            localVariables,
            name: Option::from("root"),
        }
    }
}

#[derive(Clone)]
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
}

pub enum CachedOpCode {
    CallCache {
        stack: Vec<Value>,
        typ: FuncType,
        argCount: usize,
    },
}

pub struct VirtualMachine {
    pub functions: HashMap<Box<str>, Func>,
    pub stack: Vec<Value>,
    pub classes: HashMap<Box<str>, ObjectDefinition>,
    pub opCodes: Vec<OpCode>,
    pub opCodeCache: Vec<Option<CachedOpCode>>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
            stack: vec![],
            classes: Default::default(),
            opCodes: vec![],
            opCodeCache: vec![],
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
            genName.into_boxed_str(),
            Func {
                name,
                returnType: ret,
                varTable: args,
                argAmount: l,
                typ: Native { callback: fun },
            },
        );
    }

    pub fn makeRuntime(&mut self, name: String, args: Box<[VariableMetadata]>, begin: usize, argsCount: usize, ret: Option<DataType>, end: usize) {
        let genName = genFunNameMeta(&name, &args, argsCount);

        let fun = Func {
            name,
            returnType: ret,
            varTable: args,
            argAmount: argsCount,
            typ: Runtime { rangeStart: begin, rangeStop: end }
        };

        self.functions.insert(genName.into_boxed_str(), fun);
    }
}

pub struct SeekableOpcodes<'a> {
    pub index: isize,
    pub opCodes: &'a [OpCode],
    pub start: Option<usize>,
    pub end: Option<usize>,
}

impl SeekableOpcodes<'_> {
    #[inline(always)]
    pub fn seek(&mut self, offset: isize) {
        // FIXME boundary check
        self.index += offset;
    }

    #[inline(always)]
    pub fn nextOpcode(&mut self) -> (Option<&OpCode>, usize) {
        let n = self.opCodes.get(self.index as usize);
        self.index += 1;

        (n, (self.index - 1) as usize)
    }

    #[inline(always)]
    pub fn getOpcode(&self, index: usize) -> Option<&OpCode> {
        self.opCodes.get(index)
    }
}

#[inline(always)]
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

#[inline(always)]
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

#[inline(always)]
pub fn genFunName(name: &str, args: &[DataType]) -> String {
    format!("{}({})", name, argsToString(args))
}

#[inline(always)]
pub fn genFunNameMeta(name: &str, args: &[VariableMetadata], argsLen: usize) -> String {
    format!("{}({})", name, argsToStringMeta(&args[0..argsLen]))
}

#[inline(always)]
pub fn run<'a>(opCodes: &mut SeekableOpcodes, vm: &mut VirtualMachine, stackFrame: &mut StackFrame) {
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
                    v => panic!("{:?}", v)
                };
                index += 1;
                let (vars, argCount) = match opCodes.getOpcode(index).unwrap() {
                    LocalVarTable { typ, argsCount } => (typ, argsCount),
                    v => panic!("{:?}", v)
                };
                index += 1;
                let ret = match opCodes.getOpcode(index).unwrap() {
                    FunReturn { typ } => typ,
                    v => panic!("{:?}", v)
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
                        },
                        _ => {
                            index += 1;
                        }
                    }
                }

                vm.makeRuntime(name.clone().into_string(), vars.clone(), startIndex, *argCount, ret.clone(), 0);
                opCodes.index = index as isize;
            },
            FunName { .. } => panic!(),
            FunReturn { .. } => panic!(),
            LocalVarTable { .. } => panic!(),
            FunEnd => panic!(),
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
            Dup => {
                let x = vm.stack.pop().unwrap();
                vm.stack.push(x.clone());
                vm.stack.push(x);
            }
            PushLocal { index } => {
                // println!("{:?}", stackFrame.localVariables.get(*index));
                // println!("loclas size {}", stackFrame.localVariables.len());
                vm.stack.push(unsafe { stackFrame.localVariables.get_unchecked(*index) }.clone())
            },
            SetLocal { index, typ: _ } => {
                let x = vm.stack.pop().unwrap();
                *unsafe{ (stackFrame.localVariables.get_unchecked_mut(*index)) } = x;
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
            Call { encoded } => {
                // println!("function call {}", encoded);
                // println!("stack size {}", vm.stack.len());
                let cached = match &vm.opCodeCache[index] {
                    Some(v) => match v {
                        CachedOpCode::CallCache {
                            stack,
                            typ,
                            argCount,
                        } => (stack, typ, argCount),
                    },
                    None => {
                        let f = vm.functions.get(encoded).unwrap();
                        // println!("meta {:?}", f.varTable);
                        let localVars = vec![Value::Num(-1); f.varTable.len()]; // Vec::with_capacity(f.varTable.len());

                        /*
                        for i in &*f.varTable {
                            localVars.push(i.typ.toDefaultValue())
                        }

                         */

                        vm.opCodeCache[index] = Some(CachedOpCode::CallCache {
                            stack: localVars,
                            typ: f.typ.clone(),
                            argCount: f.argAmount,
                        });
                        match unsafe{ vm.opCodeCache.get_unchecked(index) } {
                            None => panic!(),
                            Some(v) => {
                                match v {
                                    CachedOpCode::CallCache {
                                        ref stack,
                                        ref typ,
                                        ref argCount,
                                    } => (stack, typ, argCount)
                                }
                            }
                        }
                    }
                };

                let ee = &mut cached.0.clone();
                let argsLen = ee.len();
                // println!("args len {} {}", argsLen, cached.2);
                // println!("vm {:?} {}", vm.stack, encoded);

                for i in 0..(*cached.2) {
                    let arg = match vm.stack.pop() {
                        None => {
                            return;
                        }
                        Some(v) => v
                    };
                    // println!("seting {:?} {:?}", i, &arg);
                    ee[(cached.2 - 1) - i] = arg;
                }

                // FIXME
                // let enc = &String::from(encoded);
                // println!("frame {:?}", &ee);
                match cached.1 {
                    Runtime {
                        rangeStart: s,
                        rangeStop: e,
                    } => {
                        let old = index + 1;

                        let mut stack = StackFrame {
                            previous: Some(stackFrame),
                            localVariables: ee,
                            name: None,
                        };

                        opCodes.index = *s as isize;

                        run(opCodes, vm, &mut stack);
                        opCodes.index = old as isize;
                    }
                    Native { callback } => {
                        let mut stack = StackFrame {
                            previous: Some(stackFrame),
                            localVariables: ee,
                            name: None,
                        };
                        callback(vm, &mut stack)
                    },
                }
            }
            Return => return,
            Add(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len()-1;
                vm.stack.get_unchecked_mut(l).add(&a, v);
            }
            Sub(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len()-1;
                vm.stack.get_unchecked_mut(l).sub(&a, v);
            }
            Div(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len()-1;
                vm.stack.get_unchecked_mut(l).div(&a, v);
            }
            Mul(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).mul(&a, v);
                // println!("{:?}", vm.stack.get(l));
            }
            Equals(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).refEq(&a, v);
            }
            Greater(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).refGt(&a, v);
            }
            Less(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).refLess(&a, v);
            }
            Or => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).or(&a);
            }
            And => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).and(&a);
            }
            Not => unsafe {
                let l = vm.stack.len() - 1;
                vm.stack.get_unchecked_mut(l).not();
            }
            ClassBegin => panic!(),
            ClassName { .. } => panic!(),
            ClassField { .. } => panic!(),
            ClassEnd => panic!(),
            New { .. } => panic!(),
            GetField { .. } => panic!(),
            SetField { .. } => panic!(),
            ArrayNew(d) => {
                // println!("{}", vm.stack.len());
                let size = vm.stack.pop().unwrap();
                vm.stack.push(Value::Reference { instance: Some(Rc::new(crate::objects::Array { internal: vec![], typ: d.clone() })) })
            },
            ArrayStore(_) => {
                let index = vm.stack.pop().unwrap().getNum();
                let val = vm.stack.pop().unwrap();
                match vm.stack.pop().unwrap() {
                    Reference { instance } => unsafe {
                        let mut clon = instance.unwrap();
                        let ne = Rc::get_mut_unchecked(&mut clon);
                        match ne.downcast_mut::<crate::objects::Array>() {
                            Some(v) => {
                                if index as usize == v.internal.len() {
                                    v.internal.push(val)
                                } else {
                                    v.internal[index as usize] = val
                                }
                            }
                            None => panic!()
                        }
                    }
                    _ => panic!()
                };
            },
            ArrayLoad(_) => {
                let index = vm.stack.pop().unwrap().getNum();
                match vm.stack.pop().unwrap() {
                    Reference { instance } => unsafe {
                        let mut clon = instance.unwrap();
                        let ne = Rc::get_mut_unchecked(&mut clon);
                        match ne.downcast_mut::<crate::objects::Array>() {
                            None => {}
                            Some(v) => {
                                vm.stack.push(v.internal.get(index as usize).unwrap().clone())
                            }
                        }
                    }
                    _ => panic!()
                };
            },
            ArrayLength => {
                match vm.stack.pop().unwrap() {
                    Reference { instance } => unsafe {
                        let mut clon = instance.unwrap();
                        let ne = Rc::get_mut_unchecked(&mut clon);
                        match ne.downcast_mut::<crate::objects::Array>() {
                            None => {}
                            Some(v) => {
                                vm.stack.push(Value::Num(v.internal.len() as isize))
                            }
                        }
                    }
                    _ => panic!()
                };
            },
            Inc { typ, index } => unsafe {stackFrame.localVariables.get_unchecked_mut(*index).inc(typ)},
            Dec { typ, index } => unsafe {stackFrame.localVariables.get_unchecked_mut(*index).dec(typ)},
            PushChar(c) => {
                vm.stack.push(Value::Chr(*c))
            }
        }
    }
}

pub fn evaluateBytecode(bytecode: Vec<OpCode>, locals: Vec<DataType>) -> VirtualMachine {
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
            opCodes: &bytecode,
            start: None,
            end: None,
        },
        &mut vm,
        &mut StackFrame::new(&mut vals),
    );

    vm
}

pub fn evaluateBytecode2(bytecode: Vec<OpCode>, locals: Vec<DataType>, vm: &mut VirtualMachine) {
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
            opCodes: &bytecode,
            start: None,
            end: None,
        },
        vm,
        &mut StackFrame::new(&mut vals),
    );
}
