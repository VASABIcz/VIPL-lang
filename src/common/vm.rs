use std::{intrinsics, ptr};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

use libloading::{Library, Symbol};

use crate::ast::Expression;
use crate::ffi::NativeWrapper;
use crate::heap::{Allocation, Hay, HayCollector, Heap};
use crate::objects::{Array, ObjectDefinition, ViplObject};
use crate::objects::Str;
use crate::std::bootStrapVM;
use crate::vm::DataType::*;
use crate::vm::FuncType::*;
use crate::vm::OpCode::*;

struct FastVec<T> {
    cap: usize,
    ptr: *mut T,
    size: usize
}

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
        Self{Num: val}
    }
}

impl From<char> for Value {
    #[inline]
    fn from(val: char) -> Self {
        Value{Chr: val}
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(val: f64) -> Self {
        Value{Flo: val}
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(val: bool) -> Self {
        Value{Bol: val}
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
            Int => 0.into(),
            Float => 0.0.into(),
            Bool => false.into(),
            Char => 0.into(),
            Object(_) => 0.into()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JmpType {
    One,
    Zero,
    Jmp,
    Gt,
    Less,
    True,
    False,
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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
    PushIntOne(),
    PushIntZero(),
    PushFloat(f64),
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

#[derive(Copy, Clone)]
pub union Value {
    pub Num: isize,
    pub Flo: f64,
    pub Bol: bool,
    pub Chr: char,
    pub Reference: Hay<ViplObject>,
}

impl Value {
    #[inline(always)]
    pub fn asHay(&self) -> Hay<ViplObject> {
        unsafe { self.Reference }
    }

    #[inline(always)]
    pub fn asRef(&self) -> &ViplObject {
        unsafe { &self.Reference }
    }

    #[inline(always)]
    pub fn asMutRef(&mut self) -> &mut ViplObject {
        unsafe { &mut self.Reference }
    }

    #[inline(always)]
    pub fn asChar(&self) -> char {
        unsafe { self.Chr }
    }

    #[inline(always)]
    pub fn asNum(&self) -> isize {
        unsafe { self.Num }
    }

    #[inline(always)]
    pub fn asFlo(&self) -> f64 {
        unsafe { self.Flo }
    }

    #[inline(always)]
    pub fn asBool(&self) -> bool {
        unsafe { self.Bol }
    }
}

impl Value {
    pub fn toExpression(&self, t: &DataType) -> Expression {
        match t {
            Int => {
                Expression::IntLiteral(self.asNum().to_string())
            }
            Float => {
                Expression::FloatLiteral(self.asFlo().to_string())
            }
            Bool => {
                Expression::BoolLiteral(self.asBool())
            }
            Char => {
                Expression::CharLiteral(self.asChar())
            }
            Object(_) => {
                panic!()
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.asNum())
    }
}

impl Value {
    #[inline(always)]
    pub fn getString(&self) -> &String {
        &self.asRef().getStr().string
    }

    #[inline(always)]
    pub fn getMutArray(&mut self) -> &mut Array {
        self.asMutRef().getMutArr()
    }

    #[inline]
    pub fn makeString(str: String, vm: &mut VirtualMachine) -> Value {
        Value{Reference: vm.heap.allocate(Str::new(str).into())}
    }

    #[inline]
    pub fn makeObject(_obj: Box<dyn crate::objects::Object>) -> Value {
        todo!();
        // Value{Reference: ManuallyDrop::new(Rice::new(ViplObject::Runtime(Box::leak(obj))))}
    }

    #[inline]
    pub fn makeArray(arr: Vec<Value>, typ: DataType, vm: &mut VirtualMachine) -> Value {
        Value{Reference: vm.heap.allocate(Array{internal: arr, typ}.into())}
    }

    #[inline]
    pub fn valueStr(&self) -> String {
        self.asNum().to_string()
    }
}

impl Value {
    #[inline(always)]
    pub fn getNum(self) -> isize {
        self.asNum()
    }

    #[inline(always)]
    pub fn getNumRef(&self) -> isize {
        self.asNum()
    }

    #[inline(always)]
    pub fn getFlo(&self) -> f64 {
        self.asFlo()
    }

    #[inline(always)]
    pub fn getRefFlo(&mut self) -> &mut f64 {
        unsafe { &mut self.Flo }
    }

    #[inline(always)]
    pub fn getRefNum(&mut self) -> &mut isize {
        unsafe { &mut self.Num }
    }

    #[inline(always)]
    pub fn getRefBol(&mut self) -> &mut bool {
        unsafe { &mut self.Bol }
    }

    #[inline(always)]
    pub fn getBool(&self) -> bool {
        self.asBool()
    }

    #[inline(always)]
    pub fn getReference(&self) -> &ViplObject {
        self.asRef()
    }

    #[inline(always)]
    pub fn getMutReference(&mut self) -> &mut ViplObject {
        self.asMutRef()
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
            Int => self.getNumRef() > val.getNumRef(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
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
            Int => self.getNumRef() < val.getNumRef(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
        }
    }

    #[inline]
    pub fn refLess(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNumRef() > val.getNumRef(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            _ => panic!()
        };

        *self = l.into()
    }

    #[inline]
    pub fn refGt(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNumRef() < val.getNumRef(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
        };

        *self = l.into()
    }

    #[inline]
    pub fn eq(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNumRef() == val.getNumRef(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object { .. } => panic!(),
        }
    }

    #[inline]
    pub fn refEq(&mut self, val: &Value, typ: &DataType) {
        let x = match typ {
            Int => self.getNumRef() == val.getNumRef(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object(a) => panic!("{:?}", a),
        };
        *self = x.into()
    }

    #[inline]
    pub fn getChar(&self) -> char {
        self.asChar()
    }

    #[inline]
    pub fn toDataType(&self) -> DataType {
        panic!()
    }
}

impl Value {
    #[inline]
    pub fn add(&mut self, value: Value, typ: &DataType, vm: &mut VirtualMachine) {
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
                        let str1 = self.getString();
                        let str2 = value.getString();

                        let mut buf = String::with_capacity(str1.len() + str2.len());

                        buf.push_str(str1);
                        buf.push_str(str2);

                        unsafe { self.Reference = Value::makeString(buf, vm).asHay() }
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
                *self.getRefNum() -= value.getNumRef();
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
                *self.getRefNum() *= value.getNumRef();
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
                *self.getRefNum() /= value.getNumRef();
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
    pub fn f2i(&mut self) {
        *self = (self.getFlo() as isize).into()
    }

    #[inline]
    pub fn i2f(&mut self) {
        *self = (self.getNumRef() as f64).into()
    }
}

#[derive(Debug)]
pub struct StackFrame<'a> {
    pub localVariables: &'a mut [Value],
    pub objects: Option<Vec<Hay<ViplObject>>>,
    pub previous: Option<&'a StackFrame<'a>>
}

impl StackFrame<'_> {
    pub fn collect(&self, vm: &VirtualMachine, collector: &mut HayCollector) {
        for local in self.localVariables.iter() {
            if vm.heap.contains(local.asNum() as usize) {
                collector.visit(local.asNum() as usize)
            }
        }
        if let Some(prev) = self.previous {
            prev.collect(vm, collector)
        }
    }
}

impl Drop for StackFrame<'_> {
    fn drop(&mut self) {}
}

/*impl StackFrame<'_> {
    #[inline]
    pub fn addObject(&mut self, obj: Hay<ViplObject>) {
        match &mut self.objects {
            None => panic!(),
            Some(v) => v.push(obj)
        }
    }
}*/

impl StackFrame<'_> {
    pub fn new(localVariables: &mut [Value]) -> StackFrame {
        StackFrame {
            localVariables,
            // name: Option::from("root"),
            objects: None,
            previous: None,
        }
    }
}

pub fn parseDataTypeFromStr(_s: &str) {
    // Array<Array<Array<int>>>
    // Array<Array<Array
    panic!()
}

pub fn decodeFunctionString(_s: &str) {
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

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub returnType: Option<DataType>,
    pub varTable: Box<[VariableMetadata]>,
    pub argAmount: usize,
    pub typ: FuncType,
}

#[derive(Clone, Copy)]
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
            Runtime { rangeStart, rangeStop: _ } => {
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
    pub nativeLibraries: Vec<Library>,
    pub heap: Heap,

    pub cachedFunctions: Vec<Func>,
    pub cachedFunctionsLookup: HashMap<MyStr, usize>
}

const DEBUG: bool = false;

impl VirtualMachine {
    #[inline(always)]
    pub fn pop(&self) -> Value {
        let mut res: &mut FastVec<Value> = unsafe { &mut *(&self.stack as *const Vec<Value> as *mut FastVec<Value>) };
        let mut buf: Value = Value{Num: 0};
        res.size -= 1;

        unsafe { ptr::copy(intrinsics::offset(res.ptr, res.size as isize) as *mut Value, &mut buf as *mut Value, 1); }

        buf
    }

    pub fn gc(&mut self, frame: &StackFrame) {
        let mut collector = HayCollector::new();

        for v in &self.stack {
            println!("num {}", v.asNum() as usize);
            if self.heap.allocations.contains(&(v.asNum() as usize)) {
                v.asRef().collectAllocations(&mut collector);
            }
        }
        frame.collect(self, &mut collector);
        println!("{}", collector.visited.len());
        self.heap.gc(collector);
    }

    #[inline(always)]
    pub fn getMutTop(&mut self) -> &mut Value {
        let s = self.stack.len();
        unsafe { self.stack.get_unchecked_mut(s - 1) }
    }

    #[inline(always)]
    pub fn getTop(&self) -> &Value {
        let s = self.stack.len();
        unsafe { self.stack.get_unchecked(s - 1) }
    }

    pub fn callFast(&mut self, identifier: usize) {
        let f = unsafe { self.cachedFunctions.get_unchecked(identifier) };
        let mut locals = vec![Value{Num: 0}; f.varTable.len()];

        for i in 0..(f.argAmount) {
            let arg = self.stack.pop().unwrap();
            locals[(f.argAmount - 1) - i] = arg;
        }

        let mut stack = StackFrame {
            localVariables: &mut locals,
            // name: None,
            objects: None,
            previous: None,
        };

        let t = f.typ;

        // FIXME this is so much cursed
        // FIXME i am bypassing all rust safety guaranties :)

        let ptr = self as *mut VirtualMachine;

        match t {
            Runtime {
                rangeStart: s,
                rangeStop: _e,
            } => unsafe {
                let mut seekable = SeekableOpcodes {
                    index: s,
                    opCodes: &mut (*ptr).opCodes,
                };
                run(&mut seekable, &mut *ptr, &mut stack);
            },
            Native { callback } => callback(self, &mut stack),
            Extern { callback } => {
                // stack.objects = Some(vec![]);
                callback(self, &mut stack);
            },
        }
    }

    #[inline]
    pub fn call(&mut self, name: MyStr) {
        let f = self.functions.get(&name).unwrap();
        let mut locals = vec![Value{Num: 0}; f.varTable.len()];

        for i in 0..(f.argAmount) {
            let arg = self.stack.pop().unwrap();
            locals[(f.argAmount - 1) - i] = arg;
        }

        let mut stack = StackFrame {
            localVariables: &mut locals,
            // name: None,
            objects: None,
            previous: None,
        };

        let t = f.typ;

        // FIXME this is so much cursed
        // FIXME i am bypassing all rust safety guaranties :)

        let ptr = self as *mut VirtualMachine;

        match t {
            Runtime {
                rangeStart: s,
                rangeStop: _e,
            } => unsafe {
                let mut seekable = SeekableOpcodes {
                    index: s,
                    opCodes: &mut (*ptr).opCodes,
                };
                run(&mut seekable, &mut *ptr, &mut stack);
            },
            Native { callback } => callback(self, &mut stack),
            Extern { callback } => {
                // stack.objects = Some(vec![]);
                callback(self, &mut stack);
            },
        }
    }
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
            stack: Vec::with_capacity(128),
            classes: Default::default(),
            opCodes: vec![],
            opCodeCache: vec![],
            nativeWrapper: NativeWrapper::new(),
            nativeLibraries: vec![],
            heap: Default::default(),
            cachedFunctions: vec![],
            cachedFunctionsLookup: Default::default(),
        }
    }

    pub fn registerFunc(&mut self, fun: Func, name: MyStr) {
        self.functions.insert(name.clone(), fun.clone());
        self.cachedFunctions.push(fun);
        self.cachedFunctionsLookup.insert(name, self.cachedFunctions.len()-1);
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
        let n = MyStr::Runtime(genName.into_boxed_str());
        let f = Func {
            name,
            returnType: ret,
            varTable: args,
            argAmount: l,
            typ: Native { callback: fun },
        };
        self.registerFunc(f, n);
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
        let n = MyStr::Runtime(genName.into_boxed_str());
        let f =     Func {
            name,
            returnType: ret,
            varTable: args,
            argAmount: l,
            typ: Extern { callback: fun },
        };
        self.registerFunc(f, n);
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

        self.registerFunc(fun, MyStr::Runtime(genName.into_boxed_str()));
    }
}

pub struct SeekableOpcodes<'a> {
    pub index: usize,
    pub opCodes: &'a mut [OpCode],
}

impl SeekableOpcodes<'_> {
    #[inline(always)]
    pub fn seek(&mut self, offset: isize) {
        // FIXME boundary check
        self.index = (self.index as isize + offset) as usize;
    }

    #[inline(always)]
    pub fn nextOpcode(&mut self) -> (Option<&mut OpCode>, usize) {
        let n = self.opCodes.get_mut(self.index);
        let i = self.index;
        self.index += 1;

        (n, i)
    }

    #[inline(always)]
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
                let mut index = opCodes.index;
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
                opCodes.index = index;
            }
            F2I => {
                vm.getMutTop().f2i()
            }
            I2F => {
                vm.getMutTop().f2i()
            }
            PushInt(v) => vm.stack.push(Value{Num: *v}),
            PushIntOne() => vm.stack.push(Value{Num: 1}),
            PushIntZero() => vm.stack.push(Value{Num: 0}),
            PushFloat(v) => vm.stack.push((*v).into()),
            PushBool(v) => vm.stack.push((*v).into()),
            Pop => {
                vm.stack.pop();
            }
            Dup => unsafe {
                let val = vm.getTop();
                vm.stack.push(*val);
            },
            PushLocal { index } => {
                vm.stack.push(*unsafe { stackFrame.localVariables.get_unchecked(*index) })
            }
            SetLocal { index, typ: _ } => {
                let x = vm.pop();
                *unsafe { stackFrame.localVariables.get_unchecked_mut(*index) } = x;
            }
            Jmp { offset, jmpType } => match jmpType {
                JmpType::One => {
                    vm.pop();
                    let _b = vm.pop();
                    panic!()
                }
                JmpType::Zero => {}
                JmpType::Jmp => {
                    let x = *offset;
                    opCodes.seek(x);
                }
                JmpType::Gt => {
                    let a = vm.pop();
                    let b = vm.pop();
                    if a.gt(&b, &DataType::Float) {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
                JmpType::Less => {
                    let a = vm.pop();
                    let b = vm.pop();
                    if a.less(&b, &DataType::Float) {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
                JmpType::True => {
                    let a = vm.pop();
                    if a.getBool() {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
                JmpType::False => {
                    let a = vm.pop();
                    if !a.getBool() {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                }
            },
            Call { encoded } => unsafe {
                let cached = match &vm.opCodeCache.get_unchecked(index) {
                    Some(v) => match v {
                        CachedOpCode::CallCache {
                            locals: stack,
                            typ,
                            argCount,
                        } => (stack, typ, argCount),
                    },
                    None => {
                        let f = vm.functions.get(encoded).unwrap_or_else(|| panic!("function {} not found", &encoded));
                        let localVars = vec![Value{Num: 0}; f.varTable.len()];

                        vm.opCodeCache[index] = Some(CachedOpCode::CallCache {
                            locals: localVars.into(),
                            typ: f.typ,
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
                    let arg = vm.pop();
                    cahedLocals[(cached.2 - 1) - i] = arg;
                }

                let mut stack = StackFrame {
                    localVariables: &mut cahedLocals,
                    // name: None,
                    objects: None,
                    previous: Some(stackFrame),
                };

                match cached.1 {
                    Runtime {
                        rangeStart: s,
                        rangeStop: _e,
                    } => {
                        let old = index + 1;

                        opCodes.index = *s;
                        run(opCodes, vm, &mut stack);
                        opCodes.index = old;
                    }
                    Native { callback } => callback(vm, &mut stack),
                    Extern { callback } => {
                        // stack.objects = Some(vec![]);
                        callback(vm, &mut stack);
                    }
                }
            },
            Return => return,
            Add(v) => unsafe {
                let a = vm.pop();
                let c = vm.getMutTop() as *mut Value;

                (*c).add(a, v, vm);
            },
            Sub(v) => {
                let a = vm.pop();
                vm.getMutTop().sub(&a, v);
            },
            Div(v) => {
                let a = vm.pop();
                vm.getMutTop().div(&a, v);
            },
            Mul(v) => {
                let a = vm.pop();
                vm.getMutTop().mul(&a, v);
            },
            Equals(v) => {
                let a = vm.pop();
                vm.getMutTop().refEq(&a, v);
            },
            Greater(v) => {
                let a = vm.pop();
                vm.getMutTop().refGt(&a, v);
            },
            Less(v) => {
                let a = vm.pop();
                vm.getMutTop().refLess(&a, v);
            },
            Or => {
                let a = vm.pop();
                vm.getMutTop().or(&a);
            },
            And => {
                let a = vm.pop();
                vm.getMutTop().and(&a);
            },
            Not => {
                vm.getMutTop().not();
            },
            ArrayNew(d) => {
                let _size = vm.pop();
                let a = Value::makeArray(vec![], d.clone(), vm);
                vm.stack.push(a)
            }
            ArrayStore(_) => {
                let index = vm.pop().getNum();
                let mut c = vm.pop();
                let val = c.getMutArray();
                let value = vm.pop();

                if index as usize == val.internal.len() {
                    val.internal.push(value)
                } else {
                    val.internal[index as usize] = value
                }
            }
            ArrayLoad(_) => {
                let index = vm.pop().getNum();
                let mut value1 = vm.pop();
                let arr = value1.getMutArray();
                vm.stack.push(*arr.internal.get(index as usize).unwrap());
            }
            ArrayLength => {
                vm.stack.push(Value{Num: vm.pop().getReference().getArr().internal.len() as isize});
            },
            // FIXME inc is slower than executing: pushLocal, PushOne, Add, SetLocal
            Inc { typ, index } => unsafe {
                stackFrame.localVariables.get_unchecked_mut(*index).inc(typ)
            },
            Dec { typ, index } => unsafe {
                stackFrame.localVariables.get_unchecked_mut(*index).dec(typ)
            },
            PushChar(c) => vm.stack.push((*c).into()),
            StrNew(s) => {
                let a = Value::makeString(s.to_string(), vm);
                vm.stack.push(a)
            },
            GetChar => {
                let index = vm.pop().getNum();

                let opIndex = vm.stack.len()-1;

                let r = vm.stack.get_mut(opIndex).unwrap();
                let c = *r.getString().as_bytes().get(index as usize).unwrap() as char;
                *r = c.into();
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
