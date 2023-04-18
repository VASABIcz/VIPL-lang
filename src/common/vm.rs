use std::{intrinsics, ptr};
use std::alloc::{alloc, Layout};
use std::collections::HashMap;
use std::env::var;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

use libloading::{Library, Symbol};

use crate::asm::asmGen::generateAssembly;
use crate::asm::asmLib::NasmGen;
use crate::asm::jitCompiler::JITCompiler;
use crate::ast::{Expression, Op};
use crate::bytecodeGen::genFunctionDef;
use crate::ffi::NativeWrapper;
use crate::heap::{Allocation, Hay, HayCollector, Heap};
use crate::lexer::tokenizeSource;
use crate::namespace::{FunctionMeta, FunctionTypeMeta, LoadedFunction, Namespace, NamespaceState};
use crate::namespace::NamespaceState::Loaded;
use crate::nativeStack::StackManager;
use crate::objects::{Array, ObjectDefinition, ViplObject};
use crate::parser::{parseDataType, TokenProvider};
use crate::std::std::bootStrapVM;
use crate::value::Value;
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
    Function{
        args: Vec<DataType>,
        ret: Box<DataType>
    },
    Void
}

impl DataType {
    pub fn asArray(&self) -> Result<&ObjectMeta,Box<dyn Error>>  {
        match self {
            Object(o) => {
                if o.name.as_str() == "Array" {
                    Ok(o)
                }
                else {
                    Err(format!("expected Array got: {:?}", o.name).into())
                }
            }
            v => Err(format!("expected Array got: {:?}", v).into())
        }
    }

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
    pub fn toString(&self) -> String {
        match self {
            Int => "int".to_string(),
            Float => "float".to_string(),
            Bool => "bool".to_string(),
            Object(x) => x.name.clone().to_string(),
            Char => "char".to_string(),
            Void => "!".to_string(),
            Function { args, ret } =>
                format!("({}): {}", args.iter().map(|it| {
                    it.toString()
                }).collect::<Vec<_>>().join(", "), ret.toString())
        }
    }

    pub fn toCString(&self) -> &str {
        match self {
            Int => "long",
            Float => "float",
            Bool => "bool",
            Object(_) => "ViplObject*",
            Char => "char",
            Void => "void",
            Function { .. } => todo!()
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
            Object(_) => 0.into(),
            Function { .. } => 0.into(),
            Void => unreachable!(),
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
    F2I,
    I2F,
    PushInt(isize),
    PushFunction(u32, u32),
    PushIntOne(),
    PushIntZero(),
    PushFloat(f64),
    PushBool(bool),
    PushChar(char),
    Pop,
    Dup,
    Swap,
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
    SCall {
        id: usize
    },
    LCall {
        namespace: usize,
        id: usize
    },
    DynamicCall,
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
    New {
        namespaceID: usize,
        structID: usize
    },
    GetField {
        namespaceID: usize,
        structID: usize,
        fieldID: usize
    },
    SetField {
        namespaceID: usize,
        structID: usize,
        fieldID: usize
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

#[derive(Debug)]
pub struct StackFrame<'a> {
    pub localVariables: &'a mut [Value],
    pub objects: Option<Vec<Hay<ViplObject>>>,
    pub previous: Option<&'a StackFrame<'a>>,
    pub programCounter: usize,
    pub namespace: &'a Namespace,
    pub functionID: usize
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

pub fn parseDataTypeFromStr(s: &str) -> Result<DataType, Box<dyn Error>> {
    let p = tokenizeSource(s)?;
    parseDataType(&mut TokenProvider::new(p))
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub returnType: Option<DataType>,
    pub varTable: Box<[VariableMetadata]>,
    pub argAmount: usize,
    pub typ: FuncType,
}

pub type ExternFn = extern fn(&mut VirtualMachine, &mut StackFrame) -> ();
pub type BuiltInFn = fn(&mut VirtualMachine, &mut StackFrame) -> ();

#[derive(Clone, Copy)]
pub enum FuncType {
    Runtime {
        rangeStart: usize,
        rangeStop: usize,
    },
    Builtin {
        callback: BuiltInFn,
    },
    Extern {
        callback: ExternFn,
    },
}

impl Debug for FuncType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Runtime { rangeStart, rangeStop: _ } => {
                write!(f, "runtime func {:?}", rangeStart)
            }
            Builtin { callback } => {
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

#[derive(Debug)]
pub struct NamespaceLoader {
    pub lookupPaths: Vec<String>,
    pub lookupBuiltin: Vec<fn(&mut VirtualMachine)>
}

impl NamespaceLoader {
    pub fn registerPath(&mut self, path: &str, depth: usize) {
        todo!()
    }

    pub fn registerBuiltin(&mut self, name: Vec<String>, init: fn (&mut VirtualMachine)) {
        todo!()
    }

    pub fn loadNamespace(&self, path: Vec<String>) -> Namespace {
        todo!()
    }

    pub fn new() -> Self {
        let s = Self {
            lookupPaths: vec![],
            lookupBuiltin: vec![],
        };

        s
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct VirtualMachine<'a> {
    pub nativeWrapper: NativeWrapper,

    // pub functions: HashMap<MyStr, Func>,
    // pub classes: HashMap<MyStr, ObjectDefinition>,
    pub stack: Vec<Value>,
    // pub opCodes: Vec<OpCode>,
    // pub opCodeCache: Vec<Option<CachedOpCode>>,
    pub nativeLibraries: Vec<Library>,
    pub heap: Heap,

    // pub cachedFunctions: Vec<Func>,
    // pub cachedFunctionsLookup: HashMap<MyStr, usize>,

    pub stackManager: StackManager<2048>,

    pub frames: Vec<StackFrame<'a>>,

    pub namespaceLookup: HashMap<String, usize>,
    pub namespaces: Vec<Namespace>,

    pub namespaceLoader: NamespaceLoader,
    pub jitCompiler: JITCompiler
}

const DEBUG: bool = false;

impl VirtualMachine<'_> {
    pub fn buildFunctionReturn(&self) -> HashMap<MyStr, Option<DataType>> {
        let mut x = HashMap::new();

        for n in &self.namespaces {
            for f in &n.functionsMeta {
                let mut buf = String::new();
                buf += &n.name;
                buf += "::";
                buf += &f.genName();
                x.insert(buf.into(), f.returnType.clone());
            }
        }

        x
    }

    pub fn registerNamespace(&mut self, mut namespace: Namespace) -> usize {
        let index = self.namespaces.len();
        namespace.id = index;

        self.namespaceLookup.insert(namespace.name.clone(), index);
        self.namespaces.push(namespace);
        index
    }

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
    pub fn getMutTop(&self) -> &mut Value {
        let s = self.stack.len();
        unsafe { (&mut *(&self.stack as *const Vec<Value> as *mut Vec<Value>)).get_unchecked_mut(s - 1) }
    }

    #[inline(always)]
    pub fn getTop(&self) -> &Value {
        let s = self.stack.len();
        unsafe { self.stack.get_unchecked(s - 1) }
    }

    #[inline]
    pub fn call(&mut self, name: MyStr) {
        todo!()
    }
}

impl VirtualMachine<'_> {
    #[inline(always)]
    pub fn getFrame<'a>(&self) -> &'a StackFrame {
        unsafe { self.frames.get_unchecked(self.frames.len() - 1) }
    }

    #[inline(always)]
    pub fn getMutFrame(&self) -> &mut StackFrame {
        let i = self.frames.len() - 1;
        unsafe { (&mut *(self as *const VirtualMachine as *mut VirtualMachine)).frames.get_unchecked_mut(i) }
    }

    #[inline]
    pub fn nextOpcode2<'a>(&'a self, ops: &'a Vec<OpCode>) -> (Option<&OpCode>, usize) {
        let x = self.getMutFrame();
        x.programCounter += 1;
        (ops.get(x.programCounter-1), x.programCounter-1)
    }

    #[inline]
    pub fn getIndex(&self) -> usize {
        unsafe { self.frames.get_unchecked(self.frames.len() - 1).programCounter }
    }

    #[inline]
    pub fn setIndex(&mut self, index: usize) {
        let i = self.frames.len() - 1;
        unsafe { self.frames.get_unchecked_mut(i).programCounter = index }
    }

    #[inline]
    pub fn getLocal(&self, index: usize) -> &Value {
        let f = self.getFrame();
        unsafe { f.localVariables.get_unchecked(index) }
    }

    #[inline]
    pub fn getMutLocal(&self, usize: usize) -> &mut Value {
        let i = self.frames.len() - 1;
        unsafe { (&mut *(&self.frames as *const Vec<StackFrame<'_>> as *mut Vec<StackFrame<'_>>)).get_unchecked_mut(i).localVariables.get_unchecked_mut(usize) }
    }

    #[inline]
    pub fn setLocal(&mut self, index: usize, value: Value) {
        let i = self.frames.len() - 1;
        unsafe { self.frames.get_unchecked_mut(i).localVariables[index] = value }
    }

    #[inline]
    pub fn seek(&mut self, offset: isize) {
        let f = self.getMutFrame();
        f.programCounter = ((f.programCounter as isize) + offset) as usize;
    }

    #[inline]
    pub fn goto(&mut self, index: usize) {
        self.getMutFrame().programCounter = index;
    }

    #[inline]
    pub fn pushFrame(&mut self, frame: StackFrame) {
        if self.frames.len() > 2048 {
            panic!()
        }
        unsafe { (&mut *(self as *const VirtualMachine as *mut VirtualMachine)).frames.push(frame) };
        let f = self.getFrame();
    }

    #[inline]
    pub fn popFrame(&mut self) {
        self.frames.pop();
    }

    #[inline]
    pub fn execute(&mut self, opCodes: &Vec<OpCode>) {
        loop {
            let index = self.getIndex();
            let op = match opCodes.get(index) {
                None => {
                    self.popFrame();
                    return;
                }
                Some(v) => v
            };
            self.getMutFrame().programCounter += 1;

            if DEBUG {
                println!("evaluating {:?}", op);
            }

            unsafe {
                match op {
                    F2I => {
                        self.getMutTop().f2i()
                    }
                    I2F => {
                        self.getMutTop().f2i()
                    }
                    PushInt(v) => self.stack.push(Value { Num: *v }),
                    PushIntOne() => self.stack.push(Value { Num: 1 }),
                    PushIntZero() => self.stack.push(Value { Num: 0 }),
                    PushFloat(v) => self.stack.push((*v).into()),
                    PushBool(v) => self.stack.push((*v).into()),
                    Pop => {
                        self.stack.pop();
                    }
                    Dup => unsafe {
                        let val = self.getTop();
                        self.stack.push(*val);
                    },
                    PushLocal { index } => {
                        self.stack.push(*self.getLocal(*index))
                    }
                    SetLocal { index, typ: _ } => {
                        let x = self.pop();
                        self.setLocal(*index, x);
                    }
                    Jmp { offset, jmpType } => match jmpType {
                        JmpType::One => {
                            self.pop();
                            let _b = self.pop();
                            panic!()
                        }
                        JmpType::Zero => {}
                        JmpType::Jmp => {
                            let x = *offset;
                            self.seek(x);
                        }
                        JmpType::Gt => {
                            let a = self.pop();
                            let b = self.pop();
                            if a.gt(&b, &DataType::Float) {
                                let x = *offset;
                                self.seek(x)
                            }
                        }
                        JmpType::Less => {
                            let a = self.pop();
                            let b = self.pop();
                            if a.less(&b, &DataType::Float) {
                                let x = *offset;
                                self.seek(x)
                            }
                        }
                        JmpType::True => {
                            let a = self.pop();
                            if a.getBool() {
                                let x = *offset;
                                self.seek(x)
                            }
                        }
                        JmpType::False => {
                            let a = self.pop();
                            if !a.getBool() {
                                let x = *offset;
                                self.seek(x)
                            }
                        }
                    },
                    Call { encoded } => panic!("deprecated"),
                    Return => {
                        self.popFrame();
                        return;
                    },
                    Add(v) => unsafe {
                        let a = self.pop();
                        self.getMutTop().add(a, v, &mut *(self as *const VirtualMachine as *mut VirtualMachine));
                    },
                    Sub(v) => {
                        let a = self.pop();
                        self.getMutTop().sub(&a, v);
                    },
                    Div(v) => {
                        let a = self.pop();
                        self.getMutTop().div(&a, v);
                    },
                    Mul(v) => {
                        let a = self.pop();
                        self.getMutTop().mul(&a, v);
                    },
                    Equals(v) => {
                        let a = self.pop();
                        self.getMutTop().refEq(&a, v);
                    },
                    Greater(v) => {
                        let a = self.pop();
                        self.getMutTop().refGt(&a, v);
                    },
                    Less(v) => {
                        let a = self.pop();
                        self.getMutTop().refLess(&a, v);
                    },
                    Or => {
                        let a = self.pop();
                        self.getMutTop().or(&a);
                    },
                    And => {
                        let a = self.pop();
                        self.getMutTop().and(&a);
                    },
                    Not => {
                        self.getMutTop().not();
                    },
                    ArrayNew(d) => {
                        let _size = self.pop();
                        let a = Value::makeArray(vec![], d.clone(), self);
                        self.stack.push(a)
                    }
                    ArrayStore(_) => {
                        let index = self.pop().getNum();

                        let value = self.pop();

                        let mut arrayRaw = self.pop();
                        let array = arrayRaw.getMutArray();

                        if index as usize == array.internal.len() {
                            array.internal.push(value)
                        } else {
                            array.internal[index as usize] = value
                        }
                    }
                    ArrayLoad(_) => {
                        let index = self.pop().getNum();
                        let mut value1 = self.pop();
                        let arr = value1.getMutArray();
                        self.stack.push(*arr.internal.get(index as usize).unwrap());
                    }
                    ArrayLength => {
                        self.stack.push(Value { Num: self.pop().getReference().getArr().internal.len() as isize });
                    },
                    // FIXME inc is slower than executing: pushLocal, PushOne, Add, SetLocal
                    Inc { typ, index } => unsafe {
                        self.getMutLocal(*index).inc(typ)
                    },
                    Dec { typ, index } => unsafe {
                        self.getMutLocal(*index).dec(typ)
                    },
                    PushChar(c) => self.stack.push((*c).into()),
                    StrNew(s) => {
                        let a = Value::makeString(s.to_string(), self);
                        self.stack.push(a)
                    },
                    GetChar => {
                        let index = self.pop().getNum();

                        let opIndex = self.stack.len() - 1;

                        let r = self.stack.get_mut(opIndex).unwrap();
                        let c = *r.getString().as_bytes().get(index as usize).unwrap() as char;
                        *r = c.into();
                    },
                    SCall { id } => {
                        let r = self as *const VirtualMachine as *mut VirtualMachine;
                        let frame = self.getFrame();
                        let fMeta = frame.namespace.functionsMeta.get_unchecked(*id);
                        let f = frame.namespace.functions.get_unchecked(*id);


                        let mut locals = fMeta.localsMeta.iter().map(|it| {
                            it.typ.toDefaultValue()
                        }).collect::<Vec<_>>();

                        for i in 0..fMeta.argsCount {
                            let arg = self.pop();
                            // println!("poped {}", arg.asNum());
                            locals[(fMeta.argsCount - 1) - i] = arg;
                        }

                        let mut fs = StackFrame{
                            localVariables: &mut locals,
                            objects: None,
                            previous: None,
                            programCounter: 0,
                            namespace: frame.namespace,
                            functionID: *id,
                        };

                        f.as_ref().unwrap().call(&mut *r, fs)
                    }
                    LCall { namespace, id} => {
                        let r = self as *const VirtualMachine as *mut VirtualMachine;
                        let frame = self.getFrame();
                        let namespace = self.namespaces.get(*namespace).unwrap();
                        let fMeta = namespace.functionsMeta.get(*id).unwrap();
                        let f = namespace.functions.get(*id).unwrap();

                        let mut locals = fMeta.localsMeta.iter().map(|it| {
                            it.typ.toDefaultValue()
                        }).collect::<Vec<_>>();

                        for i in 0..fMeta.argsCount {
                            let arg = self.pop();
                            // println!("poped {}", arg.asNum());
                            locals[(fMeta.argsCount - 1) - i] = arg;
                        }

                        let mut fs = StackFrame{
                            localVariables: &mut locals,
                            objects: None,
                            previous: None,
                            programCounter: 0,
                            namespace: frame.namespace,
                            functionID: *id,
                        };

                        f.as_ref().unwrap().call(&mut *r, fs)
                    }
                    PushFunction(namespaceID, functionID) => {
                        self.stack.push(Value::makeFunction(*namespaceID, *functionID));
                    }
                    DynamicCall => {
                        let (namespaceRaw, idRaw) = self.stack.pop().unwrap().asFunction();
                        let namespace = namespaceRaw as usize;
                        let id = idRaw as usize;


                        let r = self as *const VirtualMachine as *mut VirtualMachine;
                        let frame = self.getFrame();
                        let namespace = self.namespaces.get(namespace).unwrap();
                        let fMeta = namespace.functionsMeta.get(id).unwrap();
                        let f = namespace.functions.get(id).unwrap();

                        let mut locals = fMeta.localsMeta.iter().map(|it| {
                            it.typ.toDefaultValue()
                        }).collect::<Vec<_>>();

                        for i in 0..fMeta.argsCount {
                            let arg = self.pop();
                            // println!("poped {}", arg.asNum());
                            locals[(fMeta.argsCount - 1) - i] = arg;
                        }

                        let mut fs = StackFrame{
                            localVariables: &mut locals,
                            objects: None,
                            previous: None,
                            programCounter: 0,
                            namespace: frame.namespace,
                            functionID: id,
                        };

                        f.as_ref().unwrap().call(&mut *r, fs)
                    }
                    New { namespaceID, structID } => {
                        let n = self.namespaces.get(*namespaceID).unwrap();
                        let s = n.structs.get(*structID).unwrap();

                        let alloc = alloc(Layout::array::<Value>(s.fields.len()).unwrap()) as *mut Value;
                        let d = vec![Value::from(0); s.fields.len()];

                        let ptr = Box::into_raw(d.into_boxed_slice());
                        self.heap.allocations.insert(ptr as *mut Value as usize);


                        self.stack.push(Value::from(ptr as *mut Value as usize))
                    }
                    SetField { namespaceID, structID, fieldID } => {
                        let value = self.pop();
                        let ptr = self.pop().Reference.inner as *mut Value;

                        *ptr.add(*fieldID) = value;
                    }
                    GetField { namespaceID, structID, fieldID } => {
                        let ptr = self.pop().Reference.inner as *mut Value;

                        let v = ptr.add(*fieldID).read();
                        self.stack.push(v);
                    }
                    Swap => {
                        let a = self.pop();
                        let b = self.pop();
                        self.stack.push(a);
                        self.stack.push(b);
                    }
                    o => panic!("unimplemented opcode {:?}", o)
                }
            }
        }
    }
}

impl VirtualMachine<'_> {
    pub fn link(&mut self) {
        let functionReturns = self.buildFunctionReturn();

        let v = self as *mut VirtualMachine as *const VirtualMachine;

        for n in &mut self.namespaces {
            let nn = n as *mut Namespace;
            if n.state == Loaded {
                continue
            }

            for (index, f) in n.functionsMeta.iter_mut().enumerate() {
                unsafe {
                    if let FunctionTypeMeta::Runtime(_) = f.functionType {
                        let mut ops = vec![];
                        let res = unsafe { genFunctionDef(f, &mut ops, &functionReturns, &*v, &mut *nn).unwrap() };
                        f.localsMeta = res.into_boxed_slice();
                        println!("f ops {:?}", ops);

                        // let nf = self.jitCompiler.compile(ops, &*v, &*nn);

                        *n.functions.get_mut(index).unwrap() = Some(LoadedFunction::Virtual(ops));
                    }
                }
            }

            n.state = Loaded;
        }
    }

    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(128),
            nativeWrapper: NativeWrapper::new(),
            nativeLibraries: vec![],
            heap: Default::default(),
            stackManager: StackManager::new(),
            frames: vec![],
            namespaceLookup: Default::default(),
            namespaces: vec![],
            namespaceLoader: NamespaceLoader::new(),
            jitCompiler: JITCompiler {},
        }
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
    pub fn nextOpcode(&mut self) -> (Option<&OpCode>, usize) {
        let n = self.opCodes.get(self.index);
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
        buf.push_str(&arg.toString());
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
        buf.push_str(&arg.typ.toString());
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
            Call { encoded } => panic!("deprecated"),
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

impl Drop for VirtualMachine<'_> {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}
