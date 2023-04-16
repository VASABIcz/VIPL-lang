use std::{intrinsics, ptr};
use std::alloc::{alloc, Layout};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

use libloading::{Library, Symbol};
use crate::asm::asmGen::generateAssembly;
use crate::asm::asmLib::NasmGen;
use crate::asm::jitCompiler::JITCompiler;

use crate::ast::{Expression, Op};
use crate::betterGen::genFunctionDef;
use crate::ffi::NativeWrapper;
use crate::heap::{Allocation, Hay, HayCollector, Heap};
use crate::namespace::{FunctionMeta, FunctionTypeMeta, LoadedFunction, Namespace, NamespaceState};
use crate::namespace::NamespaceState::Loaded;
use crate::nativeStack::StackManager;
use crate::objects::{Array, ObjectDefinition, ViplObject};
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
    PushFunction(u32, u32),
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
    pub namespace: &'a Namespace
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
        todo!();
        StackFrame {
            localVariables,
            // name: Option::from("root"),
            objects: None,
            previous: None,
            programCounter: 0,
            namespace: &Namespace::new("".to_string()),
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

impl VirtualMachine<'_> {
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

    pub functions: HashMap<MyStr, Func>,
    pub classes: HashMap<MyStr, ObjectDefinition>,
    pub stack: Vec<Value>,
    pub opCodes: Vec<OpCode>,
    pub opCodeCache: Vec<Option<CachedOpCode>>,
    pub nativeLibraries: Vec<Library>,
    pub heap: Heap,

    pub cachedFunctions: Vec<Func>,
    pub cachedFunctionsLookup: HashMap<MyStr, usize>,

    pub stackManager: StackManager<2048>,

    pub frames: Vec<StackFrame<'a>>,

    pub namespaceLookup: HashMap<String, usize>,
    pub namespaces: Vec<Namespace>,

    pub namespaceLoader: NamespaceLoader,
    pub jitCompiler: JITCompiler
}

const DEBUG: bool = false;

impl VirtualMachine<'_> {
    pub fn builtdFuncgtionReturn(&self) -> HashMap<MyStr, Option<DataType>> {
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

    pub fn callFast(&mut self, identifier: usize) {
        let f = unsafe { self.cachedFunctions.get_unchecked(identifier) };
        let mut locals = vec![Value{Num: 0}; f.varTable.len()];

        for i in 0..(f.argAmount) {
            let arg = self.stack.pop().unwrap();
            locals[(f.argAmount - 1) - i] = arg;
        }

        todo!();
        let mut stack = StackFrame {
            localVariables: &mut locals,
            // name: None,
            objects: None,
            previous: None,
            programCounter: 0,
            namespace: &Namespace::new("".to_string()),
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
            Builtin { callback } => callback(self, &mut stack),
            Extern { callback } => {
                // stack.objects = Some(vec![]);
                callback(self, &mut stack);
            },
        }
    }

    #[inline]
    pub fn callRaw(&mut self, func: &Func, stack: &mut StackFrame) {
        match func.typ {
            Runtime { rangeStart, rangeStop } => unsafe {
                let x = &mut *(self as *mut VirtualMachine);
                let mut seekable = SeekableOpcodes {
                    index: rangeStart,
                    opCodes: &mut self.opCodes,
                };
                run(&mut seekable, x, stack);
            }
            Builtin { callback } => callback(self, stack),
            Extern { callback } => {
                stack.objects = Some(vec![]);
                callback(self, stack)
            }
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

        todo!();
        let mut stack = StackFrame {
            localVariables: &mut locals,
            // name: None,
            objects: None,
            previous: None,
            programCounter: 0,
            namespace: &Namespace::new("".to_string()),
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
            Builtin { callback } => callback(self, &mut stack),
            Extern { callback } => {
                stack.objects = Some(vec![]);
                callback(self, &mut stack);
            },
        }
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
    pub fn nextOpcode(&self) -> (Option<&OpCode>, usize) {
        let x = self.getMutFrame();
        x.programCounter += 1;
        (self.opCodes.get(x.programCounter-1), x.programCounter-1)
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
    pub fn getOpcode(&self, index: usize) -> Option<&OpCode> {
        self.opCodes.get(index)
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
    pub fn addBytecode(&mut self, bytecode: Vec<OpCode>) {
        for _ in 0..(bytecode.len()) {
            self.opCodeCache.push(None)
        }
        self.opCodes.extend(bytecode);
    }

    #[inline]
    pub fn execute2(&mut self, opCodes: &Vec<OpCode>) {
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

            // println!("evaluating {:?}", op);
            unsafe {
                match op {
                    FunBegin => {
                        let mut index = self.getIndex();
                        let name = match self.getOpcode(index).unwrap() {
                            FunName { name } => name,
                            v => panic!("{v:?}"),
                        };
                        index += 1;
                        let (vars, argCount) = match self.getOpcode(index).unwrap() {
                            LocalVarTable { typ, argsCount } => (typ, argsCount),
                            v => panic!("{v:?}"),
                        };
                        index += 1;
                        let ret = match self.getOpcode(index).unwrap() {
                            FunReturn { typ } => typ,
                            v => panic!("{v:?}"),
                        };
                        index += 1;
                        let startIndex = index;

                        'a: loop {
                            let peek = self.getOpcode(index).unwrap();
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

                        self.makeRuntime(
                            name.to_string(),
                            vars.clone(),
                            startIndex,
                            *argCount,
                            ret.clone(),
                            0,
                        );
                        self.goto(index)
                    }
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
                    Call { encoded } => unsafe {
                        todo!("deprecated");
                        println!("{} {} {}", &encoded, self.opCodeCache.len(), index);
                        let cached = match &self.opCodeCache.get_unchecked(index) {
                            Some(v) => match v {
                                CachedOpCode::CallCache {
                                    locals: stack,
                                    typ,
                                    argCount,
                                } => (stack, typ, argCount),
                            },
                            None => {
                                let f = self.functions.get(encoded).unwrap_or_else(|| panic!("function {} not found", &encoded));
                                let localVars = vec![Value { Num: 0 }; f.varTable.len()];

                                self.opCodeCache[index] = Some(CachedOpCode::CallCache {
                                    locals: localVars.into(),
                                    typ: f.typ,
                                    argCount: f.argAmount,
                                });
                                match self.opCodeCache.get_unchecked(index) {
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
                        for i in 0..(*cached.2) {
                            let arg = self.pop();
                            println!("poped {}", arg.asNum());
                            cahedLocals[(cached.2 - 1) - i] = arg;
                        }
                        // FIXME
                        let d = Box::leak(cahedLocals);
                        // println!("len {} {} {}", cahedLocals.len(), *cached.2, cahedLocals[0].asNum());

                        todo!();
                        let mut stack = StackFrame {
                            localVariables: d,
                            // name: None,
                            objects: None,
                            previous: None,
                            programCounter: 0,
                            namespace: &Namespace::new("".to_string()),
                        };

                        match cached.1 {
                            Runtime {
                                rangeStart: s,
                                rangeStop: _e,
                            } => {
                                stack.programCounter = *s;
                                self.pushFrame(stack);
                            }
                            Builtin { callback } => callback(self, &mut stack),
                            Extern { callback } => {
                                // stack.objects = Some(vec![]);
                                callback(self, &mut stack);
                            }
                        }
                    },
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
                        let mut c = self.pop();
                        let val = c.getMutArray();
                        let value = self.pop();

                        if index as usize == val.internal.len() {
                            val.internal.push(value)
                        } else {
                            val.internal[index as usize] = value
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
                            namespace: frame.namespace
                        };

                        f.call(&mut *r, fs)
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
                            namespace: frame.namespace
                        };

                        f.call(&mut *r, fs)
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
                            namespace: frame.namespace
                        };

                        f.call(&mut *r, fs)
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
                    o => panic!("unimplemented opcode {:?}", o)
                }
            }
        }
    }

    #[inline]
    pub fn execute(&mut self) {
        loop {
            let (op, index) = match self.nextOpcode() {
                (None, _) => {
                    self.popFrame();
                    return;
                }
                (Some(v), i) => (v, i),
            };
            // println!("evaluating {:?}", op);
            unsafe {
                match op {
                    FunBegin => {
                        let mut index = self.getIndex();
                        let name = match self.getOpcode(index).unwrap() {
                            FunName { name } => name,
                            v => panic!("{v:?}"),
                        };
                        index += 1;
                        let (vars, argCount) = match self.getOpcode(index).unwrap() {
                            LocalVarTable { typ, argsCount } => (typ, argsCount),
                            v => panic!("{v:?}"),
                        };
                        index += 1;
                        let ret = match self.getOpcode(index).unwrap() {
                            FunReturn { typ } => typ,
                            v => panic!("{v:?}"),
                        };
                        index += 1;
                        let startIndex = index;

                        'a: loop {
                            let peek = self.getOpcode(index).unwrap();
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

                        self.makeRuntime(
                            name.to_string(),
                            vars.clone(),
                            startIndex,
                            *argCount,
                            ret.clone(),
                            0,
                        );
                        self.goto(index)
                    }
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
                    Call { encoded } => unsafe {
                        println!("{}", &encoded);
                        let cached = match &self.opCodeCache.get_unchecked(index) {
                            Some(v) => match v {
                                CachedOpCode::CallCache {
                                    locals: stack,
                                    typ,
                                    argCount,
                                } => (stack, typ, argCount),
                            },
                            None => {
                                let f = self.functions.get(encoded).unwrap_or_else(|| panic!("function {} not found", &encoded));
                                let localVars = vec![Value { Num: 0 }; f.varTable.len()];

                                self.opCodeCache[index] = Some(CachedOpCode::CallCache {
                                    locals: localVars.into(),
                                    typ: f.typ,
                                    argCount: f.argAmount,
                                });
                                match self.opCodeCache.get_unchecked(index) {
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
                        for i in 0..(*cached.2) {
                            let arg = self.pop();
                            // println!("poped {}", arg.asNum());
                            cahedLocals[(cached.2 - 1) - i] = arg;
                        }
                        // FIXME
                        let d = Box::leak(cahedLocals);
                        // println!("len {} {} {}", cahedLocals.len(), *cached.2, cahedLocals[0].asNum());

                        todo!();
                        let mut stack = StackFrame {
                            localVariables: d,
                            // name: None,
                            objects: None,
                            previous: None,
                            programCounter: 0,
                            namespace: &Namespace::new("".to_string()),
                        };

                        match cached.1 {
                            Runtime {
                                rangeStart: s,
                                rangeStop: _e,
                            } => {
                                stack.programCounter = *s;
                                self.pushFrame(stack);
                            }
                            Builtin { callback } => callback(self, &mut stack),
                            Extern { callback } => {
                                // stack.objects = Some(vec![]);
                                callback(self, &mut stack);
                            }
                        }
                    },
                    Return => self.popFrame(),
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
                        let mut c = self.pop();
                        let val = c.getMutArray();
                        let value = self.pop();

                        if index as usize == val.internal.len() {
                            val.internal.push(value)
                        } else {
                            val.internal[index as usize] = value
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
                        let f = self.getFrame().namespace.functions.get_unchecked(*id);
                    }
                    LCall { namespace, id, } => {}
                    o => panic!("unimplemented opcode {:?}", o)
                }
            }
        }
    }
}

impl VirtualMachine<'_> {
    pub fn link(&mut self) {
        let idk = self.builtdFuncgtionReturn();
        let v = self as *mut VirtualMachine as *const VirtualMachine;
        for n in &mut self.namespaces {
            let nn = n as *mut Namespace;
            if n.state == Loaded {
                continue
            }

            for f in &mut n.functionsMeta[n.functions.len()..] {
                unsafe {
                    if let FunctionTypeMeta::Runtime(_) = f.functionType {
                        let mut ops = vec![];
                        let res = unsafe { genFunctionDef(f, &mut ops, &idk, &*v, &mut *nn).unwrap() };
                        f.localsMeta = res.into_boxed_slice();
                        println!("f ops {:?}", ops);
                        // let nf = self.jitCompiler.compile(ops, &*v, &*nn);
                        n.functions.push(LoadedFunction::Virtual(ops));
                    }
                }
            }

            n.state = Loaded;
        }
    }

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
            stackManager: StackManager::new(),
            frames: vec![],
            namespaceLookup: Default::default(),
            namespaces: vec![],
            namespaceLoader: NamespaceLoader::new(),
            jitCompiler: JITCompiler {},
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
            typ: Builtin { callback: fun },
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

    pub fn addOpcodes(&mut self, opCodes: Vec<OpCode>) {
        let l = opCodes.len();
        self.opCodes.extend(opCodes);
        for _ in 0..l {
            self.opCodeCache.push(None);
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

                todo!();
                let mut stack = StackFrame {
                    localVariables: &mut cahedLocals,
                    // name: None,
                    objects: None,
                    previous: Some(stackFrame),
                    programCounter: 0,
                    namespace: &Namespace::new("".to_string()),
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
                    Builtin { callback } => callback(vm, &mut stack),
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

impl VirtualMachine<'_> {
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

impl Drop for VirtualMachine<'_> {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}

pub fn evaluateBytecode(mut bytecode: Vec<OpCode>, locals: Vec<DataType>) -> VirtualMachine<'static> {
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
