use std::alloc::{alloc, Layout};
use std::cell::UnsafeCell;
use std::fmt::{Debug, Formatter};
use std::fs;
use std::hint::unreachable_unchecked;
use std::intrinsics::unlikely;
use std::mem::transmute;
use std::ops::Not;
use std::sync::Mutex;

use crate::asm::jitCompiler::JITCompiler;
use crate::ast::ASTNode;
use crate::bytecodeGen::{emitOpcodes, genFunctionDef, SymbolicOpcode};
use crate::codeGenCtx::{ExpressionCtx, SimpleCtx, StatementCtx};
use crate::errors::{CodeGenError, LoadFileError, SymbolNotFoundE};
use crate::fastAccess::FastAccess;
use crate::ffi::NativeWrapper;
use crate::implicitConverter::{implicitConversionRules, ImplicitConverter};
use crate::lexer::LexingUnit;
use crate::lexingUnits::{getLexingUnits, TokenType};
use crate::naughtyBox::Naughty;
use crate::parser::ParsingUnit;
use crate::parsingUnits::getParsingUnits;
use crate::symbolManager::SymbolManager;
use crate::utils::{FastVec, genFunName, genNamespaceName, transform};
use crate::viplParser::VIPLParsingState;
use crate::vm::dataType::{DataType, RawDataType};
use crate::vm::heap::{Allocation, Hay, HayCollector, Heap};
use crate::vm::namespace::{FunctionTypeMeta, LoadedFunction, loadSourceFile, Namespace};
use crate::vm::namespace::LoadedFunction::Native;
use crate::vm::namespace::NamespaceState::{FailedToLoad, Loaded};
use crate::vm::nativeObjects::{
    ObjectType, UntypedObject, ViplObject, ViplObjectMeta,
};
use crate::vm::objects::{Array, Str};
use crate::vm::optimizations::branchOmit::branchOmit;
use crate::vm::optimizations::bytecodeOptimizer::optimizeBytecode;
use crate::vm::optimizations::constEval::constEvaluation;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::FuncType::{Builtin, Extern, Runtime};
use crate::vm::vm::OpCode::*;

#[derive(Debug, Clone)]
pub enum ImportHints {
    Namespace(Vec<String>, Option<String>),
    Symbols(Vec<String>, Vec<(String, Option<String>)>),
}

// FIXME DEBUG is faster than default
const DEBUG: bool = true;
const TRACE: bool = false;
const OPTIMIZE: bool = false;
const CALL_TRACE: bool = TRACE || true;

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum JmpType {
    Jmp,
    True,
    False,
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    F2I,
    I2F,
    PushInt(isize),
    PushFunction {
        namespaceId: u32,
        functionId: u32,
    },
    PushIntOne,
    PushIntZero,
    PushFloat(f64),
    PushBool(bool),
    PushChar(char),
    Pop,
    Dup,
    Swap,
    GetLocal {
        index: usize,
    },
    SetLocal {
        index: usize,
    },
    Jmp {
        offset: i32,
        jmpType: JmpType,
    },
    SCall {
        id: usize,
    },
    LCall {
        namespace: u32,
        id: u32,
    },
    DynamicCall {
        returns: bool,
        argsCount: usize,
    },
    Return,

    Add(RawDataType),
    Sub(RawDataType),
    Div(RawDataType),
    Mul(RawDataType),
    Modulo(RawDataType),

    Equals(RawDataType),
    Greater(RawDataType),
    Less(RawDataType),

    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    Xor,

    Or,
    And,
    Not,
    New {
        namespaceID: u32,
        structID: u32,
    },
    GetField {
        fieldID: usize,
    },
    SetField {
        fieldID: usize,
    },
    ArrayNew,
    ArrayStore,
    ArrayLoad,
    ArrayLength,
    StringLength,
    StrNew(usize),
    GetChar,
    Inc {
        typ: RawDataType,
        index: u32,
    },
    Dec {
        typ: RawDataType,
        index: u32,
    },
    SetGlobal {
        namespaceID: u32,
        globalID: u32,
    },
    GetGlobal {
        namespaceID: u32,
        globalID: u32,
    },
    MulInt,
    AddInt,
    SubInt,
    LessInt,
    GetLocalZero,
    SetLocalZero,
    IsStruct {
        namespaceId: usize,
        structId: usize,
    },
    PushNull,
    AssertNotNull,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub returnType: Option<DataType>,
    pub varTable: Box<[VariableMetadata]>,
    pub argAmount: usize,
    pub typ: FuncType,
}

pub type ExternFn = extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> ();
pub type BuiltInFn = fn(&mut VirtualMachine, &mut StackFrame) -> ();

#[derive(Clone, Copy)]
pub enum FuncType {
    Runtime { rangeStart: usize, rangeStop: usize },
    Builtin { callback: BuiltInFn },
    Extern { callback: ExternFn },
}

impl Debug for FuncType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Runtime {
                rangeStart,
                rangeStop: _,
            } => {
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
#[repr(C)]
pub struct VirtualMachine {
    nativeWrapper: NativeWrapper,

    stack: Vec<Value>,
    heap: Heap,

    frames: Vec<StackFrame>,
    namespaces: FastAccess<String, Namespace>,

    jitCompiler: JITCompiler,

    lexingUnits: &'static [Box<dyn LexingUnit<TokenType>>],
    parsingUnits: &'static [Box<dyn ParsingUnit<ASTNode, TokenType, VIPLParsingState>>],

    interruptMutex: Mutex<bool>,
}

unsafe impl Send for VirtualMachine {}

impl VirtualMachine {
    pub fn triggerInterrupt(&mut self) {
        let guard = self.interruptMutex.get_mut().unwrap();
        *guard = true;
    }

    pub fn allocate<T: Allocation>(&mut self, v: T) -> Hay<T> {
        self.heap.allocate(v)
    }

    pub fn frameCount(&self) -> usize {
        self.frames.len()
    }

    #[inline]
    pub fn allocateString(&mut self, st: String) -> Hay<ViplObject<Str>> {
        let a1 = Str::new(st);
        let aw = ViplObject::<Str>::str(a1);

        self.heap.allocate(aw)
    }

    pub fn getLocalString(&mut self, id: usize) -> Value {
        let n = self.currentNamespace();

        n.getString(id)
    }

    pub fn stackSize(&self) -> usize {
        self.stack.len()
    }

    #[inline]
    pub fn allocateArray(&mut self, vec: Vec<Value>) -> Hay<ViplObject<Array>> {
        let arr = Array {
            internal: vec
        };
        self.heap.allocate(ViplObject::<Array>::arr(arr))
    }

    #[inline]
    pub fn findNamespace(&self, name: &str) -> Result<(&Namespace, usize), CodeGenError> {
        let (namespace, namespaceId) =
            self.namespaces
                .getSlowStr(name)
                .ok_or_else(|| CodeGenError::SymbolNotFound(SymbolNotFoundE::namespace(
                    name,
                )))?;

        Ok((namespace, namespaceId))
    }

    #[inline]
    pub fn findNamespaceParts(
        &self,
        parts: &[String],
    ) -> Result<(&Namespace, usize), CodeGenError> {
        self.findNamespace(&genNamespaceName(parts))
    }

    #[inline]
    pub fn getNaughty(&mut self) -> Naughty<Self> {
        Naughty::new(self as *mut VirtualMachine)
    }

    #[inline]
    pub fn buildSymbolTable(&self, hints: &[ImportHints]) -> Result<SymbolManager, CodeGenError> {
        let mut table = SymbolManager::new();

        for hint in hints {
            match hint {
                ImportHints::Namespace(namespace, rename) => {
                    let (n, nId) = self.findNamespaceParts(namespace)?;

                    let name = match rename {
                        None => n.name.clone(),
                        Some(v) => v.clone()
                    };

                    for (fId, f) in n.getFunctions().iter().enumerate() {
                        table.registerFunction(format!("{}::{}", name, f.0.genName()), nId, fId, f.0.getArgs(), f.0.returnType.clone());
                    }

                    for (sId, s) in n.getStructs().iter().enumerate() {
                        table.registerStruct(format!("{}::{}", name, s.name), nId, sId, s.clone());
                    }

                    for (gId, s) in n.getGlobals().iter().enumerate() {
                        table.registerGlobal(format!("{}::{}", name, s.0.name), nId, gId, s.0.typ.clone());
                    }
                }
                ImportHints::Symbols(namespace, syms) => {
                    let (n, nId) = self.findNamespaceParts(namespace)?;

                    for (symName, symRename) in syms {
                        if symName == "*" {
                            for (fId, f) in n.getFunctions().iter().enumerate() {
                                table.registerFunction(f.0.genName(), nId, fId, f.0.getArgs(), f.0.returnType.clone());
                            }

                            for (sId, s) in n.getStructs().iter().enumerate() {
                                table.registerStruct(s.name.clone(), nId, sId, s.clone());
                            }

                            for (gId, s) in n.getGlobals().iter().enumerate() {
                                table.registerGlobal(s.0.name.clone(), nId, gId, s.0.typ.clone());
                            }
                            continue;
                        }

                        let name = match symRename {
                            None => symName,
                            Some(v) => v
                        };

                        if let Ok(v) = n.findStruct(symName) {
                            table.registerStruct(name.clone(), nId, v.1, v.0.clone());
                        }
                        if let Ok(v) = n.findGlobal(symName) {
                            table.registerGlobal(name.clone(), nId, v.1, v.0.typ.clone())
                        }

                        for f in n.findFunctionsByName(symName) {
                            let argz = f.0.getArgs();
                            table.registerFunction(genFunName(&name, &argz), nId, f.1, argz, f.0.returnType)
                        }
                    }
                }
            }
        }

        Ok(table)
    }

    #[inline]
    pub fn registerNamespace(&mut self, namespace: Namespace) -> usize {
        let id = self
            .namespaces
            .insert(namespace.name.clone(), namespace)
            .unwrap();
        self.getNamespaceMut(id).id = id;

        id
    }

    #[inline]
    pub fn getNamespace(&self, id: usize) -> &Namespace {
        self.namespaces.getFast(id).unwrap()
    }

    #[inline]
    pub fn getNamespaceMut(&mut self, id: usize) -> &mut Namespace {
        self.namespaces.getFastMut(id).unwrap()
    }

    #[inline(always)]
    pub fn pop(&mut self) -> Value {
        let v = self
            .stack
            .pop()
            .unwrap();

        if TRACE {
            println!("poped {:?}", v);
        }

        v
    }

    #[inline(always)]
    pub fn popAmount(&mut self, amount: usize) {
        if DEBUG || TRACE {
            for _ in 0..amount {
                unsafe {
                    self
                        .stack
                        .pop()
                };
            }
        } else {
            let mut res =
                unsafe { &mut *(&mut self.stack as *mut Vec<Value> as *mut FastVec<Value>) };

            unsafe { res.size = res.size.unchecked_sub(amount) };
        }
    }

    #[inline(always)]
    pub fn push(&mut self, value: Value) {
        if TRACE {
            println!("pushed {:?}", value);
        }
        self.stack.push(value)
    }

    #[inline]
    pub fn gc(&mut self) {
        let mut collector = HayCollector::new(&self.heap.allocations);

        for frame in &self.frames {
            for v in &self.stack {
                if self.heap.allocations.contains(&(v.asUnsigned())) {
                    match &v.asRefMeta().objectType {
                        ObjectType::Simple(len) => {
                            todo!()
                        }
                        ObjectType::Native(v) => {
                            todo!()
                        }
                    }
                }
            }

            frame.collect(self, &mut collector);
        }

        self.heap.gc(collector.visited);
    }

    #[inline(always)]
    pub fn getMutTop(&mut self) -> &mut Value {
        let s = self.stack.len();

        if s == 0 {
            panic!()
        }

        unsafe {
            (*(&mut self.stack as *mut Vec<Value>)).get_unchecked_mut(s - 1)
        }
    }

    #[inline(always)]
    pub fn getTop(&self) -> &Value {
        let s = self.stack.len();
        unsafe { self.stack.get_unchecked(s - 1) }
    }
    #[inline(always)]
    pub fn getFrame(&self) -> &StackFrame {
        if DEBUG || TRACE {
            self.frames.last().unwrap()
        } else {
            unsafe {
                self.frames
                    .get_unchecked(self.frames.len().unchecked_sub(1))
            }
        }
    }

    #[inline(always)]
    pub fn currentNamespace(&self) -> &Namespace {
        let f = self.getFrame();

        self.getNamespace(f.namespaceId)
    }

    #[inline(always)]
    pub fn getMutFrame(&mut self) -> &mut StackFrame {
        let l = self.frames.len();
        unsafe {
            self
                .frames
                .get_unchecked_mut(l.unchecked_sub(1))
        }
    }

    #[inline(always)]
    pub fn nextOpcode2<'a>(&'a mut self, ops: &'a [OpCode]) -> (Option<&OpCode>, usize) {
        let x = self.getMutFrame();
        let a = x.programCounter;
        unsafe {
            x.programCounter = x.programCounter.unchecked_add(1);
        }
        (ops.get(a), a)
    }

    #[inline]
    pub fn getIndex(&self) -> usize {
        unsafe {
            self.frames
                .get_unchecked(self.frames.len() - 1)
                .programCounter
        }
    }

    #[inline]
    pub fn setIndex(&mut self, index: usize) {
        let i = self.frames.len() - 1;
        unsafe { self.frames.get_unchecked_mut(i).programCounter = index }
    }

    #[inline]
    pub fn getLocal(&self, index: usize) -> &Value {
        let f = self.getFrame();
        f.getRef(index)
    }

    #[inline]
    pub fn getMutLocal(&mut self, usize: usize) -> &mut Value {
        let i = self.frames.len() - 1;
        unsafe {
            self.frames
                .get_unchecked_mut(i)
                .getRefMut(usize)
        }
    }

    #[inline]
    pub fn setLocal(&mut self, index: usize, value: Value) {
        let i = self.frames.len() - 1;
        unsafe { *self.frames.get_unchecked_mut(i).getRefMut(index) = value }
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

    #[inline(always)]
    pub fn pushFrame(&mut self, frame: StackFrame) {
        if unlikely(self.frames.len() > 2048) {
            panic!("stack overflow")
        }
        self.frames.push(frame);
    }

    #[inline(always)]
    pub fn popFrame(&mut self) {
        self.frames.pop();
    }

    #[inline(always)]
    pub fn call(&mut self, namespaceId: usize, functionId: usize) {
        let mut mother = self.getNaughty();
        let vm = mother.getMut();

        let namespace = self.getNamespace(namespaceId);
        let (fMeta, f) = namespace.getFunction(functionId);
        for _ in 0..fMeta.localsMeta.len() - fMeta.argsCount {
            vm.push(Value::null());
        }

        if CALL_TRACE {
            println!("[call] {} {:?} args count {}", fMeta.name, fMeta.localsMeta, fMeta.argsCount);
        }

        let res = if !vm.stack.is_empty() {
            vm.stack.len() - fMeta.localsMeta.len()
        } else {
            0
        };

        let fs = StackFrame {
            // FIXME
            localVariables: unsafe { vm.stack.as_mut_ptr().add(res) },
            programCounter: 0,
            namespaceId,
            functionId,
        };

        let x = f.as_ref().unwrap();

        let ret = x.call(vm, fs, fMeta.returns());

        vm.popAmount(fMeta.localsMeta.len());

        if fMeta.returns {
            self.push(ret)
        }
    }

    #[inline]
    pub fn execute(&mut self, opCodes: &[OpCode], returns: bool) -> Value {
        let mother = self as *mut VirtualMachine;

        let vm = unsafe { &mut *mother };
        let vm1 = unsafe { &mut *mother };

        loop {
            if let Ok(mut guard) = vm1.interruptMutex.try_lock() && *guard {
                println!("[INTERRUPTED]");
                *guard = false;
            }
            let (op1, _) = (*vm1).nextOpcode2(opCodes);

            let op = match op1 {
                None => panic!("F"),
                Some(v) => v,
            };

            if TRACE {
                match op {
                    SCall { id } => {
                        println!("evaluating {:?} {}", op, self.currentNamespace().getFunction(*id).0.genName());
                    }
                    LCall { namespace, id } => {
                        println!("evaluating {:?} {}", op, self.getNamespace(*namespace as usize).getFunction(*id as usize).0.genName());
                    }
                    _ => println!("evaluating {:?} {}", op, self.getFrame().programCounter),
                }
            }

            match op {
                F2I => self.getMutTop().f2i(),
                I2F => self.getMutTop().i2f(),
                PushInt(v) => self.push(Value { Num: *v }),
                PushIntOne => self.push(Value { Num: 1 }),
                PushIntZero => self.push(Value { Num: 0 }),
                PushFloat(v) => self.push((*v).into()),
                PushBool(v) => self.push((*v).into()),
                Pop => {
                    self.pop();
                }
                Dup => {
                    let val = self.getTop();
                    self.push(*val);
                }
                GetLocal { index } => self.push(*self.getLocal(*index)),
                SetLocal { index } => {
                    let x = self.pop();
                    self.setLocal(*index, x);
                }
                Jmp { offset, jmpType } => match jmpType {
                    JmpType::Jmp => {
                        let x = *offset;
                        self.seek(x as isize);
                    }
                    JmpType::True => {
                        let a = self.pop();
                        if a.getBool() {
                            let x = *offset;
                            self.seek(x as isize)
                        }
                    }
                    JmpType::False => {
                        let a = self.pop();
                        if !a.getBool() {
                            self.seek(*offset as isize)
                        }
                    }
                },
                Return => return if returns { self.pop() } else { Value::null() },
                Add(v) => unsafe {
                    let a = self.pop();
                    self.getMutTop().add(
                        a,
                        &v.toType(),
                        vm1,
                    );
                },
                Sub(v) => {
                    let a = self.pop();
                    self.getMutTop().sub(&a, &v.toType());
                }
                Div(v) => {
                    let a = self.pop();
                    self.getMutTop().div(&a, &v.toType());
                }
                Mul(v) => {
                    let a = self.pop();
                    self.getMutTop().mul(&a, &v.toType());
                }
                Equals(v) => {
                    let a = self.pop();
                    self.getMutTop().refEq(&a, &v.toType());
                }
                Greater(v) => {
                    let a = self.pop();
                    self.getMutTop().refGt(&a, &v.toType());
                }
                Less(v) => {
                    let a = self.pop();
                    self.getMutTop().refLess(&a, &v.toType());
                }
                Or => {
                    let a = self.pop();
                    self.getMutTop().or(&a);
                }
                And => {
                    let a = self.pop();
                    self.getMutTop().and(&a);
                }
                Not => {
                    self.getMutTop().not();
                }
                ArrayNew => {
                    let size = self.pop();
                    let a = Value::makeArray(vec![Value::null(); size.asNum() as usize], self);
                    self.push(a)
                }
                ArrayStore => {
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
                ArrayLoad => {
                    let index = self.pop().getNum();
                    let mut value1 = self.pop();
                    let arr = value1.getMutArray();
                    self.push(*arr.internal.get(index as usize).unwrap());
                }
                ArrayLength => {
                    self.push(Value {
                        Num: vm.pop().getReference::<Array>().data.internal.len() as isize,
                    });
                }
                // FIXME inc is slower than executing: pushLocal, PushOne, Add, SetLocal
                Inc { typ, index } => unsafe { self.getMutLocal(*index as usize).inc(&typ.toType()) },
                Dec { typ, index } => unsafe { self.getMutLocal(*index as usize).dec(&typ.toType()) },
                PushChar(c) => self.push((*c).into()),
                StrNew(sId) => {
                    let s = self.currentNamespace();

                    self.push(s.getString(*sId))
                }
                GetChar => {
                    let index = self.pop().getNum();

                    let opIndex = self.stack.len() - 1;

                    let r = self.stack.get_mut(opIndex).unwrap();
                    let c = *r.getString().as_bytes().get(index as usize).unwrap() as char;
                    *r = c.into();
                }
                SCall { id } => {
                    let frame = self.getFrame();

                    (*vm).call(frame.namespaceId, *id)
                }
                DynamicCall { .. } => {
                    let (namespaceRaw, idRaw) = self.pop().asFunction();
                    let namespace = namespaceRaw as usize;
                    let id = idRaw as usize;

                    vm.call(namespace, id)
                }
                LCall { namespace, id } => vm.call(*namespace as usize, *id as usize),
                PushFunction { namespaceId, functionId } => {
                    self.push(Value::makeFunction(*namespaceId, *functionId));
                }
                New {
                    namespaceID,
                    structID,
                } => unsafe {
                    let alloc = self.allocateObject(*namespaceID as usize, *structID as usize);

                    self.push(Value::from(alloc as usize))
                },
                SetField {
                    fieldID,
                } => {
                    let value = self.pop();
                    let obj = self.pop();

                    Self::setField(obj, *fieldID, value)
                }
                GetField {
                    fieldID,
                } => {
                    let obj = self.pop();

                    let v = Self::getField(obj, *fieldID);

                    self.push(v);
                }
                Swap => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(a);
                    self.push(b);
                }
                StringLength => {
                    let a = self.pop();
                    let len = a.getString().len();
                    self.push(len.into());
                }
                SetGlobal {
                    namespaceID,
                    globalID,
                } => {
                    let v = self.pop();
                    let n = self.getNamespaceMut(*namespaceID as usize);
                    let g = n.getGlobalMut(*globalID as usize);

                    g.1 = v;
                }
                GetGlobal {
                    namespaceID,
                    globalID,
                } => {
                    let n = self.getNamespace(*namespaceID as usize);

                    self.push(n.getGlobal(*globalID as usize).1)
                }
                MulInt => {
                    let a = self.pop();
                    *self.getMutTop().getRefNum() *= a.getNum();
                }
                AddInt => {
                    let a = self.pop();
                    *self.getMutTop().getRefNum() += a.getNum();
                }
                SubInt => {
                    let a = self.pop();
                    *self.getMutTop().getRefNum() -= a.getNum();
                }
                LessInt => {
                    let a = self.pop();
                    *self.getMutTop() = (self.getTop().getNumRef() > a.getNumRef()).into()
                }
                SetLocalZero => {
                    let x = self.pop();
                    self.setLocal(0, x);
                }
                GetLocalZero => self.push(*self.getLocal(0)),
                Modulo(t) => {
                    let a = self.pop();
                    self.getMutTop().modulo(&a, &t.toType());
                }
                IsStruct { namespaceId, structId } => {
                    let v = self.pop().toRefMeta();

                    self.push((v.structId == *structId && v.namespaceId == *namespaceId).into())
                }
                BitwiseAnd => {
                    let a = self.pop();
                    self.getMutTop().bitwiseAnd(a);
                }
                BitwiseOr => {
                    let a = self.pop();
                    self.getMutTop().bitwiseOr(a);
                }
                Xor => {
                    let a = self.pop();
                    self.getMutTop().bitwiseXor(a);
                }
                BitwiseNot => {
                    self.getMutTop().bitwiseNot();
                }
                ShiftRight => {
                    let a = self.pop();
                    self.getMutTop().shiftRight(a);
                }
                ShiftLeft => {
                    let a = self.pop();
                    self.getMutTop().shiftLeft(a);
                }
                AssertNotNull => {
                    if self.pop().getNum() == 0 {
                        panic!()
                    }
                }
                PushNull => {
                    self.push(0.into())
                }
                o => {
                    if DEBUG || TRACE {
                        panic!("unimplemented opcode {:?}", o)
                    } else {
                        unsafe { unreachable_unchecked() }
                    }
                }
            }
        }
    }

    pub fn setField(obj: Value, fId: usize, value: Value) {
        unsafe {
            let ptr =
                ((obj.asRefMeta() as *const UntypedObject).add(1)) as *const Value;
            let p = ptr.add(fId) as *mut Value;

            *p = value;
        }
    }

    pub fn getField(obj: Value, fId: usize) -> Value {
        unsafe {
            let ptr = obj.asRefMeta() as *const UntypedObject;

            let ptr2 = ptr.add(1) as *const Value;

            let p = ptr2.add(fId);

            p.read()
        }
    }

    pub fn allocateObject(&mut self, nId: usize, sId: usize) -> *mut UntypedObject {
        let n = self.getNamespace(nId);

        let s = n.getStruct(sId);

        let mut l = Layout::new::<()>();

        l = l
            .extend(Layout::array::<ViplObjectMeta<()>>(1).unwrap())
            .unwrap()
            .0;
        l = l
            .extend(Layout::array::<Value>(s.fieldCount()).unwrap())
            .unwrap()
            .0;

        let alloc = unsafe {
            let alloc = alloc(l) as *mut UntypedObject;

            (*alloc).objectType = ObjectType::Simple(s.fieldCount());
            (*alloc).structId = sId;
            (*alloc).namespaceId = nId;

            alloc
        };

        if DEBUG {
            println!("[vm] allocated {}:{} {} bytes", nId, sId, l.size())
        }

        alloc
    }

    pub fn link(&mut self, h: fn(&mut StatementCtx<SymbolicOpcode>, DataType)) -> Result<(), Vec<CodeGenError>> {
        let mut mother = self.getNaughty();
        let mut mother2 = self.getNaughty();
        let warCrime: &mut UnsafeCell<VirtualMachine> = unsafe { transmute(self) };

        unsafe {
            for n in &mut (mother.getMut()).namespaces.actual {
                if n.state == Loaded || n.state == FailedToLoad {
                    continue;
                }

                let mut symbols = mother2.getMut().buildSymbolTable(n.getImportHintsMut()).map_err(|it| vec![it])?;

                for (fId, (fMeta, _)) in n.getFunctions().iter().enumerate() {
                    let argz = fMeta.getArgs();
                    symbols.registerFunction(fMeta.genName(), n.id, fId, argz, fMeta.returnType.clone())
                }
                for (sId, s) in n.getStructs().iter().enumerate() {
                    symbols.registerStruct(s.name.clone(), n.id, sId, s.clone());
                }
                for (gId, (g, _)) in n.getGlobals().iter().enumerate() {
                    symbols.registerGlobal(g.name.clone(), n.id, gId, g.typ.clone());
                }

                let anotherWarCrime: &mut UnsafeCell<Namespace> = transmute(n);

                for (_, g) in (*anotherWarCrime.get())
                    .getGlobalsMut()
                    .iter_mut()
                    .enumerate()
                {
                    let mut a = Default::default();
                    let mut b = vec![];
                    let mut converter = ImplicitConverter{ rules: implicitConversionRules() };

                    let mut simpleCtx = SimpleCtx::new(
                        anotherWarCrime,
                        warCrime,
                        h,
                        &mut symbols,
                        &mut a,
                        &mut b,
                        & converter
                    );

                    let mut ctx: ExpressionCtx<SymbolicOpcode> = ExpressionCtx {
                        exp: &g.0.default,
                        ctx: simpleCtx,
                    };
                    g.0.typ = ctx.toDataType().map_err(|it| vec![it])?;
                }

                let nId = (*anotherWarCrime.get()).id;

                for (index, (f, a)) in (*anotherWarCrime.get())
                    .getFunctionsMut()
                    .iter_mut()
                    .enumerate()
                {
                    if let FunctionTypeMeta::Runtime(_) = f.functionType {
                        symbols.enterScope();

                        for local in f.localsMeta.iter() {
                            symbols.registerLocal(&local.name, local.typ.clone());
                        }

                        let mut ops = vec![];
                        let implicitConversion = ImplicitConverter{ rules: implicitConversionRules() };

                        let mut manager = Default::default();
                        let mut ctx = SimpleCtx::new(
                            anotherWarCrime,
                            warCrime,
                            h,
                            &mut symbols,
                            &mut manager,
                            &mut ops,
                            &implicitConversion
                        );

                        match genFunctionDef(f, &mut ctx) {
                            Ok(v) => {}
                            Err(e) => {
                                anotherWarCrime.get_mut().state = FailedToLoad;
                                return Err(e);
                            }
                        };


                        let cLocals = ctx.getLocals();
                        if cLocals.len() > f.localsMeta.len() {
                            let mut buf = f.localsMeta.clone().into_vec();

                            let s = f.localsMeta.len();
                            let e = cLocals.len();

                            for i in s..e {
                                buf.push(cLocals[i].clone())
                            }

                            f.localsMeta = buf.into_boxed_slice();
                        }

                        if DEBUG {
                            println!("[generated] N: {}, F: {} {} {:?} {:?}", nId, index, f.name, ops, f);
                        }

                        if OPTIMIZE {
                            ops = transform(ops, |it| {
                                let r = constEvaluation(it);
                                optimizeBytecode(r)
                            });

                            ops  = branchOmit(ops).0;
                        }


                        if DEBUG {
                            println!("[optimized] N: {}, F: {} {} {:?}", nId, index, f.name, ops);
                            println!("locals meta{:?}", f.localsMeta);
                        }

                        if false {
                            let nf = warCrime.get_mut().jitCompiler.compile(&ops, mother2.getMut(), anotherWarCrime.get_mut(), f.returns());
                            *a = Some(Native(nf));
                        } else {
                            let opt = emitOpcodes(ops).map_err(|it| vec![it])?;
                            *a = Some(LoadedFunction::Virtual(opt));
                        }

                        symbols.exitScope();
                    }
                }

                (*anotherWarCrime.get()).state = Loaded;
            }
        }
        Ok(())
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn loadNamespace(&mut self, path: &str, name: &[String]) -> Result<usize, LoadFileError<TokenType>> {
        // TODO create more complex logic for loading namespaces
        let file = fs::read_to_string(path).unwrap();

        self.loadNamespaceFromString(&file, name)
    }

    pub fn loadNamespaceFromString(&mut self, data: &str, name: &[String]) -> Result<usize, LoadFileError<TokenType>> {
        let res = loadSourceFile(&data, &self.lexingUnits, &self.parsingUnits)?;

        let n = Namespace::constructNamespace(res, &genNamespaceName(name), self, vec![]);

        Ok(self.registerNamespace(n))
    }

    pub fn getLexingUnits(&self) -> &[Box<dyn LexingUnit<TokenType>>] {
        &self.lexingUnits
    }

    pub fn getParsingUnits(&self) -> &[Box<dyn ParsingUnit<ASTNode, TokenType, VIPLParsingState>>] {
        &self.parsingUnits
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self {
            stack: Vec::with_capacity(128),
            nativeWrapper: NativeWrapper::new(),
            heap: Default::default(),
            frames: Vec::with_capacity(32),
            namespaces: Default::default(),
            jitCompiler: Default::default(),
            lexingUnits: getLexingUnits(),
            parsingUnits: getParsingUnits(),
            interruptMutex: Mutex::new(false),
        }
    }
}

impl Drop for VirtualMachine {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}
