use std::alloc::{alloc, dealloc, Layout};
use std::cell::{RefCell, UnsafeCell};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::hint::unreachable_unchecked;
use std::intrinsics::{read_via_copy, unlikely};
use std::mem::{size_of, transmute};
use std::{intrinsics, ptr};

use crate::asm::jitCompiler::JITCompiler;
use crate::ast::FunctionDef;
use crate::bytecodeGen::{
    emitOpcodes, genFunctionDef, ExpressionCtx, SimpleCtx, StatementCtx, SymbolicOpcode,
};
use crate::errors::{CodeGenError, Errorable, SymbolNotFoundE, SymbolType};
use crate::fastAccess::FastAcess;
use crate::ffi::NativeWrapper;
use crate::symbolManager::SymbolManager;
use crate::utils::{readNeighbours, transform, FastVec, genFunName};
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::{Int, Void};
use crate::vm::heap::{Allocation, Hay, HayCollector, Heap};
use crate::vm::namespace::LoadedFunction::Native;
use crate::vm::namespace::NamespaceState::{FailedToLoad, Loaded};
use crate::vm::namespace::{FunctionTypeMeta, GlobalMeta, LoadedFunction, Namespace};
use crate::vm::namespaceLoader::NamespaceLoader;
use crate::vm::nativeObjects::{
    ObjectType, SimpleObjectWrapper, UntypedObject, ViplObject, ViplObjectMeta,
};
use crate::vm::nativeStack::StackManager;
use crate::vm::objects::{Array, Str};
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
    Symbols(Vec<String>, Vec<(String, Option<String>)>)
}

// FIXME DEBUG is faster than default
const DEBUG: bool = false;
const TRACE: bool = false;

#[derive(Clone, Debug, PartialEq, Copy)]
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
pub enum OpCode {
    F2I,
    I2F,
    PushInt(isize),
    PushFunction(u32, u32),
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
        offset: isize,
        jmpType: JmpType,
    },
    SCall {
        id: usize,
    },
    LCall {
        namespace: usize,
        id: usize,
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
        structID: usize,
    },
    GetField {
        namespaceID: usize,
        structID: usize,
        fieldID: usize,
    },
    SetField {
        namespaceID: usize,
        structID: usize,
        fieldID: usize,
    },
    ArrayNew(DataType),
    ArrayStore,
    ArrayLoad,
    ArrayLength,
    StringLength,
    StrNew(usize),
    GetChar,
    Inc {
        typ: DataType,
        index: usize,
    },
    Dec {
        typ: DataType,
        index: usize,
    },
    SetGlobal {
        namespaceID: usize,
        globalID: usize,
    },
    GetGlobal {
        namespaceID: usize,
        globalID: usize,
    },
    ConstStr(usize),
    MulInt,
    AddInt,
    SubInt,
    LessInt,
    GetLocalZero,
    SetLocalZero,
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

    stackManager: StackManager<2048>,

    frames: Vec<StackFrame>,
    namespaces: FastAcess<String, Namespace>,

    namespaceLoader: NamespaceLoader,
    jitCompiler: JITCompiler,

    handleStatementExpression: fn(&mut StatementCtx, DataType),
}

impl VirtualMachine {
    pub fn setHandleExpression(&mut self, f: fn(&mut StatementCtx, DataType)) {
        self.handleStatementExpression = f;
    }

    pub fn allocate<T: Allocation>(&mut self, v: T) -> Hay<T> {
        self.heap.allocate(v)
    }

    pub fn frameCount(&self) -> usize {
        self.frames.len()
    }

    #[inline]
    pub fn allocateString(&mut self, st: &str) -> Hay<ViplObject<Str>> {
        let a1 = Str::new(String::from(st));
        let aw = ViplObject::<Str>::str(a1);

        self.heap.allocate(aw)
    }

    pub fn stackSize(&self) -> usize {
        self.stack.len()
    }

    #[inline]
    pub fn allocateArray(&mut self, vec: Vec<Value>, t: DataType) -> Hay<ViplObject<Array>> {
        let arr = Array {
            internal: vec,
            typ: t,
        };
        self.heap.allocate(ViplObject::<Array>::arr(arr))
    }

    #[inline]
    pub fn findNamespace(&self, name: &str) -> Result<(&Namespace, usize), CodeGenError> {
        let (namespace, namespaceId) =
            self.namespaces
                .getSlowStr(name)
                .ok_or(CodeGenError::SymbolNotFound(SymbolNotFoundE::namespace(
                    name,
                )))?;

        Ok((namespace, namespaceId))
    }

    #[inline]
    pub fn findNamespaceParts(
        &self,
        parts: &[String],
    ) -> Result<(&Namespace, usize), CodeGenError> {
        let namespaceName = parts.join("::");

        self.findNamespace(&namespaceName)
    }

    #[inline]
    pub fn rawPtr(&self) -> *mut VirtualMachine {
        self as *const VirtualMachine as *mut VirtualMachine
    }

    #[inline]
    pub fn buildSymbolTable(&self, hints: &[ImportHints]) -> Result<SymbolManager, CodeGenError> {
        let mut table = SymbolManager::new();

        for hint in hints {
            match hint {
                ImportHints::Namespace(namespace, rename) => {
                    let n = self.findNamespaceParts(&namespace)?;

                    let name = match rename {
                        None => n.0.name.clone(),
                        Some(v) => v.clone()
                    };

                    for (fId, f) in n.0.getFunctions().iter().enumerate() {
                        table.registerFunction(format!("{}::{}", name, f.0.genName()), n.1, fId, f.0.getArgs(), f.0.returnType.clone());
                    }

                    for (sId, s) in n.0.getStructs().iter().enumerate() {
                        table.registerStruct(format!("{}::{}", name, s.name), n.1, sId, s.clone());
                    }

                    for (gId, s) in n.0.getGlobals().iter().enumerate() {
                        table.registerGlobal(format!("{}::{}", name, s.0.name), n.1, gId, s.0.typ.clone());
                    }
                }
                ImportHints::Symbols(namespace, syms) => {
                    let (n, nId) = self.findNamespaceParts(&namespace)?;

                    for (symName, symRename) in syms {
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
    pub fn pop(&self) -> Value {
        unsafe {
            (&mut *(self as *const VirtualMachine as *mut VirtualMachine))
                .stack
                .pop()
                .unwrap()
        }
    }

    #[inline(always)]
    pub fn popAmount(&self, amount: usize) {
        if DEBUG || TRACE {
            for _ in 0..amount {
                unsafe {
                    (&mut *(self as *const VirtualMachine as *mut VirtualMachine))
                        .stack
                        .pop()
                };
            }
        } else {
            let mut res =
                unsafe { &mut *(&self.stack as *const Vec<Value> as *mut FastVec<Value>) };

            unsafe { res.size = res.size.unchecked_sub(amount) };
        }
    }

    #[inline(always)]
    pub fn push(&mut self, value: Value) {
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
    pub fn getMutTop(&self) -> &mut Value {
        let s = self.stack.len();
        unsafe {
            (&mut *(&self.stack as *const Vec<Value> as *mut Vec<Value>)).get_unchecked_mut(s - 1)
        }
    }

    #[inline(always)]
    pub fn getTop(&self) -> &Value {
        let s = self.stack.len();
        unsafe { self.stack.get_unchecked(s - 1) }
    }
}

impl VirtualMachine {
    #[inline(always)]
    pub fn getFrame(&self) -> &StackFrame {
        if DEBUG || TRACE {
            self.frames.get(self.frames.len() - 1).unwrap()
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
    pub fn getMutFrame(&self) -> &mut StackFrame {
        unsafe {
            (&mut *(self as *const VirtualMachine as *mut VirtualMachine))
                .frames
                .get_unchecked_mut(self.frames.len().unchecked_sub(1))
        }
    }

    #[inline(always)]
    pub fn nextOpcode2<'a>(&'a self, ops: &'a [OpCode]) -> (Option<&OpCode>, usize) {
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
    pub fn getMutLocal(&self, usize: usize) -> &mut Value {
        let i = self.frames.len() - 1;
        unsafe {
            (&mut *(&self.frames as *const Vec<StackFrame> as *mut Vec<StackFrame>))
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
        let namespace = self.getNamespace(namespaceId);
        let (fMeta, f) = namespace.getFunction(functionId);
        let vm = unsafe { &mut *(self as *const VirtualMachine as *mut VirtualMachine) };

        for _ in 0..fMeta.localsMeta.len() - fMeta.argsCount {
            vm.push(Value::null());
        }

        if TRACE {
            println!("[call] {} {:?} args count {}", fMeta.name, fMeta.localsMeta, fMeta.argsCount);
        }

        let res = if vm.stack.len() > 0 {
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

        self.popAmount(fMeta.localsMeta.len());

        if fMeta.returns {
            self.push(ret)
        }
    }

    #[inline]
    pub fn execute(&mut self, opCodes: &[OpCode], returns: bool) -> Value {
        let vm = unsafe { &mut *(self as *const VirtualMachine as *mut VirtualMachine) };
        let vm1 = unsafe { &mut *(self as *const VirtualMachine as *mut VirtualMachine) };

        loop {
            let (op1, _) = (&mut *vm1).nextOpcode2(opCodes);

            let op = match op1 {
                None => panic!("F"),
                Some(v) => v,
            };

            if TRACE {
                println!("evaluating {:?}", op);
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
                            self.seek(*offset)
                        }
                    }
                },
                Return => return if returns { self.pop() } else { Value::null() },
                Add(v) => unsafe {
                    let a = self.pop();
                    self.getMutTop().add(
                        a,
                        v,
                        &mut *(self as *const VirtualMachine as *mut VirtualMachine),
                    );
                },
                Sub(v) => {
                    let a = self.pop();
                    self.getMutTop().sub(&a, v);
                }
                Div(v) => {
                    let a = self.pop();
                    self.getMutTop().div(&a, v);
                }
                Mul(v) => {
                    let a = self.pop();
                    self.getMutTop().mul(&a, v);
                }
                Equals(v) => {
                    let a = self.pop();
                    self.getMutTop().refEq(&a, v);
                }
                Greater(v) => {
                    let a = self.pop();
                    self.getMutTop().refGt(&a, v);
                }
                Less(v) => {
                    let a = self.pop();
                    self.getMutTop().refLess(&a, v);
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
                ArrayNew(d) => {
                    let _size = self.pop();
                    let a = Value::makeArray(vec![], d.clone(), self);
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
                        Num: self.pop().getReference::<Array>().data.internal.len() as isize,
                    });
                }
                // FIXME inc is slower than executing: pushLocal, PushOne, Add, SetLocal
                Inc { typ, index } => unsafe { self.getMutLocal(*index).inc(typ) },
                Dec { typ, index } => unsafe { self.getMutLocal(*index).dec(typ) },
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

                    (&mut *vm).call(frame.namespaceId, *id)
                }
                DynamicCall => {
                    let (namespaceRaw, idRaw) = self.pop().asFunction();
                    let namespace = namespaceRaw as usize;
                    let id = idRaw as usize;

                    vm.call(namespace, id)
                }
                LCall { namespace, id } => vm.call(*namespace, *id),
                PushFunction(namespaceID, functionID) => {
                    self.push(Value::makeFunction(*namespaceID, *functionID));
                }
                New {
                    namespaceID,
                    structID,
                } => unsafe {
                    let n = self.getNamespace(*namespaceID);
                    let s = n.getStruct(*structID);

                    let mut l = Layout::new::<()>();

                    l = l
                        .extend(Layout::array::<ViplObjectMeta<()>>(1).unwrap())
                        .unwrap()
                        .0;
                    l = l
                        .extend(Layout::array::<Value>(s.fieldCount()).unwrap())
                        .unwrap()
                        .0;

                    let alloc = alloc(l) as *mut UntypedObject;

                    (*alloc).objectType = ObjectType::Simple(s.fieldCount());
                    (*alloc).structId = *structID;
                    (*alloc).namespaceId = *namespaceID;

                    self.push(Value::from(alloc as usize))
                },
                SetField {
                    namespaceID,
                    structID,
                    fieldID,
                } => unsafe {
                    let value = self.pop();

                    let ptr =
                        ((self.pop().asRefMeta() as *const UntypedObject).add(1)) as *const Value;
                    let p = ptr.add(*fieldID) as *mut Value;

                    *p = value;
                },
                GetField {
                    namespaceID,
                    structID,
                    fieldID,
                } => unsafe {
                    let ptr = self.pop().asRefMeta() as *const UntypedObject;

                    let ptr2 = ptr.add(1) as *const Value;

                    let p = ptr2.add(*fieldID) as *const Value;

                    self.push(p.read());
                },
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
                    let n = self.getNamespaceMut(*namespaceID);
                    let g = n.getGlobalMut(*globalID);

                    g.1 = v;
                }
                GetGlobal {
                    namespaceID,
                    globalID,
                } => {
                    let n = self.getNamespace(*namespaceID);

                    self.push(n.getGlobal(*globalID).1)
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
}

impl VirtualMachine {
    pub fn link(&mut self) -> Result<(), CodeGenError> {
        let warCrime: &mut UnsafeCell<VirtualMachine> = unsafe { transmute(self) };

        unsafe {
            for n in &mut (&mut *warCrime.get()).namespaces.actual {
                if n.state == Loaded || n.state == FailedToLoad {
                    continue;
                }

                let mut symbols = warCrime.get_mut().buildSymbolTable(n.getImportHints())?;

                for (fId, (fMeta, _)) in n.getFunctions().iter().enumerate() {
                    let argz = fMeta.getArgs();
                    symbols.registerFunction(fMeta.genName(), n.id, fId, argz, fMeta.returnType.clone())
                }

                let anotherWarCrime: &mut UnsafeCell<Namespace> = transmute(n);

                for (_, g) in (&mut *anotherWarCrime.get())
                    .getGlobalsMut()
                    .iter_mut()
                    .enumerate()
                {
                    let mut ctx = ExpressionCtx {
                        ops: &mut vec![],
                        exp: &g.0.default,
                        typeHint: None,
                        currentNamespace: anotherWarCrime,
                        vm: warCrime,
                        labelCounter: &mut 0,
                        symbols: &mut symbols,
                    };
                    g.0.typ = ctx.toDataType()?;
                }

                let nId = (&*anotherWarCrime.get()).id;

                for (index, (f, a)) in (&mut *anotherWarCrime.get())
                    .getFunctionsMut()
                    .iter_mut()
                    .enumerate()
                {
                    if let FunctionTypeMeta::Runtime(_) = f.functionType {
                        symbols.enterScope();

                        for local in f.localsMeta.iter() {
                            symbols.registerLocal(&local.name, local.typ.clone())
                        }

                        let mut ops = vec![];
                        let mut labelCounter = 0;

                        let mut ctx = SimpleCtx {
                            ops: &mut ops,
                            currentNamespace: anotherWarCrime,
                            handle: (&*warCrime.get()).handleStatementExpression,
                            vm: warCrime,
                            labelCounter: &mut labelCounter,
                            symbols: &mut symbols,
                        };


                        match genFunctionDef(f, &mut ctx) {
                            Ok(v) => {},
                            Err(e) => {
                                anotherWarCrime.get_mut().state = FailedToLoad;

                                return Err(e)
                            }
                        };


                        if ctx.symbols.locals.len() > f.localsMeta.len() {
                            let mut buf = f.localsMeta.clone().into_vec();

                            let s = f.localsMeta.len();
                            let e = ctx.symbols.locals.len();

                            for i in s..e {
                                buf.push(ctx.symbols.locals[i].clone())
                            }

                            f.localsMeta = buf.into_boxed_slice();
                        }

                        if DEBUG {
                            println!("[generated] N: {}, F: {} {} {:?} {:?}", nId, index, f.name, ops, f);
                        }

                        ops = transform(ops, |it| {
                            // let r = constEvaluation(it);
                            optimizeBytecode(it)
                        });

                        if DEBUG {
                            println!("[optimized] N: {}, F: {} {} {:?}", nId, index, f.name, ops);
                        }

                        let opt = emitOpcodes(ops);

                        // let nf = self.jitCompiler.compile(opt, &*v, &*nn, f.returns());
                        // *a = Some(Native(nf))

                        *a = Some(LoadedFunction::Virtual(opt));

                        symbols.exitScope();
                    }
                }

                (&mut *anotherWarCrime.get()).state = Loaded;
            }
        }
        Ok(())
    }

    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(128),
            nativeWrapper: NativeWrapper::new(),
            heap: Default::default(),
            stackManager: StackManager::new(),
            frames: vec![],
            namespaces: Default::default(),
            namespaceLoader: NamespaceLoader::new(),
            jitCompiler: JITCompiler {},
            handleStatementExpression: |it, t| {
                if t != Void {
                    it.push(Pop)
                }
            },
        }
    }
}

impl Drop for VirtualMachine {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}
