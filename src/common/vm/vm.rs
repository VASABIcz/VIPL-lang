use std::{intrinsics, ptr};
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::mem::size_of;

use libloading::os::unix::Library;

use crate::asm::jitCompiler::JITCompiler;
use crate::ast::FunctionDef;
use crate::bytecodeGen::{ExpressionCtx, genFunctionDef, StatementCtx};
use crate::errors::Errorable;
use crate::ffi::NativeWrapper;
use crate::utils::FastVec;
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::Void;
use crate::vm::heap::{Allocation, Hay, HayCollector, Heap};
use crate::vm::myStr::MyStr;
use crate::vm::namespace::{FunctionTypeMeta, GlobalMeta, LoadedFunction, Namespace};
use crate::vm::namespace::NamespaceState::Loaded;
use crate::vm::namespaceLoader::NamespaceLoader;
use crate::vm::nativeObjects::{ViplObjectMeta, ObjectType, SimpleObjectWrapper, UntypedObject, ViplObject};
use crate::vm::nativeStack::StackManager;
use crate::vm::objects::{Array, Str};
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::FuncType::{Builtin, Extern, Runtime};
use crate::vm::vm::OpCode::*;

const DEBUG: bool = true;
const TRACE: bool = false;

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
    StringLength,
    StrNew(MyStr),
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
        globalID: usize
    },
    GetGlobal {
        namespaceID: usize,
        globalID: usize
    },
    ConstStr(usize)
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

#[repr(C)]
#[derive(Debug)]
pub struct VirtualMachine {
    pub nativeWrapper: NativeWrapper,

    pub stack: Vec<Value>,
    pub nativeLibraries: Vec<Library>,
    pub heap: Heap,

    pub stackManager: StackManager<2048>,

    pub frames: Vec<StackFrame>,

    pub namespaceLookup: HashMap<String, usize>,
    pub namespaces: Vec<Namespace>,

    pub namespaceLoader: NamespaceLoader,
    pub jitCompiler: JITCompiler,

    pub handleStatementExpression: fn(&mut StatementCtx, DataType)
}

impl VirtualMachine {
    pub fn allocateString(&mut self, st: &str) ->  Hay<ViplObject<Str>> {
        let a1 = Str::new(String::from(st));
        let aw = ViplObject::<Str>::str(a1);


        self.heap.allocate(aw)
    }

    pub fn allocateArray(&mut self, vec: Vec<Value>, t: DataType) -> Hay<ViplObject<Array>> {
        let arr = Array{
            internal: vec,
            typ: t,
        };
        self.heap.allocate(ViplObject::<Array>::arr(arr))
    }

    pub fn findNamespace(&self, name: &str) -> Errorable<(&Namespace, usize)> {
        let namespaceId = self.namespaceLookup.get(name)
            .ok_or(format!("failed to find namespace {}", name))?;
        let namespace = self.namespaces.get(*namespaceId)
            .ok_or(format!("failed to find namespace with id {}", *namespaceId))?;

        Ok((namespace, *namespaceId))
    }

    pub fn findNamespaceParts(&self, parts: &[String]) -> Errorable<(&Namespace, usize)> {
        let namespaceName = parts.join("::");

        self.findNamespace(&namespaceName)
    }

    pub fn rawPtr(&self) -> *mut VirtualMachine {
        self as *const VirtualMachine as *mut VirtualMachine
    }

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

    pub fn getNamespace(&self, id: usize) -> &Namespace {
        self.namespaces.get(id).unwrap()
    }

    pub fn getNamespaceMut(&mut self, id: usize) -> &mut Namespace {
        self.namespaces.get_mut(id).unwrap()
    }

    #[inline(always)]
    pub fn pop(&self) -> Value {
        let mut res: &mut FastVec<Value> = unsafe { &mut *(&self.stack as *const Vec<Value> as *mut FastVec<Value>) };
        let mut buf: Value = Value{Num: 0};
        res.size -= 1;

        unsafe { ptr::copy(intrinsics::offset(res.ptr, res.size as isize) as *mut Value, &mut buf as *mut Value, 1); }

        buf
    }

    #[inline(always)]
    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

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
        unsafe { (&mut *(&self.stack as *const Vec<Value> as *mut Vec<Value>)).get_unchecked_mut(s - 1) }
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
        unsafe { (&mut *(&self.frames as *const Vec<StackFrame> as *mut Vec<StackFrame>)).get_unchecked_mut(i).localVariables.get_unchecked_mut(usize) }
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
        self.frames.push(frame);
    }

    #[inline]
    pub fn popFrame(&mut self) {
        self.frames.pop();
    }

    #[inline(always)]
    pub fn call(&mut self, argsCount: usize, localsCount: usize, namespaceId: usize, functionID: usize, function: &LoadedFunction) {
        let mut locals = vec![Value::from(0);localsCount];

        for i in 0..argsCount {
            let arg = self.pop();
            locals[(argsCount - 1) - i] = arg;
        }

        let mut fs = StackFrame{
            localVariables: locals.into_boxed_slice(),
            programCounter: 0,
            namespaceId: namespaceId,
            functionId: functionID,
        };

        function.call(self, fs)
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

            if TRACE {
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
                    PushIntOne => self.stack.push(Value { Num: 1 }),
                    PushIntZero => self.stack.push(Value { Num: 0 }),
                    PushFloat(v) => self.stack.push((*v).into()),
                    PushBool(v) => self.stack.push((*v).into()),
                    Pop => {
                        self.stack.pop();
                    }
                    Dup => unsafe {
                        let val = self.getTop();
                        self.stack.push(*val);
                    },
                    GetLocal { index } => {
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
                        self.stack.push(Value { Num: self.pop().getReference::<Array>().data.internal.len() as isize });
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

                        let namespace = self.getNamespace(frame.namespaceId);

                        let fMeta = namespace.functionsMeta.get(*id).unwrap();
                        let f = namespace.functions.get(*id).unwrap();

                        (&mut *r).call(fMeta.argsCount, fMeta.localsMeta.len(), frame.namespaceId, *id, f.as_ref().unwrap())

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

                        (&mut *r).call(fMeta.argsCount, fMeta.localsMeta.len(), namespace.id, id, f.as_ref().unwrap())
                    }
                    LCall { namespace, id} => {
                        let r = self as *const VirtualMachine as *mut VirtualMachine;
                        let frame = self.getFrame();
                        let namespace = self.namespaces.get(*namespace).unwrap();
                        let fMeta = namespace.functionsMeta.get(*id).unwrap();
                        let f = namespace.functions.get(*id).unwrap();

                        (&mut *r).call(fMeta.argsCount, fMeta.localsMeta.len(), namespace.id, *id, f.as_ref().unwrap())
                    }
                    PushFunction(namespaceID, functionID) => {
                        self.stack.push(Value::makeFunction(*namespaceID, *functionID));
                    }
                    New { namespaceID, structID } => unsafe {
                        let n = self.namespaces.get(*namespaceID).unwrap();
                        let s = n.structs.getFast(*structID).unwrap();

                        let mut l = Layout::new::<()>();

                        l = l.extend(Layout::array::<ViplObjectMeta<()>>(1).unwrap()).unwrap().0;
                        l = l.extend(Layout::array::<Value>(s.fields.len()).unwrap()).unwrap().0;

                        println!("allocating {} {}", l.size(), size_of::<ViplObjectMeta<()>>());

                        let alloc = alloc(l)  as *mut UntypedObject;

                        (*alloc).objectType = ObjectType::Simple(s.fields.len());
                        (*alloc).structId = *structID;
                        (*alloc).namespaceId = *namespaceID;

                        println!("allocated {:?}", alloc);

                        self.stack.push(Value::from(alloc as usize))
                    }
                    SetField { namespaceID, structID, fieldID } => {
                        let value = self.pop();

                        eprintln!("SetField {:?} {}", value, fieldID);

                        let ptr = ((self.pop().asRefMeta() as *const UntypedObject).add(1)) as *const Value;
                        let p = ptr.add(*fieldID) as *mut Value;

                        *p = value;
                    }
                    GetField { namespaceID, structID, fieldID } => {
                        let ptr = self.pop().asRefMeta() as *const UntypedObject;

                        eprintln!("GetField {:?} {}", ptr, fieldID);

                        let ptr2 = ptr.add(1) as *const Value;

                        let p = ptr2.add(*fieldID)  as *const Value;

                        self.stack.push(p.read());
                    }
                    Swap => {
                        let a = self.pop();
                        let b = self.pop();
                        self.stack.push(a);
                        self.stack.push(b);
                    }
                    StringLength => {
                        let a = self.pop();
                        let len = a.getString().len();
                        self.stack.push(len.into());
                    }
                    SetGlobal { namespaceID, globalID } => {
                        let v = self.pop();
                        let n = self.namespaces.get_mut(*namespaceID).unwrap();
                        let g = n.globals.getFastMut(*globalID).unwrap();

                        g.1 = v;
                    }
                    GetGlobal { namespaceID, globalID } => {
                        let n = self.namespaces.get(*namespaceID).unwrap();

                        self.push(n.globals.getFast(*globalID).unwrap().1)
                    }
                    o => panic!("unimplemented opcode {:?}", o)
                }
            }
        }
    }
}

impl VirtualMachine {
    pub fn link(&mut self) -> Result<(), Box<dyn Error>> {
        let functionReturns = self.buildFunctionReturn();

        let v = self as *mut VirtualMachine as *const VirtualMachine;

        for n in &mut self.namespaces {
            let nn = n as *mut Namespace;
            if n.state == Loaded {
                continue
            }

            for (index, g) in n.globals.actual.iter_mut().enumerate() {
                unsafe {
                    let mut ctx = ExpressionCtx{
                        exp: &g.0.default,
                        ops: &mut vec![],
                        functionReturns: &functionReturns,
                        vTable: &Default::default(),
                        typeHint: None,
                        currentNamespace: &mut *nn,
                        vm: &*v,
                    };
                    g.0.typ = ctx.toDataType()?.unwrap();
                }
            }

            for (index, f) in n.functionsMeta.iter_mut().enumerate() {
                unsafe {
                    if let FunctionTypeMeta::Runtime(_) = f.functionType {
                        let mut ops = vec![];
                        let res = unsafe { genFunctionDef(f, &mut ops, &functionReturns, &*v, &mut *nn, self.handleStatementExpression)? };
                        f.localsMeta = res.into_boxed_slice();

                        if DEBUG {
                            println!("link opcodes {} {:?}", f.name, ops);
                        }

                        // let nf = self.jitCompiler.compile(ops, &*v, &*nn);

                        *n.functions.get_mut(index).unwrap() = Some(LoadedFunction::Virtual(ops));
                    }
                }
            }

            n.state = Loaded;
        }
        Ok(())
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
            handleStatementExpression: |it, t| { if t != Void { it.push(Pop) } },
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

impl Drop for VirtualMachine {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}
