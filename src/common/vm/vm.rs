use std::{intrinsics, ptr};
use std::alloc::{alloc, dealloc, Layout};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::hint::unreachable_unchecked;
use std::mem::size_of;

use crate::asm::jitCompiler::JITCompiler;
use crate::ast::FunctionDef;
use crate::bytecodeGen::{ExpressionCtx, genFunctionDef, StatementCtx};
use crate::errors::Errorable;
use crate::ffi::NativeWrapper;
use crate::utils::FastVec;
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::{Int, Void};
use crate::vm::heap::{Allocation, Hay, HayCollector, Heap};
use crate::vm::myStr::MyStr;
use crate::vm::namespace::{FunctionTypeMeta, GlobalMeta, LoadedFunction, Namespace};
use crate::vm::namespace::LoadedFunction::Native;
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
    ArrayStore,
    ArrayLoad,
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
    ConstStr(usize),
    MulInt,
    AddInt,
    SubInt,
    LessInt,
    GetLocalZero,
    SetLocalZero,
}

fn isPure(ops: &[OpCode], vm: &VirtualMachine, namespace: &Namespace) -> bool {
    for op in ops {
        match op {
            SCall { id } => {
                let f = namespace.getFunction(*id);
                if !f.0.isPure {
                    return false
                }
            }
            LCall { namespace, id } => {
                let f = vm.getNamespace(*namespace).getFunction(*id);
                if !f.0.isPure {
                    return false
                }
            }
            DynamicCall => {
                return false
            }
            SetGlobal { .. } => {
                return false
            }
            GetGlobal { .. } => {
                return false
            }
            _ => {}
        }
    }
    true
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
#[repr(C)]
pub struct VirtualMachine {
    pub nativeWrapper: NativeWrapper,

    pub stack: Vec<Value>,
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
    #[inline]
    pub fn allocateString(&mut self, st: &str) ->  Hay<ViplObject<Str>> {
        let a1 = Str::new(String::from(st));
        let aw = ViplObject::<Str>::str(a1);


        self.heap.allocate(aw)
    }

    #[inline]
    pub fn allocateArray(&mut self, vec: Vec<Value>, t: DataType) -> Hay<ViplObject<Array>> {
        let arr = Array{
            internal: vec,
            typ: t,
        };
        self.heap.allocate(ViplObject::<Array>::arr(arr))
    }

    #[inline]
    pub fn findNamespace(&self, name: &str) -> Errorable<(&Namespace, usize)> {
        let namespaceId = self.namespaceLookup.get(name)
            .ok_or(format!("failed to find namespace {}", name))?;
        let namespace = self.namespaces.get(*namespaceId)
            .ok_or(format!("failed to find namespace with id {}", *namespaceId))?;

        Ok((namespace, *namespaceId))
    }

    #[inline]
    pub fn findNamespaceParts(&self, parts: &[String]) -> Errorable<(&Namespace, usize)> {
        let namespaceName = parts.join("::");

        self.findNamespace(&namespaceName)
    }

    #[inline]
    pub fn rawPtr(&self) -> *mut VirtualMachine {
        self as *const VirtualMachine as *mut VirtualMachine
    }

    #[inline]
    pub fn buildFunctionReturn(&self) -> HashMap<MyStr, Option<DataType>> {
        let mut x = HashMap::new();

        for n in &self.namespaces {
            for (f, _) in &n.functions.actual {
                let mut buf = String::new();
                buf += &n.name;
                buf += "::";
                buf += &f.genName();
                x.insert(buf.into(), f.returnType.clone());
            }
        }

        x
    }

    #[inline]
    pub fn registerNamespace(&mut self, mut namespace: Namespace) -> usize {
        let index = self.namespaces.len();
        namespace.id = index;

        self.namespaceLookup.insert(namespace.name.clone(), index);
        self.namespaces.push(namespace);
        index
    }

    #[inline]
    pub fn getNamespace(&self, id: usize) -> &Namespace {
        self.namespaces.get(id).unwrap()
    }

    #[inline]
    pub fn getNamespaceMut(&mut self, id: usize) -> &mut Namespace {
        self.namespaces.get_mut(id).unwrap()
    }

    #[inline(always)]
    pub fn pop(&self) -> Value {
        if DEBUG || TRACE {
            unsafe { (&mut *(self as *const VirtualMachine as *mut VirtualMachine)).stack.pop().unwrap() }
        }
        else {
            let mut res: &mut FastVec<Value> = unsafe { &mut *(&self.stack as *const Vec<Value> as *mut FastVec<Value>) };
            let mut buf: Value = Value::null();

            unsafe { res.size = res.size.unchecked_sub(1) };

            unsafe { ptr::copy(intrinsics::offset(res.ptr, res.size as isize) as *mut Value, &mut buf as *mut Value, 1); }

            buf
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
        unsafe { self.frames.get_unchecked(self.frames.len().unchecked_sub(1)) }
    }

    #[inline(always)]
    pub fn getMutFrame(&self) -> &mut StackFrame {
        unsafe { (&mut *(self as *const VirtualMachine as *mut VirtualMachine)).frames.get_unchecked_mut(self.frames.len().unchecked_sub(1)) }
    }

    #[inline(always)]
    pub fn nextOpcode2<'a>(&'a self, ops: &'a [OpCode]) -> (Option<&OpCode>, usize) {
        let x = self.getMutFrame();
        let a = x.programCounter;
        unsafe { x.programCounter = x.programCounter.unchecked_add(1); }
        (ops.get(a), a)
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
    pub fn call(&mut self, namespaceId: usize, functionId: usize) {
        let namespace = self.namespaces.get(namespaceId).unwrap();
        let (fMeta, f) = namespace.getFunction(functionId);

        println!("gona call");

        let mut locals = vec![Value::from(0);fMeta.argsCount];

        println!("gona call");

        for i in (0..fMeta.argsCount).rev() {
            locals[i] = self.pop();
        }

        println!("gona call");

        let fs = StackFrame{
            localVariables: locals.into_boxed_slice(),
            programCounter: 0,
            namespaceId: namespaceId,
            functionId,
        };

        let x = f.as_ref().unwrap();

        unsafe { x.call(&mut *(self as *const VirtualMachine as *mut VirtualMachine), fs) }
    }

    #[inline]
    pub fn execute(&mut self, opCodes: &[OpCode]) {
        let r = unsafe { &mut *(self as *const VirtualMachine as *mut VirtualMachine) };
        let r1 = unsafe { &mut *(self as *const VirtualMachine as *mut VirtualMachine) };

        loop {
            let (op1, index) = (&mut *r1).nextOpcode2(opCodes);

            let op = match op1 {
                None => {
                    return;
                }
                Some(v) => v
            };

            if TRACE {
                println!("evaluating {:?}", op);
            }

            match op {
                F2I => {
                    self.getMutTop().f2i()
                }
                I2F => {
                    self.getMutTop().f2i()
                }
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
                },
                GetLocal { index } => {
                    self.push(*self.getLocal(*index))
                }
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
                Return => {
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
                    self.push(Value { Num: self.pop().getReference::<Array>().data.internal.len() as isize });
                },
                // FIXME inc is slower than executing: pushLocal, PushOne, Add, SetLocal
                Inc { typ, index } => unsafe {
                    self.getMutLocal(*index).inc(typ)
                },
                Dec { typ, index } => unsafe {
                    self.getMutLocal(*index).dec(typ)
                },
                PushChar(c) => self.push((*c).into()),
                StrNew(s) => {
                    let a = Value::makeString(s.to_string(), self);
                    self.push(a)
                },
                GetChar => {
                    let index = self.pop().getNum();

                    let opIndex = self.stack.len() - 1;

                    let r = self.stack.get_mut(opIndex).unwrap();
                    let c = *r.getString().as_bytes().get(index as usize).unwrap() as char;
                    *r = c.into();
                },
                SCall { id } => {
                    let frame = self.getFrame();

                    (&mut *r).call(frame.namespaceId, *id)

                }
                DynamicCall => {
                    let (namespaceRaw, idRaw) = self.pop().asFunction();
                    let namespace = namespaceRaw as usize;
                    let id = idRaw as usize;

                    r.call(namespace, id)
                }
                LCall { namespace, id} => {
                    r.call(*namespace, *id)
                }
                PushFunction(namespaceID, functionID) => {
                    self.push(Value::makeFunction(*namespaceID, *functionID));
                }
                New { namespaceID, structID } => unsafe {
                    let n = self.namespaces.get(*namespaceID).unwrap();
                    let s = n.structs.getFast(*structID).unwrap();

                    let mut l = Layout::new::<()>();

                    l = l.extend(Layout::array::<ViplObjectMeta<()>>(1).unwrap()).unwrap().0;
                    l = l.extend(Layout::array::<Value>(s.fields.len()).unwrap()).unwrap().0;

                    let alloc = alloc(l)  as *mut UntypedObject;

                    (*alloc).objectType = ObjectType::Simple(s.fields.len());
                    (*alloc).structId = *structID;
                    (*alloc).namespaceId = *namespaceID;

                    self.push(Value::from(alloc as usize))
                }
                SetField { namespaceID, structID, fieldID } => unsafe {
                    let value = self.pop();

                    let ptr = ((self.pop().asRefMeta() as *const UntypedObject).add(1)) as *const Value;
                    let p = ptr.add(*fieldID) as *mut Value;

                    *p = value;
                }
                GetField { namespaceID, structID, fieldID } => unsafe {
                    let ptr = self.pop().asRefMeta() as *const UntypedObject;

                    let ptr2 = ptr.add(1) as *const Value;

                    let p = ptr2.add(*fieldID)  as *const Value;

                    self.push(p.read());
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
                GetLocalZero => {
                    self.push(*self.getLocal(0))
                }
                o => if !DEBUG && !TRACE {
                    unsafe { unreachable_unchecked() }
                }
                else {
                    panic!("unimplemented opcode {:?}", o)
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
                        ops: &mut vec![],
                        exp: &g.0.default,
                        functionReturns: &functionReturns,
                        vTable: &Default::default(),
                        typeHint: None,
                        currentNamespace: &mut *nn,
                        vm: &*v,
                    };
                    g.0.typ = ctx.toDataType()?.unwrap();
                }
            }

            for (index, (f, a)) in n.functions.actual.iter_mut().enumerate() {
                unsafe {
                    if let FunctionTypeMeta::Runtime(_) = f.functionType {
                        let mut ops = vec![];
                        let res = unsafe { genFunctionDef(f, &mut ops, &functionReturns, &*v, &mut *nn, self.handleStatementExpression)? };
                        f.localsMeta = res.into_boxed_slice();

                        let opt = optimizeOps(ops);

                        if DEBUG {
                            println!("link opcodes {} {:?}", f.name, opt);
                        }

                        let nf = self.jitCompiler.compile(opt, &*v, &*nn);
                        *a = Some(Native(nf))

                        // *a = Some(LoadedFunction::Virtual(opt));
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

fn optimizeOps(i: Vec<OpCode>) -> Vec<OpCode> {
    let mut res = vec![];

    for op in i.into_iter() {
        if op == OpCode::Add(Int) {
            res.push(OpCode::AddInt)
        }
        else if op == OpCode::Sub(Int) {
            res.push(OpCode::SubInt)
        }
        else if op == OpCode::Mul(Int) {
            res.push(OpCode::MulInt)
        }
        else if op == OpCode::Less(Int) {
            res.push(OpCode::LessInt)
        }
        else if (op == OpCode::GetLocal{index: 0}) {
            res.push(OpCode::GetLocalZero)
        }
        else if (op == OpCode::SetLocal{index: 0}) {
            res.push(OpCode::SetLocalZero)
        }
        else {
            res.push(op)
        }
    }

    res
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
