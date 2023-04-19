use std::{intrinsics, ptr};
use std::alloc::{alloc, Layout};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use libloading::os::unix::Library;
use crate::asm::jitCompiler::JITCompiler;
use crate::bytecodeGen::genFunctionDef;
use crate::ffi::NativeWrapper;
use crate::utils::FastVec;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::DataType;
use crate::vm::heap::{Allocation, HayCollector, Heap};
use crate::vm::myStr::MyStr;
use crate::vm::namespace::{FunctionTypeMeta, LoadedFunction, Namespace};
use crate::vm::namespace::NamespaceState::Loaded;
use crate::vm::namespaceLoader::NamespaceLoader;
use crate::vm::nativeStack::StackManager;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::FuncType::{Builtin, Extern, Runtime};
use crate::vm::vm::OpCode::*;

const DEBUG: bool = false;

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
pub struct VirtualMachine<'a> {
    pub nativeWrapper: NativeWrapper,

    pub stack: Vec<Value>,
    pub nativeLibraries: Vec<Library>,
    pub heap: Heap,

    pub stackManager: StackManager<2048>,

    pub frames: Vec<StackFrame<'a>>,

    pub namespaceLookup: HashMap<String, usize>,
    pub namespaces: Vec<Namespace>,

    pub namespaceLoader: NamespaceLoader,
    pub jitCompiler: JITCompiler
}

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

impl Drop for VirtualMachine<'_> {
    fn drop(&mut self) {
        println!("vm is being destroyed")
    }
}
