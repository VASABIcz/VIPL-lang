use std::collections::HashMap;
use std::intrinsics::transmute;
use crate::OpCode::{And, ArrayLength, ClassBegin, ClassEnd, Dup, F2I, FunBegin, FunEnd, I2F, Not, Or, Pop, Return};
use crate::vm::DataType::*;
use crate::vm::FuncType::*;
use crate::vm::OpCode::*;
use crate::vm::Value::*;

#[derive(Clone, Debug)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Array { inner: Box<DataType> },
    Object { name: String },
}

impl DataType {
    pub(crate) fn fromString(s: &str) -> Self {
        if s.ends_with("[]") {
            return DataType::Array {
                inner: Box::new(DataType::fromString(s.strip_suffix("[]").unwrap())),
            };
        }

        match s {
            "int" => DataType::Int,
            "float" => DataType::Float,
            "bool" => DataType::Bool,
            _ => DataType::Object {
                name: String::from(s),
            },
        }
    }
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
    pub(crate) fn toBytes(&self, bytes: &mut Vec<u8>) {
        let opId: [u8; 32] = unsafe { transmute((*self).clone()) };
        bytes.push(opId[0]);
        match self {
            Int => {}
            Float => {}
            Bool => {}
            Array { inner } => inner.toBytes(bytes),
            Object { name } => {
                let bs = name.escape_default().to_string();
                bytes.extend(bs.len().to_ne_bytes());
                bytes.extend(bs.as_bytes())
            }
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
            Object { name } => &name,
        }
    }
}

impl DataType {
    pub(crate) fn toDefaultValue(&self) -> Value {
        match self {
            Int => Num(0),
            Float => Flo(0.),
            Bool => Bol(false),
            Array { .. } => panic!(),
            Object { .. } => panic!(),
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
}

impl JmpType {
    pub(crate) fn toBytes(&self, bytes: &mut Vec<u8>) {
        let opId: [u8; 1] = unsafe { std::mem::transmute((*self).clone()) };
        bytes.push(opId[0]);
    }
}

#[derive(Clone, Debug)]
pub struct VariableMetadata {
    pub name: String,
    pub typ: DataType,
}

impl VariableMetadata {
    pub fn f(name: String) -> Self {
        Self {
            name,
            typ: Float,
        }
    }

    pub fn i(name: String) -> Self {
        Self {
            name,
            typ: Int,
        }
    }
}

impl VariableMetadata {
    pub(crate) fn toBytes(&self, bytes: &mut Vec<u8>) {
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
        name: String,
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
        encoded: String,
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
        name: String,
    },
    ClassField {
        name: String,
        typ: DataType,
    },
    ClassEnd,
    New {
        name: String,
    },
    GetField {
        name: String,
        typ: DataType,
    },
    SetField {
        name: String,
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
    Reference(),
}

impl Value {
    #[inline(always)]
    pub fn getNum(&self) -> isize {
        match self {
            Num(v) => *v,
            Flo(_) => panic!(),
            Bol(_) => panic!(),
            Reference() => panic!(),
        }
    }

    #[inline(always)]
    pub fn getFlo(&self) -> f32 {
        match self {
            Num(_) => panic!(),
            Flo(v) => *v,
            Bol(_) => panic!(),
            Reference() => panic!(),
        }
    }

    #[inline(always)]
    pub fn getRefFlo(&mut self) -> &mut f32 {
        match self {
            Num(_) => panic!(),
            Flo(v) => v,
            Bol(_) => panic!(),
            Reference() => panic!(),
        }
    }

    #[inline(always)]
    pub fn getRefNum(&mut self) -> &mut isize {
        match self {
            Num(v) => v,
            Flo(_) => panic!(),
            Bol(_) => panic!(),
            Reference() => panic!(),
        }
    }

    #[inline(always)]
    pub fn getRefBol(&mut self) -> &mut bool {
        match self {
            Num(_) => panic!(),
            Flo(_) => panic!(),
            Bol(v) => v,
            Reference() => panic!(),
        }
    }

    #[inline(always)]
    pub fn getBool(&self) -> bool {
        match self {
            Num(_) => panic!(),
            Flo(_) => panic!(),
            Bol(v) => *v,
            Reference() => panic!(),
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
            Object { .. } => {}
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
            Reference() => panic!(),
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
    name: String,
    returnType: Option<DataType>,
    varTable: Box<[VariableMetadata]>,
    argAmount: usize,
    typ: FuncType,
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
    pub functions: HashMap<String, Func>,
    pub stack: Vec<Value>,
    pub classes: HashMap<String, MyClass>,
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
        let genName = genFunNameMeta(&name, &args);
        let l = args.len();
        self.functions.insert(
            genName,
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
        let genName = genFunNameMeta(&name, &args);

        let fun = Func {
            name,
            returnType: ret,
            varTable: args,
            argAmount: argsCount,
            typ: Runtime { rangeStart: begin, rangeStop: end }
        };

        self.functions.insert(genName, fun);
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
pub fn genFunNameMeta(name: &String, args: &[VariableMetadata]) -> String {
    format!("{}({})", name, argsToStringMeta(args))
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
        // println!("processing {:?}", op);
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

                vm.makeRuntime(name.clone(), vars.clone(), startIndex, *argCount, ret.clone(), 0);
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
                }
                JmpType::Zero => {}
                JmpType::Jmp => {
                    let x = *offset;
                    opCodes.seek(x)
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
            },
            Call { encoded } => {
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

                        let mut localVars = Vec::with_capacity(f.varTable.len());

                        for i in &*f.varTable {
                            localVars.push(i.typ.toDefaultValue())
                        }

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

                let e = &mut cached.0.clone().into_boxed_slice();

                for i in 0..(*cached.2) {
                    let arg = vm.stack.pop().unwrap();
                    e[i] = arg;
                }

                // FIXME
                // let enc = &String::from(encoded);

                let mut stack = StackFrame {
                    previous: Some(stackFrame),
                    localVariables: &mut e.clone(),
                    name: None// FIXME Some(enc),
                };

                match cached.1 {
                    Runtime {
                        rangeStart: s,
                        rangeStop: e,
                    } => {
                        let old = index+1;
                        opCodes.index = *s as isize;
                        run(opCodes, vm, &mut stack);
                        opCodes.index = old as isize;
                    }
                    Native { callback } => callback(vm, &mut stack),
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
                let l = vm.stack.len()-1;
                vm.stack.get_unchecked_mut(l).mul(&a, v);
                // println!("{:?}", vm.stack.get(l));
            }
            Equals(v) => {
                let a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.eq(&b, v);
                vm.stack.push(a);
            }
            Greater(v) => {
                let a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                let res = a.gt(&b, v);
                vm.stack.push(Bol(res));
            }
            Less(v) => unsafe {
                let a = vm.stack.pop().unwrap();
                let l = vm.stack.len()-1;
                vm.stack.get_unchecked_mut(l).refLess(&a, v);
            }
            Or => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.or(&b);
                vm.stack.push(a);
            }
            And => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.and(&b);
                vm.stack.push(a);
            }
            Not => {
                let mut a = vm.stack.pop().unwrap();
                a.not();
                vm.stack.push(a);
            }
            ClassBegin => panic!(),
            ClassName { .. } => panic!(),
            ClassField { .. } => panic!(),
            ClassEnd => panic!(),
            New { .. } => panic!(),
            GetField { .. } => panic!(),
            SetField { .. } => panic!(),
            ArrayNew(_) => panic!(),
            ArrayStore(_) => panic!(),
            ArrayLoad(_) => panic!(),
            ArrayLength => panic!(),
            Inc { typ, index } => stackFrame.localVariables.get_mut(*index).unwrap().inc(typ),
            Dec { typ, index } => stackFrame.localVariables.get_mut(*index).unwrap().dec(typ),
        }
    }
}

pub fn bootStrapVM() -> VirtualMachine {
    let mut vm = VirtualMachine::new();

    vm.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: "value".to_string(),
            typ: Int,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getNum()),
        None,
    );

    vm.makeNative(
        String::from("print"),
        Box::new([VariableMetadata {
            name: "value".to_string(),
            typ: Float,
        }]),
        |_a, b| println!("{}", b.localVariables[0].getFlo()),
        None,
    );

    vm.makeNative(
        String::from("assert"),
        Box::new([VariableMetadata::i(String::from("left")), VariableMetadata::i(String::from("right"))]),
        |_a, b| {
            let left = b.localVariables[1].getNum();
            let right = b.localVariables[0].getNum();
            if left != right {
                panic!("assert {} != {}", left, right)
            }
        },
        None,
    );

    vm.makeNative(
        String::from("exec"),
        Box::default(),
        |a, b| {
            /*
            let stack = match b.previous {
                None => b,
                Some(v) => v
            };

             */
            let genOps = [
                PushInt(1),
                Pop,
                PushInt(69),
                Call {
                    encoded: "print(int)".to_string(),
                },
            ];

            let mut seek = SeekableOpcodes {
                index: 0,
                opCodes: &genOps,
                start: None,
                end: None,
            };

            run(&mut seek, a, b);
        },
        None,
    );
    vm
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
            opCodes: &bytecode.into_boxed_slice(),
            start: None,
            end: None,
        },
        &mut vm,
        &mut StackFrame::new(&mut vals.into_boxed_slice()),
    );

    (vm)
}
