use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::path::Iter;
use std::rc::Rc;
use crate::DataType::Object;
use crate::FuncType::Native;
use crate::JmpType::Less;
use crate::OpCode::{Call, Jmp, Pop, PushFloat, PushLocal, Return, SetLocal, Sub};
use crate::Value::Bool;

#[derive(Clone, Debug)]
enum DataType {
    Int,
    Float,
    Bool,
    Array {
        inner: Box<DataType>
    },
    Object {
        name: String
    }
}

impl DataType {
    fn toString(&self) -> &str {
        match self {
            DataType::Int => "int",
            DataType::Float => "float",
            DataType::Bool => "bool",
            DataType::Array { inner } => "uwu",
            DataType::Object { name } => &name
        }
    }
}

impl DataType {
    fn toDefaultValue(&self) -> Value {
        match self {
            DataType::Int => Value::Num(0),
            DataType::Float => Value::Flo(0.),
            DataType::Bool => Value::Bool(false),
            DataType::Array { .. } => panic!(),
            DataType::Object { .. } => panic!()
        }
    }
}

#[derive(Clone, Debug)]
enum JmpType {
    One,
    Zero,
    Jmp,
    Gt,
    Less,
    True
}

#[derive(Clone, Debug)]
struct VariableMetadata {
    pub name: String,
    pub typ: DataType
}

#[derive(Clone, Debug)]
enum OpCode {
    FunBegin,
    FunName {
        name: String
    },
    FunReturn {
        typ: Option<DataType>
    },
    LocalVarTable {
        typ: Box<[VariableMetadata]>,
        argsCount: usize
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
        index: usize
    },
    SetLocal {
        index: usize,
        typ: DataType
    },
    Jmp {
        offset: isize,
        jmpType: JmpType,
        typ: DataType
    },
    Call {
        name: String,
        args: Box<[DataType]>
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
        name: String
    },
    ClassField {
        name: String,
        typ: DataType
    },
    ClassEnd,
    New {
        name: String
    },
    GetField {
        name: String,
        typ: DataType
    },
    SetField {
        name: String,
        typ: DataType
    },

    ArrayNew(DataType),
    ArrayStore(DataType),
    ArrayLoad(DataType),
    ArrayLength
}

struct MyObjectField {
    pub typ: DataType,
    pub value: Value
}

enum MyObject {
    ArrayObj {
        values: Vec<Value>,
        typ: DataType,
        size: usize
    },
    RuntimeObj {
        name: String,
        fields: Option<HashMap<String, MyObjectField>>
    }
}

#[derive(Clone)]
struct MyClassField {
    pub name: String,
    pub typ: DataType
}

#[derive(Clone)]
struct MyClass {
    pub name: String,
    pub fields: HashMap<String, MyClassField>
}

#[derive(Clone, Debug)]
enum Value {
    Num(isize),
    Flo(f32),
    Bool(bool),
    Reference()
}

impl Value {
    fn getNum(&self) -> isize {
        match self {
            Value::Num(v) => *v,
            Value::Flo(_) => panic!(),
            Value::Bool(_) => panic!(),
            Value::Reference() => panic!()
        }
    }

    fn getFlo(&self) -> f32 {
        match self {
            Value::Num(_) => panic!(),
            Value::Flo(v) => *v,
            Value::Bool(_) => panic!(),
            Value::Reference() => panic!()
        }
    }

    fn getBool(&self) -> bool {
        match self {
            Value::Num(_) => panic!(),
            Value::Flo(_) => panic!(),
            Value::Bool(v) => *v,
            Value::Reference() => panic!()
        }
    }
}

impl Value {
    fn or(&mut self, val: Value) {
        match self {
            Value::Num(_) => panic!(),
            Value::Flo(_) => panic!(),
            Value::Bool(mut v) => {
                v = v || match val {
                    Value::Num(_) => panic!(),
                    Value::Flo(_) => panic!(),
                    Value::Bool(v) => v,
                    Value::Reference() => panic!()
                };
            }
            Value::Reference() => panic!(),
        }
    }

    fn and(&mut self, val: Value) {
        match self {
            Value::Num(_) => panic!(),
            Value::Flo(_) => panic!(),
            Value::Bool(mut v) => {
                v = v && match val {
                    Value::Num(_) => panic!(),
                    Value::Flo(_) => panic!(),
                    Value::Bool(v) => v,
                    Value::Reference() => panic!()
                };
            }
            Value::Reference() => panic!(),
        }
    }

    fn not(&mut self) {
        match self {
            Value::Num(_) => panic!(),
            Value::Flo(_) => panic!(),
            Value::Bool(mut v) => {
                v = !v;
            }
            Value::Reference() => panic!(),
        }
    }
}

impl Value {
    fn gt(&self, val: Value, typ: &DataType) -> bool {
        match typ {
            DataType::Int => {
                self.getNum() > val.getNum()
            }
            DataType::Float => {
                self.getFlo() > val.getFlo()
            },
            DataType::Bool => {
                self.getBool() > val.getBool()
            },
            DataType::Array { .. } => panic!(),
            DataType::Object { .. } => panic!()
        }
    }

    fn less(&self, val: Value, typ: &DataType) -> bool {
        match typ {
            DataType::Int => {
                self.getNum() < val.getNum()
            }
            DataType::Float => {
                self.getFlo() < val.getFlo()
            },
            DataType::Bool => {
                self.getBool() < val.getBool()
            },
            DataType::Array { .. } => panic!(),
            DataType::Object { .. } => panic!()
        }
    }

    fn eq(&self, val: Value, typ: &DataType) -> bool {
        match typ {
            DataType::Int => {
                self.getNum() == val.getNum()
            }
            DataType::Float => {
                self.getFlo() == val.getFlo()
            },
            DataType::Bool => {
                self.getBool() == val.getBool()
            },
            DataType::Array { .. } => panic!(),
            DataType::Object { .. } => panic!()
        }
    }
}

impl Value {
    fn add(&mut self, value: &Value, typ: &DataType) {
        match typ {
            DataType::Int => {
                match self {
                    Value::Num(v) => {
                        *v += value.getNum();
                    }
                    Value::Flo(_) => panic!(),
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Float => {
                match self {
                    Value::Num(_) => panic!(),
                    Value::Flo(v) => {
                        *v += value.getFlo();
                    },
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Bool => {}
            DataType::Array { .. } => {}
            DataType::Object { .. } => {}
        }
    }

    fn sub(&mut self, value: &Value, typ: &DataType) {
        match typ {
            DataType::Int => {
                match self {
                    Value::Num(v) => {
                        *v -= value.getNum();
                    }
                    Value::Flo(_) => panic!(),
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Float => {
                match self {
                    Value::Num(_) => panic!(),
                    Value::Flo(v) => {
                        *v -= value.getFlo();
                    },
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Bool => {}
            DataType::Array { .. } => {}
            DataType::Object { .. } => {}
        }
    }

    fn mul(&mut self, value: &Value, typ: &DataType) {
        match typ {
            DataType::Int => {
                match self {
                    Value::Num(mut v) => {
                        v *= match value {
                            Value::Num(v) => v,
                            Value::Flo(_) => panic!(),
                            Value::Bool(_) => panic!(),
                            Value::Reference() => panic!(),
                        };
                    }
                    Value::Flo(_) => panic!(),
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Float => {
                match self {
                    Value::Num(_) => panic!(),
                    Value::Flo(mut v) => {
                        v *= match value {
                            Value::Num(_) => panic!(),
                            Value::Flo(v) => v,
                            Value::Bool(_) => panic!(),
                            Value::Reference() => panic!(),
                        };
                    },
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Bool => {}
            DataType::Array { .. } => {}
            DataType::Object { .. } => {}
        }
    }

    fn div(&mut self, value: &Value, typ: &DataType) {
        match typ {
            DataType::Int => {
                match self {
                    Value::Num(mut v) => {
                        v /= match value {
                            Value::Num(v) => v,
                            Value::Flo(_) => panic!(),
                            Value::Bool(_) => panic!(),
                            Value::Reference() => panic!(),
                        };
                    }
                    Value::Flo(_) => panic!(),
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Float => {
                match self {
                    Value::Num(_) => panic!(),
                    Value::Flo(mut v) => {
                        v /= match value {
                            Value::Num(_) => panic!(),
                            Value::Flo(v) => v,
                            Value::Bool(_) => panic!(),
                            Value::Reference() => panic!(),
                        };
                    },
                    Value::Bool(_) => panic!(),
                    Value::Reference() => panic!()
                }
            }
            DataType::Bool => {}
            DataType::Array { .. } => {}
            DataType::Object { .. } => {}
        }
    }

    fn f2i(&mut self) -> Value {
        return match self {
            Value::Num(_) => panic!(),
            Value::Flo(v) => Value::Num(*v as isize),
            Value::Bool(_) => panic!(),
            Value::Reference() => panic!()
        };
    }

    fn i2f(&mut self) -> Value {
        return match self {
            Value::Num(v) => Value::Flo(*v as f32),
            Value::Flo(_) => panic!(),
            Value::Bool(_) => panic!(),
            Value::Reference() => panic!()
        };
    }
}

impl Value {
    fn isType(&self, typ: &DataType) -> bool {
        match self {
            Value::Num(_) => {
                match typ {
                    DataType::Int => true,
                    _ => false
                }
            }
            Value::Flo(_) => {
                match typ {
                    DataType::Float => true,
                    _ => false
                }
            }
            Value::Bool(_) => {
                match typ {
                    DataType::Bool => true,
                    _ => false
                }
            }
            Value::Reference() => panic!()
        }
    }
}

#[derive(Debug)]
struct StackFrame {
    // previous: Box<Option<Cell<StackFrame>>>,
    localVariables: Box<[Value]>,
    name: Option<String>
}

#[derive(Clone)]
struct Func {
    name: String,
    returnType: Option<DataType>,
    varTable: Vec<VariableMetadata>,
    argAmount: usize,
    typ: FuncType,
}

#[derive(Clone)]
enum FuncType {
    Runtime {
        rangeStart: usize,
        rangeStop: usize,
    },
    Native {
        callback: fn(&VirtualMachine, StackFrame) -> (),
    }
}

#[derive(Clone)]
struct VirtualMachine {
    pub functions: HashMap<String, Func>,
    pub stack: Vec<Value>,
    pub classes: HashMap<String, MyClass>,
    pub opCodes: Vec<OpCode>
}

impl VirtualMachine {
    fn new() -> Self {
        Self {
            functions: Default::default(),
            stack: vec![],
            classes: Default::default(),
            opCodes: vec![],
        }
    }

    fn makeNative(&mut self, name: String, args: Vec<VariableMetadata>, fun: fn(&VirtualMachine, StackFrame) -> (), ret: Option<DataType>) {
        let genName = genFunNameMeta(&name, &args);
        let l = args.len();
        self.functions.insert(genName, Func {
            name,
            returnType: ret,
            varTable: args,
            argAmount: l,
            typ: Native {
                callback: fun,
            },
        });
    }
}



struct SeekableOpcodes {
    pub index: isize,
    pub opCodes: Vec<OpCode>,
    pub start: Option<usize>,
    pub end: Option<usize>
}

impl SeekableOpcodes {
    fn seek(&mut self, offset: isize) {
        // FIXME boundary check
        self.index += offset;
    }

    fn next(&mut self) -> Option<&OpCode> {
        let n = self.opCodes.get(self.index as usize);
        self.index += 1;

        n
    }
}

fn argsToString(args: &Box<[DataType]>) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.toString());
        if i != args.len()-1 {
            buf.push_str(", ")
        }
    }
    buf
}

fn argsToStringMeta(args: &Vec<VariableMetadata>) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.typ.toString());
        if i != args.len()-1 {
            buf.push_str(", ")
        }
    }
    buf
}

fn genFunName(name: &String, args: &Box<[DataType]>) -> String {
    format!("{}({})", name, argsToString(args))
}

fn genFunNameMeta(name: &String, args: &Vec<VariableMetadata>) -> String {
    format!("{}({})", name, argsToStringMeta(args))
}

fn run(mut opCodes: Cell<SeekableOpcodes>, mut vm: VirtualMachine, mut stackFrame: Cell<StackFrame>) -> VirtualMachine {
    loop {
        let op = match opCodes.get_mut().next() {
            None => {
                return vm;
            }
            Some(v) => v
        };
        // println!("processing {:?}", op);
        match op {
            OpCode::FunBegin => panic!(),
            OpCode::FunName { .. } => panic!(),
            OpCode::FunReturn { .. } => panic!(),
            OpCode::LocalVarTable { .. } => panic!(),
            OpCode::FunEnd => panic!(),
            OpCode::F2I => {
                let mut x = vm.stack.pop().unwrap();
                vm.stack.push(x.f2i())
            }
            OpCode::I2F => {
                let mut x = vm.stack.pop().unwrap();
                vm.stack.push(x.i2f())
            }
            OpCode::PushInt(v) => {
                vm.stack.push(Value::Num(*v))
            }
            OpCode::PushFloat(v) => {
                vm.stack.push(Value::Flo(*v))
            }
            OpCode::PushBool(v) => {
                vm.stack.push(Value::Bool(*v))
            }
            OpCode::Pop => {
                vm.stack.pop();
            }
            OpCode::Dup => {
                let x = vm.stack.pop().unwrap();
                vm.stack.push(x.clone());
                vm.stack.push(x);
            }
            OpCode::PushLocal { index } => {
                vm.stack.push(stackFrame.get_mut().localVariables.get(*index).unwrap().clone())
            }
            OpCode::SetLocal { index, typ } => {
                let x = vm.stack.pop().unwrap();
                *(stackFrame.get_mut().localVariables.get_mut(*index).unwrap()) = x;
                // stackFrame.get_mut().localVariables.insert(*index, x);
            }
            OpCode::Jmp { offset, jmpType, typ } => {
                match jmpType {
                    JmpType::One => {
                        let mut a = vm.stack.pop().unwrap();
                        let b = vm.stack.pop().unwrap();
                    }
                    JmpType::Zero => {}
                    JmpType::Jmp => {
                        let x = *offset;
                        opCodes.get_mut().seek(x)
                    }
                    JmpType::Gt => {
                        let mut a = vm.stack.pop().unwrap();
                        let b = vm.stack.pop().unwrap();
                        if a.gt(b, typ) {
                            let x = *offset;
                            opCodes.get_mut().seek(x)
                        }
                    }
                    JmpType::Less => {
                        let mut a = vm.stack.pop().unwrap();
                        let b = vm.stack.pop().unwrap();
                        if a.less(b, typ) {
                            unsafe {
                                // opCodes.as_ptr().as_ref().unwrap().seek(offset);
                                let x = *offset;
                                opCodes.get_mut().seek(x)
                            }
                        }
                    }
                    JmpType::True => {
                        let mut a = vm.stack.pop().unwrap();
                        if a.getBool() {
                            let x = *offset;
                            opCodes.get_mut().seek(x)
                        }
                    }
                }
            }
            OpCode::Call { name, args } => {
                let genName = genFunName(&name, args);
                let mut f = vm.functions.get(&genName).unwrap();

                let mut localVars = vec![];

                for i in 0..(f.argAmount) {
                    let arg = vm.stack.pop().unwrap();
                    let x = f.varTable.get(i).unwrap();
                    if !arg.isType(&x.typ) {
                        panic!()
                    }
                    localVars.push(arg)
                }

                for i in 0..(f.varTable.len()-f.argAmount) {
                    localVars.push(f.varTable[i].typ.toDefaultValue())
                }

                let stack = StackFrame {
                    localVariables: localVars.into_boxed_slice(),
                    name: Some(genName),
                };

                match f.typ {
                    FuncType::Runtime { rangeStart, rangeStop } => {
                        panic!()
                    }
                    Native { callback } => {
                        callback(&vm, stack)
                    }
                }
            }
            OpCode::Return => {
                return vm
            }
            OpCode::Add(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.add(&b, v);
                vm.stack.push(a)
            }
            OpCode::Sub(v) => {
                let a = vm.stack.pop().unwrap();
                let mut b = vm.stack.pop().unwrap();

                b.sub(&a, v);
                vm.stack.push(b)
            }
            OpCode::Div(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.div(&b, v);
                vm.stack.push(a)
            }
            OpCode::Mul(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.mul(&b, v);
                vm.stack.push(a)
            }
            OpCode::Equals(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.eq(b, v);
                vm.stack.push(a);
            }
            OpCode::Greater(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                let res = a.gt(b, v);
                vm.stack.push(Bool(res));
            }
            OpCode::Less(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                let res = a.less(b, v);
                vm.stack.push(Bool(res));
            }
            OpCode::Or => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.or(b);
                vm.stack.push(a);
            }
            OpCode::And => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.and(b);
                vm.stack.push(a);
            }
            OpCode::Not => {
                let mut a = vm.stack.pop().unwrap();
                a.not();
                vm.stack.push(a);
            }
            OpCode::ClassBegin => panic!(),
            OpCode::ClassName { .. } => panic!(),
            OpCode::ClassField { .. } => panic!(),
            OpCode::ClassEnd => panic!(),
            OpCode::New { .. } => panic!(),
            OpCode::GetField { .. } => panic!(),
            OpCode::SetField { .. } => panic!(),
            OpCode::ArrayNew(_) => panic!(),
            OpCode::ArrayStore(_) => panic!(),
            OpCode::ArrayLoad(_) => panic!(),
            OpCode::ArrayLength => panic!(),
        }
    }
    return vm;
}

fn main() {
    let mut vm = VirtualMachine::new();

    vm.makeNative(String::from("print"), vec![VariableMetadata { name: "value".to_string(), typ: DataType::Int }], |a, mut b|{
        println!("{:?}", b.localVariables[0].getNum())
    }, None);

    vm.makeNative(String::from("print"), vec![VariableMetadata { name: "value".to_string(), typ: DataType::Float }], |a, mut b|{
        println!("{:?}", b.localVariables[0].getFlo())
    }, None);

    let ops = vec![
        PushFloat(10000000.),
        SetLocal { index: 0, typ: DataType::Float },
        PushLocal { index: 0 },
        PushFloat(0.),
        OpCode::Less(DataType::Float),
        Jmp {
            offset: 1,
            jmpType: JmpType::True,
            typ: DataType::Int,
        },
        Return,
        PushLocal { index: 0 },
        Call { name: "print".to_string(), args: Box::new([DataType::Float]) },
        PushLocal { index: 0},
        PushFloat(1.),
        Sub(DataType::Float),
        SetLocal { index: 0, typ: DataType::Float },
        Jmp {
            offset: -12,
            jmpType: JmpType::Jmp,
            typ: DataType::Int,
        }
    ];

    let x = vec![
        vec![
            OpCode::PushInt(69),
            OpCode::PushInt(1),
            OpCode::Add(DataType::Int),
            OpCode::Call {
                name: "print".to_string(),
                args: Box::new([DataType::Int]),
            }
        ]
    ];

    let seek = SeekableOpcodes {
        index: 0,
        opCodes: ops,
        start: None,
        end: None,
    };

    let stack = StackFrame {
        // previous: Box::new(None),
        localVariables: Box::new([Value::Flo(0.)]),
        name: None,
    };

    let mut v = run(Cell::new(seek), vm, Cell::new(stack));
    println!("{:?}", v.stack.pop());
}