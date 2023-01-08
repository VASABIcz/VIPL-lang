extern crate core;

use core::panicking::panic;
use std::collections::HashMap;
use std::path::Iter;

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
    fn toString(&self) -> String {
        match self {
            DataType::Int => "int".into_string(),
            DataType::Float => "float".into_string(),
            DataType::Bool => "bool".into_string(),
            DataType::Array { inner } => format!("{}[]", inner.toString()),
            DataType::Object { name } => name
        }
    }
}

enum JmpType {
    One,
    Zero,
    Jmp,
    Gt,
    Less,
    True
}

struct VariableMetadata {
    pub name: String,
    pub typ: DataType
}

enum OpCode {
    FunBegin,
    FunName {
        name: String
    },
    FunReturn {
        typ: Option<DataType>
    },
    LocalVarTable {
        typ: Vec<VariableMetadata>,
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
        args: Vec<DataType>
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

struct MyClassField {
    pub name: String,
    pub typ: DataType
}

struct MyClass {
    pub name: String,
    pub fields: HashMap<String, MyClassField>
}

#[derive(Clone)]
enum Value {
    Num(isize),
    Flo(f32),
    Bool(bool),
    Reference()
}

impl Value {
    fn add(&mut self, value: &Value, typ: DataType) {
        match typ {
            DataType::Int => {
                match self {
                    Value::Num(mut v) => {
                        v += match value {
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
                        v += match value {
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

    fn sub(&mut self, value: &Value, typ: DataType) {
        match typ {
            DataType::Int => {
                match self {
                    Value::Num(mut v) => {
                        v -= match value {
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
                        v -= match value {
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

    fn mul(&mut self, value: &Value, typ: DataType) {
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

    fn div(&mut self, value: &Value, typ: DataType) {
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
            Value::Flo(v) => Value::Num(v as isize),
            Value::Bool(_) => panic!(),
            Value::Reference() => panic!()
        };
    }

    fn i2f(&mut self) -> Value {
        return match self {
            Value::Num(v) => Value::Flo(v as f32),
            Value::Flo(_) => panic!(),
            Value::Bool(_) => panic!(),
            Value::Reference() => panic!()
        };
    }
}

struct StackFrame {
    previous: Box<StackFrame>,
    localVariables: Vec<Value>,
    name: Option<String>
}

enum Func {
    Runtime {
        name: String,
        rangeStart: usize,
        rangeStop: usize,
        argAmount: usize,
        returnType: Option<DataType>,
        varTable: Vec<VariableMetadata>
    },
    Native {
        name: String,
        rangeStart: usize,
        callback: dyn Fn(VirtualMachine, StackFrame) -> (),
        returnType: Option<DataType>,
        varTable: Vec<VariableMetadata>
    }
}

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

    fn makeNative(&mut self, name: String, args: Vec<5>) {

    }
}



struct SeekableOpcodes {
    pub index: usize,
    pub opCodes: Vec<OpCode>,
    pub start: Option<usize>,
    pub end: Option<usize>
}

impl Iterator for SeekableOpcodes {
    type Item = OpCode;

    fn next(&mut self) -> Option<Self::Item> {
        let n = self.opCodes.get(self.index).cloned();
        self.index += 1;

        n
    }
}

fn argsToString(args: &Vec<DataType>) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.toString().as_str());
        if i != args.len()-1 {
            buf.push_str(", ")
        }
    }
    buf
}

fn genFunName(name: &String, args: &Vec<DataType>) -> String {
    format!("{}({})", name, argsToString(args))
}

fn run(opCodes: SeekableOpcodes, mut vm: VirtualMachine, stackFrame: StackFrame) {
    for op in opCodes {
        match op {
            OpCode::FunBegin => {}
            OpCode::FunName { .. } => {}
            OpCode::FunReturn { .. } => {}
            OpCode::LocalVarTable { .. } => {}
            OpCode::FunEnd => {}
            OpCode::F2I => {
                let mut x = vm.stack.pop().unwrap();
                vm.stack.push(x.f2i())
            }
            OpCode::I2F => {
                let mut x = vm.stack.pop().unwrap();
                vm.stack.push(x.i2f())
            }
            OpCode::PushInt(v) => {
                vm.stack.push(Value::Num(v))
            }
            OpCode::PushFloat(v) => {
                vm.stack.push(Value::Flo(v))
            }
            OpCode::PushBool(v) => {
                vm.stack.push(Value::Bool(v))
            }
            OpCode::Pop => {
                vm.stack.pop()
            }
            OpCode::Dup => {
                let x = vm.stack.pop().unwrap();
                vm.stack.push(x.clone());
                vm.stack.push(x);
            }
            OpCode::PushLocal { .. } => {}
            OpCode::SetLocal { .. } => {}
            OpCode::Jmp { .. } => {}
            OpCode::Call { name, args } => {

            }
            OpCode::Return => {}
            OpCode::Add(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.add(&b, v);
                vm.stack.push(a)
            }
            OpCode::Sub(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.add(&b, v);
                vm.stack.push(a)
            }
            OpCode::Div(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.add(&b, v);
                vm.stack.push(a)
            }
            OpCode::Mul(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.add(&b, v);
                vm.stack.push(a)
            }
            OpCode::Equals(v) => {}
            OpCode::Greater(v) => {}
            OpCode::Less(v) => {}
            OpCode::Or => {}
            OpCode::And => {}
            OpCode::Not => {}
            OpCode::ClassBegin => {}
            OpCode::ClassName { .. } => {}
            OpCode::ClassField { .. } => {}
            OpCode::ClassEnd => {}
            OpCode::New { .. } => {}
            OpCode::GetField { .. } => {}
            OpCode::SetField { .. } => {}
            OpCode::ArrayNew(_) => {}
            OpCode::ArrayStore(_) => {}
            OpCode::ArrayLoad(_) => {}
            OpCode::ArrayLength => {}
        }
    }
}

fn main() {
    let vm = VirtualMachine::new();

}
