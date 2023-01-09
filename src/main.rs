use std::collections::HashMap;
use crate::DataType::*;
use crate::FuncType::*;
use crate::OpCode::*;
use crate::Value::*;

#[derive(Clone, Debug)]
enum DataType<'a> {
    Int,
    Float,
    Bool,
    Array {
        inner: &'a DataType<'a>
    },
    Object {
        name: String
    }
}

impl DataType<'_> {
    fn toString(&self) -> &str {
        match self {
            Int => "int",
            Float => "float",
            Bool => "bool",
            Array { inner } => "",
            Object { name } => &name
        }
    }
}

impl DataType<'_> {
    fn toDefaultValue(&self) -> Value {
        match self {
            Int => Num(0),
            Float => Flo(0.),
            Bool => Bol(false),
            Array { .. } => panic!(),
            Object { .. } => panic!()
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
struct VariableMetadata<'a> {
    pub name: String,
    pub typ: DataType<'a>
}

#[derive(Clone)]
enum OpCode<'a> {
    FunBegin,
    FunName {
        name: String
    },
    FunReturn {
        typ: Option<DataType<'a>>
    },
    LocalVarTable {
        typ: &'a [VariableMetadata<'a>],
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
        typ: DataType<'a>
    },
    Jmp {
        offset: isize,
        jmpType: JmpType,
        typ: DataType<'a>
    },
    Call {
        name: &'a str,
        args: &'a [DataType<'a>],
        encoded: &'a str
    },
    Return,

    Add(DataType<'a>),
    Sub(DataType<'a>),
    Div(DataType<'a>),
    Mul(DataType<'a>),

    Equals(DataType<'a>),
    Greater(DataType<'a>),
    Less(DataType<'a>),

    Or,
    And,
    Not,

    ClassBegin,
    ClassName {
        name: String
    },
    ClassField {
        name: String,
        typ: DataType<'a>
    },
    ClassEnd,
    New {
        name: String
    },
    GetField {
        name: String,
        typ: DataType<'a>
    },
    SetField {
        name: String,
        typ: DataType<'a>
    },

    ArrayNew(DataType<'a>),
    ArrayStore(DataType<'a>),
    ArrayLoad(DataType<'a>),
    ArrayLength,
    Inc {
        typ: DataType<'a>,
        index: usize
    },
    Dec {
        typ: DataType<'a>,
        index: usize
    }
}

struct MyObjectField<'a> {
    pub typ: DataType<'a>,
    pub value: Value
}

enum MyObject<'a> {
    ArrayObj {
        values: Vec<Value>,
        typ: DataType<'a>,
        size: usize
    },
    RuntimeObj {
        name: String,
        fields: Option<HashMap<String, MyObjectField<'a>>>
    }
}

#[derive(Clone)]
struct MyClassField<'a> {
    pub name: String,
    pub typ: DataType<'a>,
}

#[derive(Clone)]
struct MyClass<'a> {
    pub name: String,
    pub fields: HashMap<String, MyClassField<'a>>
}

#[derive(Clone, Debug)]
enum Value {
    Num(isize),
    Flo(f32),
    Bol(bool),
    Reference()
}

impl Value {
    #[inline(always)]
    fn getNum(&self) -> isize {
        match self {
            Num(v) => *v,
            Flo(_) => panic!(),
            Bol(_) => panic!(),
            Reference() => panic!()
        }
    }

    #[inline(always)]
    fn getFlo(&self) -> f32 {
        match self {
            Num(_) => panic!(),
            Flo(v) => *v,
            Bol(_) => panic!(),
            Reference() => panic!()
        }
    }

    #[inline(always)]
    fn getRefFlo(&mut self) -> &mut f32 {
        match self {
            Num(_) => panic!(),
            Flo(v) => v,
            Bol(_) => panic!(),
            Reference() => panic!()
        }
    }

    #[inline(always)]
    fn getRefNum(&mut self) -> &mut isize {
        match self {
            Num(v) => v,
            Flo(_) => panic!(),
            Bol(_) => panic!(),
            Reference() => panic!()
        }
    }

    #[inline(always)]
    fn getRefBol(&mut self) -> &mut bool {
        match self {
            Num(_) => panic!(),
            Flo(_) => panic!(),
            Bol(v) => v,
            Reference() => panic!()
        }
    }

    #[inline(always)]
    fn getBool(&self) -> bool {
        match self {
            Num(_) => panic!(),
            Flo(_) => panic!(),
            Bol(v) => *v,
            Reference() => panic!()
        }
    }
}

impl Value {
    #[inline(always)]
    fn or(&mut self, val: Value) {
        let r = self.getRefBol();
        *r = *r || val.getBool();
    }

    #[inline(always)]
    fn and(&mut self, val: Value) {
        let r = self.getRefBol();
        *r = *r && val.getBool();
    }

    #[inline(always)]
    fn not(&mut self) {
        let r = self.getRefBol();
        *r = !*r;
    }
}

impl Value {
    #[inline(always)]
    fn gt(&self, val: Value, typ: &DataType) -> bool {
        match typ {
            Int => {
                self.getNum() > val.getNum()
            }
            Float => {
                self.getFlo() > val.getFlo()
            },
            Bool => {
                self.getBool() & !val.getBool()
            },
            Array { .. } => panic!(),
            Object { .. } => panic!()
        }
    }

    #[inline(always)]
    fn inc(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum()+=1;
            }
            Float => {
                *self.getRefFlo()+=1.;
            }
            Bool => panic!(),
            Array { .. } => panic!(),
            Object { .. } => panic!()
        }
    }

    #[inline(always)]
    fn dec(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum()-=1;
            }
            Float => {
                *self.getRefFlo()-=1.;
            }
            Bool => panic!(),
            Array { .. } => panic!(),
            Object { .. } => panic!()
        }
    }

    #[inline(always)]
    fn less(&self, val: Value, typ: &DataType) -> bool {
        match typ {
            Int => {
                self.getNum() < val.getNum()
            }
            Float => {
                self.getFlo() < val.getFlo()
            },
            Bool => {
                !self.getBool() & val.getBool()
            },
            Array { .. } => panic!(),
            Object { .. } => panic!()
        }
    }

    #[inline(always)]
    fn eq(&self, val: Value, typ: &DataType) -> bool {
        match typ {
            Int => {
                self.getNum() == val.getNum()
            }
            Float => {
                self.getFlo() == val.getFlo()
            },
            Bool => {
                self.getBool() == val.getBool()
            },
            Array { .. } => panic!(),
            Object { .. } => panic!()
        }
    }
}

impl Value {
    #[inline(always)]
    fn add(&mut self, value: &Value, typ: &DataType) {
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
    fn sub(&mut self, value: &Value, typ: &DataType) {
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
    fn mul(&mut self, value: &Value, typ: &DataType) {
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
    fn div(&mut self, value: &Value, typ: &DataType) {
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
    fn f2i(&mut self) -> Value {
        Num(self.getFlo() as isize)
    }

    #[inline(always)]
    fn i2f(&mut self) -> Value {
        Flo(self.getNum() as f32)
    }
}

impl Value {
    #[inline(always)]
    fn isType(&self, typ: &DataType) -> bool {
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
            Reference() => panic!()
        }
    }
}

#[derive(Debug)]
struct StackFrame<'a> {
    previous: Option<&'a StackFrame<'a>>,
    localVariables: &'a mut [Value],
    name: Option<&'a str>
}

#[derive(Clone)]
struct Func<'a> {
    name: String,
    returnType: Option<DataType<'a>>,
    varTable: &'a [VariableMetadata<'a>],
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
        callback: fn(&mut VirtualMachine, &mut StackFrame) -> (),
    }
}

#[derive(Clone)]
struct VirtualMachine<'a> {
    pub functions: HashMap<String, Func<'a>>,
    pub stack: Vec<Value>,
    pub classes: HashMap<String, MyClass<'a>>,
    pub opCodes: Vec<OpCode<'a>>
}

impl <'a>VirtualMachine<'a> {
    fn new() -> Self {
        Self {
            functions: Default::default(),
            stack: vec![],
            classes: Default::default(),
            opCodes: vec![],
        }
    }

    fn makeNative(&mut self, name: String, args: &'a [VariableMetadata], fun: fn(&mut VirtualMachine, &mut StackFrame) -> (), ret: Option<DataType<'a>>) {
        let genName = genFunNameMeta(&name, args);
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



struct SeekableOpcodes<'a> {
    pub index: isize,
    pub opCodes: &'a[OpCode<'a>],
    pub start: Option<usize>,
    pub end: Option<usize>
}

impl SeekableOpcodes<'_> {
    #[inline(always)]
    fn seek(&mut self, offset: isize) {
        // FIXME boundary check
        self.index += offset;
    }

    #[inline(always)]
    fn next(&mut self) -> Option<&OpCode> {
        let n = self.opCodes.get(self.index as usize);
        self.index += 1;

        n
    }
}

#[inline(always)]
fn argsToString(args: &[DataType]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.toString());
        if i != args.len()-1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline(always)]
fn argsToStringMeta(args: &[VariableMetadata]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(arg.typ.toString());
        if i != args.len()-1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline(always)]
fn genFunName(name: &str, args: &[DataType]) -> String {
    format!("{}({})", name, argsToString(args))
}

#[inline(always)]
fn genFunNameMeta(name: &String, args: &[VariableMetadata]) -> String {
    format!("{}({})", name, argsToStringMeta(args))
}

#[inline(always)]
fn run<'a>(opCodes: &mut SeekableOpcodes, vm: &mut VirtualMachine<'a>, stackFrame: &mut StackFrame) {
    loop {
        let op = match opCodes.next() {
            None => {
                return;
            }
            Some(v) => v
        };
        // println!("processing {:?}", op);
        match op {
            FunBegin => panic!(),
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
            PushInt(v) => {
                vm.stack.push(Num(*v))
            }
            PushFloat(v) => {
                vm.stack.push(Flo(*v))
            }
            PushBool(v) => {
                vm.stack.push(Bol(*v))
            }
            Pop => {
                vm.stack.pop();
            }
            Dup => {
                let x = vm.stack.pop().unwrap();
                vm.stack.push(x.clone());
                vm.stack.push(x);
            }
            PushLocal { index } => {
                vm.stack.push(stackFrame.localVariables.get(*index).unwrap().clone())
            }
            SetLocal { index, typ } => {
                let x = vm.stack.pop().unwrap();
                *(stackFrame.localVariables.get_mut(*index).unwrap()) = x;
                // stackFrame.get_mut().localVariables.insert(*index, x);
            }
            Jmp { offset, jmpType, typ } => {
                match jmpType {
                    JmpType::One => {
                        vm.stack.pop().unwrap();
                        let b = vm.stack.pop().unwrap();
                    }
                    JmpType::Zero => {}
                    JmpType::Jmp => {
                        let x = *offset;
                        opCodes.seek(x)
                    }
                    JmpType::Gt => {
                        let a = vm.stack.pop().unwrap();
                        let b = vm.stack.pop().unwrap();
                        if a.gt(b, typ) {
                            let x = *offset;
                            opCodes.seek(x)
                        }
                    }
                    JmpType::Less => {
                        let a = vm.stack.pop().unwrap();
                        let b = vm.stack.pop().unwrap();
                        if a.less(b, typ) {
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
                }
            }
            Call { name, args, encoded } => {
                // let genName = genFunName(name, args);
                let f = vm.functions.get(*encoded).unwrap();

                let mut localVars = Vec::with_capacity(f.varTable.len());

                for _ in 0..(f.argAmount) {
                    let arg = vm.stack.pop().unwrap();
                    localVars.push(arg)
                }

                for i in 0..(f.varTable.len()-f.argAmount) {
                    localVars.push(f.varTable[i].typ.toDefaultValue())
                }

                let mut stack = StackFrame {
                    previous: Some(stackFrame),
                    localVariables: &mut *localVars.into_boxed_slice(),
                    name: Some(encoded),
                };

                match f.typ {
                    Runtime { rangeStart, rangeStop } => {
                        panic!()
                    }
                    Native { callback } => {
                        callback(vm, &mut stack)
                    }
                }
            }
            Return => {
                return
            }
            Add(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.add(&b, v);
                vm.stack.push(a)
            }
            Sub(v) => {
                let a = vm.stack.pop().unwrap();
                let mut b = vm.stack.pop().unwrap();

                b.sub(&a, v);
                vm.stack.push(b)
            }
            Div(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.div(&b, v);
                vm.stack.push(a)
            }
            Mul(v) => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.mul(&b, v);
                vm.stack.push(a)
            }
            Equals(v) => {
                let a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.eq(b, v);
                vm.stack.push(a);
            }
            Greater(v) => {
                let a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                let res = a.gt(b, v);
                vm.stack.push(Bol(res));
            }
            Less(v) => {
                let a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                let res = a.less(b, v);
                vm.stack.push(Bol(res));
            }
            Or => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.or(b);
                vm.stack.push(a);
            }
            And => {
                let mut a = vm.stack.pop().unwrap();
                let b = vm.stack.pop().unwrap();
                a.and(b);
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
            Inc { typ, index } => {
                stackFrame.localVariables.get_mut(*index).unwrap().inc(typ)
            }
            Dec { typ, index } => {
                stackFrame.localVariables.get_mut(*index).unwrap().dec(typ)
            }
        }
    }
}

fn main() {
    let mut vm = VirtualMachine::new();

    let a = [VariableMetadata { name: "value".to_string(), typ: Int }];
    let b = [VariableMetadata { name: "value".to_string(), typ: Float }];
    vm.makeNative(String::from("print"), &a, |a, b|{
        println!("{}", b.localVariables[0].getNum())
    }, None);

    vm.makeNative(String::from("print"), &b, |a, b|{
        println!("{}", b.localVariables[0].getFlo())
    }, None);

    vm.makeNative(String::from("exec"), &[], |a, b| {
        let stack = match b.previous {
            None => &b,
            Some(v) => v
        };
        let genOps = [
            PushInt(1),
            Pop,
            PushInt(69),
            Call { name: "print", args: &[Int], encoded: "print(int)" }
        ];

        let mut seek = SeekableOpcodes {
            index: 0,
            opCodes: &genOps,
            start: None,
            end: None,
        };

        run(&mut seek, a, b);
    }, None);

    let ops = [
        PushFloat(10000000.),
        SetLocal { index: 0, typ: Float },
        PushLocal { index: 0 },
        PushFloat(0.),
        Less(Float),
        Jmp {
            offset: 1,
            jmpType: JmpType::True,
            typ: Int,
        },
        Return,
        PushLocal { index: 0 },
        Pop,
        // Call { name: "print", args: &[Float], encoded: "print(float)" },
        Dec { typ: Float, index: 0 },
        Jmp {
            offset: -9,
            jmpType: JmpType::Jmp,
            typ: Int,
        }
    ];

    let cc = [
        Call {
            name: "exec",
            args: &[],
            encoded: "exec()"
        }
    ];

    let x = vec![
        vec![
            PushInt(69),
            PushInt(1),
            Add(Int),
            Call {
                name: "print",
                args: &[Int],
                encoded: "print(int)"
            }
        ]
    ];

    let mut seek = SeekableOpcodes {
        index: 0,
        opCodes: &ops,
        start: None,
        end: None,
    };

    let mut stack = StackFrame {
        previous: None,
        localVariables: &mut [Flo(0.)],
        name: None,
    };

    run(&mut seek, &mut vm, &mut stack);
    run(&mut seek, &mut vm, &mut stack);
    println!("{:?}", vm.stack.pop());
}