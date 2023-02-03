use crate::vm::*;
use crate::vm::DataType::{Bool, Float, Int};
use crate::vm::FuncType::*;
use crate::vm::OpCode::*;
use crate::vm::Value::*;

pub fn checkFunction(opCodes: &mut SeekableOpcodes, abstractStack: &mut AbstractStack, vm: &VirtualMachine) -> bool {
    let mut index = opCodes.index as usize;
    let name = match opCodes.getOpcode(index).unwrap() {
        FunName { name } => name,
        v => {
            return false;
        }
    };
    index += 1;
    let (vars, argCount) = match opCodes.getOpcode(index).unwrap() {
        LocalVarTable { typ, argsCount } => (typ, argsCount),
        v => {
            return false;
        }
    };
    index += 1;
    let ret = match opCodes.getOpcode(index).unwrap() {
        FunReturn { typ } => typ,
        v => {
            return false;
        }
    };
    let clone = ret.clone();
    index += 1;

    let size = abstractStack.len();

    let mut abstractLocals = vec![];

    for var in vars.iter() {
        abstractLocals.push(var.typ.clone())
    }

    if !checkBytecode(opCodes, &mut abstractLocals, abstractStack, vm) {
        return false;
    }

    let last = match abstractStack.stack.last() {
        None => {
            return false;
        }
        Some(v) => v
    };
    println!("lol");

    match clone {
        None => size == abstractStack.len(),
        Some(v) => {
            size == abstractStack.len() + 1 && *last == v
        }
    }
}

pub struct AbstractStack {
    pub stack: Vec<DataType>,
}

impl AbstractStack {
    fn assertPop(&mut self, typ: &DataType) -> bool {
        match self.stack.pop() {
            None => false,
            Some(v) => v == *typ
        }
    }

    fn push(&mut self, typ: DataType) {
        self.stack.push(typ);
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn pop(&mut self) -> Option<DataType> {
        self.stack.pop()
    }
}

#[inline(always)]
pub fn checkBytecode<'a>(opCodes: &mut SeekableOpcodes, abstractLocals: &mut Vec<DataType>, abstractStack: &mut AbstractStack, vm: &VirtualMachine) -> bool {
    loop {
        let (op, index) = match opCodes.nextOpcode() {
            (None, _) => {
                return true;
            }
            (Some(v), i) => (v, i),
        };
        // println!("evaluating {:?}", op);
        match op {
            FunBegin => {
                if !checkFunction(opCodes, abstractStack, vm) {
                    return false;
                }
            }
            FunName { .. } => panic!(),
            FunReturn { .. } => panic!(),
            LocalVarTable { .. } => panic!(),
            FunEnd => panic!(),
            F2I => {
                abstractStack.assertPop(&Float);
                abstractStack.push(Int);
            }
            I2F => {
                abstractStack.assertPop(&Int);
                abstractStack.push(Float);
            }
            PushInt(v) => {
                abstractStack.push(Int);
            }
            PushFloat(v) => {
                abstractStack.push(Float);
            }
            PushBool(v) => {
                abstractStack.push(Bool);
            }
            Pop => {
                if abstractStack.pop() == None {
                    return false;
                }
            }
            Dup => {
                match abstractStack.pop() {
                    None => {
                        return false;
                    }
                    Some(v) => {
                        abstractStack.push(v.clone());
                        abstractStack.push(v);
                    }
                }
            }
            PushLocal { index } => {
                match abstractLocals.get(*index) {
                    None => {
                        return false;
                    }
                    Some(v) => {
                        abstractStack.push(v.clone())
                    }
                }
            }
            SetLocal { index, typ: t } => {
                let x = match abstractStack.pop() {
                    None => {
                        return false;
                    }
                    Some(v) => v
                };
                if *index >= abstractLocals.len() || *index < 0 {
                    return false;
                }
                match abstractLocals.get(*index) {
                    None => {
                        return false;
                    }
                    Some(v) => {
                        if *v != x {
                            return false;
                        }
                    }
                }
                println!("looooool")
            }
            Jmp { offset, jmpType } => {
                panic!()
            }
            Call { encoded } => {
                match vm.functions.get(encoded) {
                    None => {
                        return false;
                    }
                    Some(fun) => {
                        for x in 0..fun.argAmount {
                            abstractStack.assertPop(&fun.varTable[x].typ);
                        }
                        match &fun.returnType {
                            None => {}
                            Some(v) => {
                                abstractStack.push(v.clone())
                            }
                        }
                    }
                }
            }
            Return => return true,
            Add(v) => unsafe {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(v.clone())
            }
            Sub(v) => unsafe {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(v.clone())
            }
            Div(v) => unsafe {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(Float)
            }
            Mul(v) => unsafe {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(v.clone())
            }
            Equals(v) => {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(Bool)
            }
            Greater(v) => {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(Bool)
            }
            Less(v) => unsafe {
                abstractStack.assertPop(v);
                abstractStack.assertPop(v);
                abstractStack.push(Bool)
            }
            Or => {
                abstractStack.assertPop(&Bool);
                abstractStack.assertPop(&Bool);
                abstractStack.push(Bool)
            }
            And => {
                abstractStack.assertPop(&Bool);
                abstractStack.assertPop(&Bool);
                abstractStack.push(Bool)
            }
            Not => {
                abstractStack.assertPop(&Bool);
                abstractStack.push(Bool)
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
                if *index >= abstractLocals.len() || *index < 0 {
                    return false;
                }

                match abstractLocals.get(*index) {
                    None => {
                        return false;
                    }
                    Some(v) => {
                        if *v != *typ {
                            return false;
                        }
                    }
                }
            }
            Dec { typ, index } => {
                if *index >= abstractLocals.len() || *index < 0 {
                    return false;
                }

                match abstractLocals.get(*index) {
                    None => {
                        return false;
                    }
                    Some(v) => {
                        if *v != *typ {
                            return false;
                        }
                    }
                }
            }
        }
        println!("ahhh")
    }
}