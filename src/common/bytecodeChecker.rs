use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::vm::*;
use crate::vm::DataType::{Bool, Char, Float, Int};
use crate::vm::FuncType::*;
use crate::vm::OpCode::*;
use crate::vm::Value::*;

#[derive(Debug)]
struct InvalidOpcode {
    msg: String
}

impl Display for InvalidOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for InvalidOpcode {}

#[derive(Debug)]
struct GenericException {
    msg: String
}

impl Display for GenericException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for GenericException {}

#[derive(Debug)]
struct OutOfBoundsException {
    max: isize,
    index: isize,
    msg: String
}

impl Display for OutOfBoundsException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "tried to index {} out of bounds index {} bounds 0-{}", self.msg, self.index, self.index)
    }
}

impl Error for OutOfBoundsException {}

pub fn checkFunction(opCodes: &mut SeekableOpcodes, abstractStack: &mut AbstractStack, vm: &mut VirtualMachine, checkedFunctions: &mut HashSet<String>) -> Result<(), Box<dyn Error>> {
    let mut index = opCodes.index as usize;
    let name = match opCodes.getOpcode(index).unwrap() {
        FunName { name } => name,
        v => {
            return Err(Box::new(InvalidOpcode{ msg: format!("FunName") }))
        }
    };
    index += 1;
    let (vars, argCount) = match opCodes.getOpcode(index).unwrap() {
        LocalVarTable { typ, argsCount } => (typ, argsCount),
        v => {
            return Err(Box::new(InvalidOpcode{ msg: format!("LocalVarTable") }))
        }
    };
    index += 1;
    let ret = match opCodes.getOpcode(index).unwrap() {
        FunReturn { typ } => typ,
        v => {
            return Err(Box::new(InvalidOpcode{ msg: format!("FunReturn") }))
        }
    };
    let retClone = ret.clone();
    index += 1;

    let size = abstractStack.len();

    let mut abstractLocals = vec![];

    for var in vars.iter() {
        abstractLocals.push(var.typ.clone())
    }

    let genName = genFunNameMeta(&name, vars);
    checkedFunctions.insert(genName);
    opCodes.index += index as isize;

    checkBytecode(opCodes, &mut abstractLocals, abstractStack, vm, checkedFunctions)?;

    let last = match abstractStack.stack.last() {
        None => {
            if retClone.is_some() {
                return Err(Box::new(InvalidOpcode{ msg: format!("invalid stack state") }))
            } else if size != abstractStack.len() {
                return Err(Box::new(InvalidOpcode{ msg: format!("invalid stack state") }))
            }
            return Ok(())
        }
        Some(v) => v
    };

    match retClone {
        None => {
            if size == abstractStack.len() {
                Ok(())
            }
            else {
                Err(Box::new(InvalidOpcode{ msg: format!("invalid stack state") }))
            }
        },
        Some(v) => {
            if size == abstractStack.len() + 1 && *last == v {
                Ok(())
            }
            else {
                Err(Box::new(InvalidOpcode{ msg: format!("invalid stack state") }))

            }
        }
    }
}

pub struct AbstractStack {
    pub stack: Vec<DataType>,
}

#[derive(Debug)]
pub struct InvalidTypeException {
    expected: DataType,
    actual: Option<DataType>
}

impl Display for InvalidTypeException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.actual {
            None => {
                write!(f, "expected {:?}, got None", self.expected)
            }
            Some(v) => {
                write!(f, "expected {:?}, got {:?}", self.expected, v)
            }
        }
    }
}

impl Error for InvalidTypeException {}

impl AbstractStack {
    fn assertPop(&mut self, typ: &DataType) -> Result<(), Box<dyn Error>> {
        match self.stack.pop() {
            None => Err(Box::new(InvalidTypeException { expected: typ.clone(), actual: None })),
            Some(v) => {
                if v == *typ {
                    Err(Box::new(InvalidTypeException { expected: typ.clone(), actual: Some(v) }))
                }
                else {
                    Ok(())
                }
            }
        }
    }

    fn push(&mut self, typ: DataType) {
        self.stack.push(typ);
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn pop(&mut self) -> Result<DataType, Box<dyn Error>> {
        match self.stack.pop() {
            None => Err(Box::new(GenericException{ msg: format!("empty stack") })),
            Some(v) => Ok(v)
        }
    }
}

#[inline(always)]
pub fn checkBytecode<'a>(opCodes: &mut SeekableOpcodes, abstractLocals: &mut Vec<DataType>, abstractStack: &mut AbstractStack, vm: &mut VirtualMachine, checkedFunctions: &mut HashSet<String>) -> Result<(), Box<dyn Error>> {
    loop {
        let (op, index) = match opCodes.nextOpcode() {
            (None, _) => {
                return Ok(());
            }
            (Some(v), i) => (v, i),
        };
        // println!("checking {:?}", op);
        match op {
            FunBegin => {
                checkFunction(opCodes, abstractStack, vm, checkedFunctions)?;
                opCodes.index += 1;
            }
            FunName { .. } => panic!(),
            FunReturn { .. } => panic!(),
            LocalVarTable { .. } => panic!(),
            FunEnd => panic!(),
            F2I => {
                abstractStack.assertPop(&Float)?;
                abstractStack.push(Int);
            }
            I2F => {
                abstractStack.assertPop(&Int)?;
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
                abstractStack.pop()?;
            }
            Dup => {
                let v = abstractStack.pop()?;
                abstractStack.push(v.clone());
                abstractStack.push(v);
            }
            PushLocal { index } => {
                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException{
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        abstractStack.push(v.clone())
                    }
                }
            }
            SetLocal { index, typ: t } => {
                let x = abstractStack.pop()?;
                if *index >= abstractLocals.len() || *index < 0 {
                    return Err(Box::new(OutOfBoundsException{
                        max: (abstractLocals.len() - 1) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }))
                }
                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException{
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        if *v != x {
                            return Err(Box::new(InvalidTypeException{ expected: x, actual: Some(v.clone()) }))
                        }
                    }
                }
            }
            Jmp { offset, jmpType } => {
            }
            Call { encoded } => {
                if checkedFunctions.contains(encoded) {
                    continue
                }

                match vm.functions.get(encoded) {
                    None => {
                        return Err(Box::new(GenericException{ msg: format!("function {} not found", encoded) }));
                    }
                    Some(fun) => {
                        for x in 0..fun.argAmount {
                            abstractStack.assertPop(&fun.varTable[x].typ)?;
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
            Return => return Ok(()),
            Add(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(v.clone())
            }
            Sub(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(v.clone())
            }
            Div(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(Float)
            }
            Mul(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(v.clone())
            }
            Equals(v) => {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(Bool)
            }
            Greater(v) => {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(Bool)
            }
            Less(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(Bool)
            }
            Or => {
                abstractStack.assertPop(&Bool)?;
                abstractStack.assertPop(&Bool)?;
                abstractStack.push(Bool)
            }
            And => {
                abstractStack.assertPop(&Bool)?;
                abstractStack.assertPop(&Bool)?;
                abstractStack.push(Bool)
            }
            Not => {
                abstractStack.assertPop(&Bool)?;
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
                    return Err(Box::new(OutOfBoundsException{
                        max: (abstractLocals.len() - 1) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }))
                }

                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException{
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        if *v != *typ {
                            return Err(Box::new(InvalidTypeException{ expected: typ.clone(), actual: Some(v.clone()) }))
                        }
                    }
                }
            }
            Dec { typ, index } => {
                if *index >= abstractLocals.len() || *index < 0 {
                    return Err(Box::new(OutOfBoundsException{
                        max: (abstractLocals.len() - 1) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }))
                }

                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException{
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        if *v != *typ {
                            return Err(Box::new(OutOfBoundsException{
                                max: (abstractLocals.len() - 1) as isize,
                                index: *index as isize,
                                msg: "locals".to_string(),
                            }))
                        }
                    }
                }
            }
            PushChar(_) => {
                abstractStack.push(Char)
            }
        }
    }
}