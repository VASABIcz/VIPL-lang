use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::vm::*;
use crate::vm::DataType::{Bool, Char, Float, Int, Object};
use crate::vm::OpCode::*;

#[derive(Debug)]
struct InvalidOpcode {
    msg: String,
}

impl Display for InvalidOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for InvalidOpcode {}

#[derive(Debug)]
struct GenericException {
    msg: String,
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
    msg: String,
}

impl Display for OutOfBoundsException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "tried to index {} out of bounds index {} bounds 0-{}",
            self.msg, self.index, self.index
        )
    }
}

impl Error for OutOfBoundsException {}

pub fn checkFunction(
    opCodes: &mut SeekableOpcodes,
    abstractStack: &mut AbstractStack,
    vm: &mut VirtualMachine,
    checkedFunctions: &mut HashSet<Box<str>>,
) -> Result<(), Box<dyn Error>> {
    let mut index = opCodes.index;
    let name = match opCodes.getOpcode(index).unwrap() {
        FunName { name } => name,
        _v => {
            return Err(Box::new(InvalidOpcode {
                msg: "FunName".to_string(),
            }))
        }
    };
    index += 1;
    let (vars, argCount) = match opCodes.getOpcode(index).unwrap() {
        LocalVarTable { typ, argsCount } => (typ, argsCount),
        _v => {
            return Err(Box::new(InvalidOpcode {
                msg: "LocalVarTable".to_string(),
            }))
        }
    };
    index += 1;
    let ret = match opCodes.getOpcode(index).unwrap() {
        FunReturn { typ } => typ,
        _v => {
            return Err(Box::new(InvalidOpcode {
                msg: "FunReturn".to_string(),
            }))
        }
    };
    let retClone = ret.clone();

    let size = abstractStack.len();

    let mut abstractLocals = vec![];

    for var in vars.iter() {
        abstractLocals.push(var.typ.clone())
    }

    let genName = genFunNameMeta(name.as_str(), vars, *argCount);
    checkedFunctions.insert(genName.clone().into_boxed_str());
    opCodes.index += index;

    checkBytecode(
        opCodes,
        &mut abstractLocals,
        abstractStack,
        vm,
        checkedFunctions,
    )?;

    // println!("check f {}", genName.as_str());

    // FIXME
    opCodes.nextOpcode();

    let last = match abstractStack.stack.last() {
        None => {
            if retClone.is_some() {
                return Err(Box::new(InvalidOpcode {
                    msg: format!("expected function {genName} to return {retClone:?}"),
                }));
            } else if size != abstractStack.len() {
                // println!("{}", abstractStack.len());
                return Err(Box::new(InvalidOpcode {
                    msg: format!("function {genName} corrupted stack a"),
                }));
            }
            return Ok(());
        }
        Some(v) => v,
    };

    match retClone {
        None => {
            if size == abstractStack.len() {
                Ok(())
            } else {
                // println!("{}", abstractStack.len());
                Err(Box::new(InvalidOpcode {
                    msg: format!("function {genName} corrupted stack b"),
                }))
            }
        }
        Some(v) => {
            if *last != v {
                Err(Box::new(InvalidOpcode {
                    msg: format!("function returned wrong type {:?} expected {:?}", *last, v),
                }))
            } else if size + 1 != abstractStack.len() {
                // println!("{}", abstractStack.len());
                Err(Box::new(InvalidOpcode {
                    msg: format!("function {genName} corrupted stack c"),
                }))
            } else {
                Ok(())
            }
        }
    }
}

pub struct AbstractStack {
    pub stack: Vec<DataType>,
}

#[derive(Debug)]
pub struct InvalidTypeException {
    pub(crate) expected: DataType,
    pub(crate) actual: Option<DataType>,
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
            None => Err(Box::new(InvalidTypeException {
                expected: typ.clone(),
                actual: None,
            })),
            Some(v) => {
                if v != *typ {
                    match typ {
                        Object(o) => match o.generics.first() {
                            None => {}
                            Some(g) => {
                                if *g == Generic::Any {
                                    match v {
                                        Object(_) => return Ok(()),
                                        _ => {}
                                    }
                                }
                            }
                        },
                        _ => {}
                    }

                    Err(Box::new(InvalidTypeException {
                        expected: typ.clone(),
                        actual: Some(v),
                    }))
                } else {
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
            None => Err(Box::new(GenericException {
                msg: "empty stack".to_string(),
            })),
            Some(v) => Ok(v),
        }
    }
}

#[inline(always)]
pub fn checkBytecode<'a>(
    opCodes: &mut SeekableOpcodes,
    abstractLocals: &mut Vec<DataType>,
    abstractStack: &mut AbstractStack,
    vm: &mut VirtualMachine,
    checkedFunctions: &mut HashSet<Box<str>>,
) -> Result<(), Box<dyn Error>> {
    loop {
        let (op, _index) = match opCodes.nextOpcode() {
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
            PushInt(_v) => {
                abstractStack.push(Int);
            }
            PushFloat(_v) => {
                abstractStack.push(Float);
            }
            PushBool(_v) => {
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
            PushLocal { index } => match abstractLocals.get(*index) {
                None => {
                    return Err(Box::new(OutOfBoundsException {
                        max: (abstractLocals.len() - 1) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }))
                }
                Some(v) => abstractStack.push(v.clone()),
            },
            SetLocal { index, typ: _t } => {
                let x = abstractStack.pop()?;
                if *index >= abstractLocals.len() {
                    return Err(Box::new(OutOfBoundsException {
                        max: (abstractLocals.len().saturating_sub(1)) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }));
                }
                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException {
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        if *v != x {
                            return Err(Box::new(InvalidTypeException {
                                expected: x,
                                actual: Some(v.clone()),
                            }));
                        }
                    }
                }
            }
            Jmp { offset: _, jmpType } => match jmpType {
                JmpType::One => abstractStack.assertPop(&Int)?,
                JmpType::Zero => abstractStack.assertPop(&Int)?,
                JmpType::Jmp => {}
                JmpType::Gt => {
                    abstractStack.assertPop(&Int)?;
                    abstractStack.assertPop(&Int)?
                }
                JmpType::Less => {
                    abstractStack.assertPop(&Int)?;
                    abstractStack.assertPop(&Int)?
                }
                JmpType::True => abstractStack.assertPop(&Bool)?,
                JmpType::False => abstractStack.assertPop(&Bool)?,
            },
            Call { encoded } => {
                if checkedFunctions.contains(encoded.as_str()) {
                    continue;
                }

                match vm.functions.get(encoded) {
                    None => {
                        return Err(Box::new(GenericException {
                            msg: format!("function {encoded} not found"),
                        }));
                    }
                    Some(fun) => {
                        for x in 0..fun.argAmount {
                            abstractStack.assertPop(&fun.varTable[fun.argAmount - x - 1].typ)?;
                        }
                        match &fun.returnType {
                            None => {}
                            Some(v) => {
                                // println!("returning value {:?}", v);
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
            },
            Sub(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(v.clone())
            },
            Div(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(Float)
            },
            Mul(v) => unsafe {
                abstractStack.assertPop(v)?;
                abstractStack.assertPop(v)?;
                abstractStack.push(v.clone())
            },
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
            Less(v) => {
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
            ArrayNew(t) => {
                abstractStack.assertPop(&Int)?;
                abstractStack.push(DataType::arr(Generic::Type(t.clone())))
            }
            ArrayStore(t) => {
                abstractStack.assertPop(&Int)?;
                abstractStack.assertPop(t)?;
                abstractStack.assertPop(&DataType::arr(Generic::Type(t.clone())))?;
            }
            ArrayLoad(t) => {
                abstractStack.assertPop(&Int)?;
                abstractStack.assertPop(&DataType::arr(Generic::Type(t.clone())))?;
                abstractStack.push(t.clone())
            }
            ArrayLength => panic!(),
            Inc { typ, index } => {
                if *index >= abstractLocals.len() {
                    return Err(Box::new(OutOfBoundsException {
                        max: (abstractLocals.len() - 1) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }));
                }

                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException {
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        if *v != *typ {
                            return Err(Box::new(InvalidTypeException {
                                expected: typ.clone(),
                                actual: Some(v.clone()),
                            }));
                        }
                    }
                }
            }
            Dec { typ, index } => {
                if *index >= abstractLocals.len() || *index < 0 {
                    return Err(Box::new(OutOfBoundsException {
                        max: (abstractLocals.len() - 1) as isize,
                        index: *index as isize,
                        msg: "locals".to_string(),
                    }));
                }

                match abstractLocals.get(*index) {
                    None => {
                        return Err(Box::new(OutOfBoundsException {
                            max: (abstractLocals.len() - 1) as isize,
                            index: *index as isize,
                            msg: "locals".to_string(),
                        }))
                    }
                    Some(v) => {
                        if *v != *typ {
                            return Err(Box::new(OutOfBoundsException {
                                max: (abstractLocals.len() - 1) as isize,
                                index: *index as isize,
                                msg: "locals".to_string(),
                            }));
                        }
                    }
                }
            }
            PushChar(_) => abstractStack.push(Char),
            StrNew(_) => abstractStack.push(DataType::str()),
            GetChar => {
                abstractStack.assertPop(&DataType::Int)?;
                abstractStack.assertPop(&DataType::str())?;
                abstractStack.push(DataType::Char);
            }
            PushIntOne() => {
                abstractStack.push(Int)
            }
            PushIntZero() => {
                abstractStack.push(Int)
            }
            _ => panic!()
        }
    }
}
