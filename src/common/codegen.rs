use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io;
use std::io::Write;

use Statement::VariableCreate;

use crate::ast::{Expression, FunctionDef, ModType, Node, Op, Statement};
use crate::lexer::*;
use crate::parser::{*};
use crate::parser::ParsingUnitSearchType::*;
use crate::vm::{bootStrapVM, DataType, evaluateBytecode, genFunName, genFunNameMeta, JmpType, OpCode, run, SeekableOpcodes, StackFrame, VariableMetadata};
use crate::vm::DataType::Bool;
use crate::vm::OpCode::{*};

#[derive(Debug)]
struct NoValue {
    msg: String,
}

impl Display for NoValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for NoValue {}

fn constructVarTable(
    fun: &FunctionDef,
    functionReturns: &HashMap<String, Option<DataType>>,
) -> Result<(Vec<VariableMetadata>, HashMap<String, (DataType, usize)>), Box<dyn Error>> {
    let mut vTable = vec![];
    let mut counter = 0;
    let mut registeredVars = HashMap::new();

    for arg in &fun.args {
        vTable.push(arg.clone());
        registeredVars.insert(arg.name.clone(), (arg.typ.clone(), counter));
        counter += 1
    }

    for statement in &fun.body {
        if let VariableCreate(v) = statement {
            if registeredVars.contains_key(&v.name) {
                continue;
            }
            match &v.init {
                None => return Err(Box::new(NoValue { msg: format!("variable {} must have initializing value", v.name) })),
                Some(e) => {
                    let returnType = e.toDataType(&registeredVars, functionReturns)?;

                    match returnType {
                        None => {
                            return Err(Box::new(NoValue { msg: format!("initializing expression for variable {} must have return value", v.name) }));
                        }
                        Some(ret) => {
                            vTable.push(VariableMetadata {
                                name: v.name.clone(),
                                typ: ret.clone(),
                            });
                            registeredVars.insert(v.name.clone(), (ret.clone(), counter));
                            counter += 1;
                        }
                    }
                }
            }
        }
    }
    Ok((vTable, registeredVars))
}

fn genExpression(
    exp: Expression,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<String, Option<DataType>>,
    vTable: &HashMap<String, (DataType, usize)>,
) -> Result<(), Box<dyn Error>> {
    match exp {
        Expression::ArithmeticOp { left, right, op } => {
            let dataType = left.toDataType(vTable, functionReturns)?;
            match dataType {
                None => {
                    return Err(Box::new(NoValue { msg: format!("expression must have return value") }));
                }
                Some(dat) => {
                    genExpression(*left, ops, functionReturns, vTable)?;
                    genExpression(*right, ops, functionReturns, vTable)?;
                    let t = match op {
                        Op::Add => OpCode::Add(dat),
                        Op::Sub => OpCode::Sub(dat),
                        Op::Mul => OpCode::Mul(dat),
                        Op::Div => OpCode::Div(dat),
                        Op::Gt => OpCode::Greater(dat),
                        Op::Less => OpCode::Less(dat),
                        Op::Eq => OpCode::Equals(dat),
                    };
                    ops.push(t);
                }
            }
        }
        Expression::IntLiteral(i) => ops.push(PushInt(i.parse::<isize>().unwrap())),
        Expression::LongLiteral(i) => ops.push(PushInt(i.parse::<isize>().unwrap())),
        Expression::FloatLiteral(i) => ops.push(OpCode::PushFloat(i.parse::<f32>().unwrap())),
        Expression::DoubleLiteral(i) => ops.push(OpCode::PushFloat(i.parse::<f32>().unwrap())),
        Expression::StringLiteral(_) => {}
        Expression::BoolLiteral(i) => ops.push(OpCode::PushBool(i)),
        Expression::FunctionCall(e) => {
            let mut argTypes = vec![];

            for arg in e.arguments {
                let t = arg.toDataType(vTable, functionReturns)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue { msg: String::from("aahhh") }));
                    }
                    Some(v) => {
                        argTypes.push(v);
                        genExpression(arg, ops, functionReturns, vTable)?;
                    }
                }
            }

            ops.push(Call {
                encoded: genFunName(&e.name, &argTypes),
            })
        }
        Expression::Variable(v) => {
            let res = match vTable.get(&v) {
                None => {
                    return Err(Box::new(VariableNotFound { name: v }));
                }
                Some(v) => v
            };
            ops.push(OpCode::PushLocal {
                index: vTable.get(&v).unwrap().1,
            })
        }
        Expression::CharLiteral(c) => {
            ops.push(PushChar(c))
        }
    }
    Ok(())
}

#[derive(Debug)]
struct VariableNotFound {
    name: String,
}

impl Display for VariableNotFound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "variable {} not found", self.name)
    }
}

impl Error for VariableNotFound {}

fn genStatement(
    statement: Statement,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<String, Option<DataType>>,
    vTable: &HashMap<String, (DataType, usize)>,
) -> Result<(), Box<dyn Error>> {
    match statement {
        Statement::FunctionExpr(e) => {
            let mut argTypes = vec![];

            for arg in e.arguments {
                let t = arg.toDataType(vTable, functionReturns).unwrap();
                match t {
                    None => return Err(Box::new(NoValue { msg: "e".to_string() })),
                    Some(v) => {
                        argTypes.push(v);
                        genExpression(arg, ops, functionReturns, vTable)?;
                    }
                }
            }

            ops.push(Call {
                encoded: genFunName(&e.name, &argTypes),
            });
        }
        VariableCreate(v) => match v.init {
            None => {}
            Some(e) => {
                let t = &e.toDataType(vTable, functionReturns)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue { msg: String::from("idk") }));
                    }
                    Some(ve) => {
                        genExpression(e, ops, functionReturns, vTable)?;
                        ops.push(OpCode::SetLocal {
                            index: vTable.get(&v.name).unwrap().1,
                            typ: ve.clone(),
                        });
                    }
                }
            }
        },
        Statement::While(w) => {
            let ret = w.exp.toDataType(vTable, functionReturns)?;
            match ret {
                None => {
                    return Err(Box::new(NoValue { msg: format!("expression {:?} must return bool", w.exp) }));
                }
                Some(ve) => {
                    if ve != Bool {
                        return Err(Box::new(NoValue { msg: format!("expected bool got {:?} {:?}", ve, w.exp) }));
                    }
                    let size = ops.len();
                    genExpression(w.exp, ops, functionReturns, vTable)?;
                    let mut bodyBuf = vec![];
                    for s in w.body {
                        genStatement(s, &mut bodyBuf, functionReturns, vTable)?;
                    }
                    let len = bodyBuf.len();
                    ops.push(Not);
                    ops.push(OpCode::Jmp { offset: len as isize + 1, jmpType: JmpType::True });
                    ops.extend(bodyBuf);
                    ops.push(OpCode::Jmp { offset: (ops.len() as isize - size as isize + 1) * -1, jmpType: JmpType::Jmp })
                }
            }
        }
        Statement::If(_) => {}
        Statement::Return(ret) => {
            genExpression(ret.exp, ops, functionReturns, vTable)?;
            ops.push(OpCode::Return)
        }
        Statement::VariableMod(m) => {
            match vTable.get(&m.varName) {
                None => {
                    return Err(Box::new(VariableNotFound { name: m.varName }));
                }
                Some(v) => {
                    ops.push(OpCode::PushLocal { index: v.1 });
                    let op = match m.modType {
                        ModType::Add => OpCode::Add(v.0.clone()),
                        ModType::Sub => OpCode::Sub(v.0.clone()),
                        ModType::Div => OpCode::Div(v.0.clone()),
                        ModType::Mul => OpCode::Mul(v.0.clone())
                    };
                    genExpression(m.expr, ops, functionReturns, vTable)?;
                    ops.push(op);
                    ops.push(OpCode::SetLocal { index: v.1, typ: v.0.clone() });
                }
            }
        }
    }
    Ok(())
}

fn genFunctionDef(
    fun: FunctionDef,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<String, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    ops.push(OpCode::FunBegin);
    ops.push(FunName {
        name: fun.name.clone(),
    });
    let vTable = constructVarTable(&fun, functionReturns)?;
    ops.push(LocalVarTable {
        typ: vTable.0.clone().into_boxed_slice(),
        argsCount: fun.argCount,
    });
    ops.push(FunReturn { typ: fun.returnType });

    for statement in fun.body {
        genStatement(statement, ops, functionReturns, &vTable.1)?;
    }
    ops.push(OpCode::Return);
    ops.push(OpCode::FunEnd);
    Ok(())
}

pub fn complexBytecodeGen(operations: Vec<Operation>, localTypes: &mut Vec<DataType>, functionReturns: &mut HashMap<String, Option<DataType>>, mainLocals: &mut HashMap<String, (DataType, usize)>) -> Result<Vec<OpCode>, Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut ops = vec![];
    let mut counter = localTypes.len();

    for op in &operations {
        match op {
            Operation::FunctionDef(f) => match f {
                Node::FunctionDef(v) => {
                    functionReturns.insert(
                        genFunNameMeta(&v.name, &v.args.clone()),
                        v.returnType.clone(),
                    );
                }
            },
            Operation::Statement(v) => {
                if let VariableCreate(c) = v {
                    match c.init {
                        None => {
                            return Err(Box::new(NoValue { msg: "ahhh".to_string() }));
                        }
                        Some(ref ex) => {
                            let t = ex.clone().toDataType(&mainLocals, &functionReturns)?;
                            if !mainLocals.contains_key(&c.name) {
                                mainLocals.insert(c.name.clone(), (t.clone().unwrap(), counter));
                                localTypes.push(t.unwrap().clone());
                                counter += 1;
                            }
                        }
                    }
                }
                inlineMain.push(op.clone())
            }
            _ => inlineMain.push(op.clone()),
        }
    }

    for op in &operations {
        if let Operation::FunctionDef(f) = op {
            match f {
                Node::FunctionDef(v) => {
                    genFunctionDef(v.clone(), &mut ops, &functionReturns)?;
                }
            }
        }
    }

    for op in &inlineMain {
        match op {
            Operation::Statement(s) => {
                genStatement(s.clone(), &mut ops, &functionReturns, &mainLocals)?;
            }
            Operation::Expression(e) => {
                genExpression(e.clone(), &mut ops, &functionReturns, &mainLocals)?;
            }
            _ => {}
        }
    }

    Ok(ops)
}

pub fn bytecodeGen(operations: Vec<Operation>) -> Result<(Vec<OpCode>, Vec<DataType>), Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut mainLocals = HashMap::new();
    let mut ops = vec![];
    let mut functionReturns = HashMap::new();
    let mut counter = 0;
    let mut localTypes = vec![];

    for op in &operations {
        match op {
            Operation::FunctionDef(f) => match f {
                Node::FunctionDef(v) => {
                    functionReturns.insert(
                        genFunNameMeta(&v.name, &v.args.clone()),
                        v.returnType.clone(),
                    );
                }
            },
            Operation::Statement(v) => {
                if let VariableCreate(c) = v {
                    match c.init {
                        None => {
                            return Err(Box::new(NoValue { msg: "ahhh".to_string() }));
                        }
                        Some(ref ex) => {
                            let t = ex.clone().toDataType(&mainLocals, &functionReturns)?;
                            mainLocals.insert(c.name.clone(), (t.clone().unwrap(), counter));
                            localTypes.push(t.unwrap().clone());
                            counter += 1;
                        }
                    }
                }
                inlineMain.push(op.clone())
            }
            _ => inlineMain.push(op.clone()),
        }
    }

    for op in &operations {
        if let Operation::FunctionDef(f) = op {
            match f {
                Node::FunctionDef(v) => {
                    genFunctionDef(v.clone(), &mut ops, &functionReturns)?;
                }
            }
        }
    }

    for op in &inlineMain {
        match op {
            Operation::Statement(s) => {
                genStatement(s.clone(), &mut ops, &functionReturns, &mainLocals)?;
            }
            Operation::Expression(e) => {
                genExpression(e.clone(), &mut ops, &functionReturns, &mainLocals)?;
            }
            _ => {}
        }
    }

    Ok((ops, localTypes))
}

#[test]
pub fn testLexingUnits() {
    let input = "fn test(x: int): int { print( x ) } test ( 25 ) ";

    let tokens = tokenizeSource(input).unwrap();
    println!("tokens {:?}", &tokens);

    let res = parseTokens(tokens).unwrap();
    println!("{:?}", &res);

    let bs = bytecodeGen(res).unwrap();

    evaluateBytecode(bs.0, bs.1);
}