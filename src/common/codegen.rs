use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io;
use std::io::Write;

use Statement::VariableCreate;

use crate::ast::{Expression, FunctionDef, ModType, Node, Op, Statement};
use crate::lexer::*;
use crate::lexer::TokenType::Var;
use crate::objects::Str;
use crate::optimalizer::evalExpr;
use crate::parser::{*};
use crate::parser::ParsingUnitSearchType::*;
use crate::vm::{DataType, evaluateBytecode, Generic, genFunName, genFunNameMeta, JmpType, OpCode, run, SeekableOpcodes, StackFrame, Value, VariableMetadata};
use crate::vm::DataType::{Bool, Int};
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

fn genExpression(
    exp: Expression,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<Box<str>, Option<DataType>>,
    vTable: &HashMap<Box<str>, (DataType, usize)>,
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
                        Op::And => OpCode::And,
                        Op::Or => OpCode::Or
                    };
                    ops.push(t);
                }
            }
        }
        Expression::IntLiteral(i) => ops.push(PushInt(i.parse::<isize>().unwrap())),
        Expression::LongLiteral(i) => ops.push(PushInt(i.parse::<isize>().unwrap())),
        Expression::FloatLiteral(i) => ops.push(OpCode::PushFloat(i.parse::<f32>().unwrap())),
        Expression::DoubleLiteral(i) => ops.push(OpCode::PushFloat(i.parse::<f32>().unwrap())),
        Expression::StringLiteral(i) => {
            ops.push(StrNew(i.into_boxed_str()));
        }
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
                encoded: genFunName(&e.name, &argTypes).into_boxed_str(),
            })
        }
        Expression::Variable(v) => {
            let res = match vTable.get(&v.clone().into_boxed_str()) {
                None => {
                    return Err(Box::new(VariableNotFound { name: v }));
                }
                Some(v) => v
            };
            ops.push(OpCode::PushLocal {
                index: vTable.get(&v.into_boxed_str()).unwrap().1,
            })
        }
        Expression::CharLiteral(c) => {
            ops.push(PushChar(c))
        }
        Expression::ArrayLiteral(i) => {
            let d = i.get(0).ok_or("array must have at least one element")?.toDataType(vTable, functionReturns)?.ok_or("array elements must have type")?;
            ops.push(PushInt(i.len() as isize));
            ops.push(ArrayNew(d.clone()));
            for (ind, exp) in i.iter().enumerate() {
                ops.push(Dup);
                genExpression(exp.clone(), ops, functionReturns, vTable)?;
                ops.push(PushInt(ind as isize));
                ops.push(ArrayStore(d.clone()));
            }
        }
        Expression::ArrayIndexing(i) => {
            // println!("{:?}", i.expr);
            let d = i.expr.toDataType(vTable, functionReturns)?.ok_or("ewgergreg")?;
            match d {
                DataType::Object(o) => {
                    // println!("{:?}", o);
                    if &*o.name == "String" {
                        genExpression(i.expr, ops, functionReturns, vTable)?;
                        genExpression(i.index, ops, functionReturns, vTable)?;
                        ops.push(GetChar);
                        return Ok(())
                    }

                    match o.generics.first().unwrap() {
                        Generic::Type(v) => {
                            genExpression(i.expr, ops, functionReturns, vTable)?;
                            genExpression(i.index, ops, functionReturns, vTable)?;

                            ops.push(OpCode::ArrayLoad(v.clone()));
                        },
                        Generic::Any => panic!()
                    }
                }
                v => panic!("{:?}", v)
            }
        }
        Expression::NotExpression(e) => {
            genExpression(*e, ops, functionReturns, vTable)?;
            ops.push(Not)
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
    functionReturns: &HashMap<Box<str>, Option<DataType>>,
    vTable: &HashMap<Box<str>, (DataType, usize)>,
    loopContext: Option<usize>,
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
                encoded: genFunName(&e.name, &argTypes).into_boxed_str(),
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
                        // println!("{}", &v.name);
                        ops.push(OpCode::SetLocal {
                            index: vTable.get(&v.name.into_boxed_str()).unwrap().1,
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
                        genStatement(s, &mut bodyBuf, functionReturns, vTable, Some(size))?;
                    }
                    let len = bodyBuf.len();
                    ops.push(OpCode::Jmp { offset: len as isize + 1, jmpType: JmpType::False });
                    ops.extend(bodyBuf);
                    ops.push(OpCode::Jmp { offset: (ops.len() as isize - size as isize + 1) * -1, jmpType: JmpType::Jmp })
                }
            }
        }
        Statement::If(flow) => {
            let mut buf = vec![];
            for s in flow.body {
                genStatement(s, &mut buf, functionReturns, vTable, loopContext)?;
            }

            genExpression(flow.condition, ops, functionReturns, vTable)?;
            let mut jumpDist = buf.len() as isize;
            if flow.elseBody.is_some() {
                jumpDist += 1;
            }
            ops.push(OpCode::Jmp { offset: jumpDist, jmpType: JmpType::False });
            ops.extend(buf);

            match flow.elseBody {
                None => {}
                Some(els) => {
                    buf = vec![];
                    for s in els {
                        genStatement(s, &mut buf, functionReturns, vTable, loopContext)?;
                    }

                    ops.push(OpCode::Jmp { offset: buf.len() as isize, jmpType: JmpType::Jmp });
                    ops.extend(buf);
                }
            }
        },
        Statement::Return(ret) => {
            genExpression(ret.exp, ops, functionReturns, vTable)?;
            ops.push(OpCode::Return)
        }
        Statement::VariableMod(m) => {
            match vTable.get(&m.varName.clone().into_boxed_str()) {
                None => {
                    return Err(Box::new(VariableNotFound { name: m.varName }));
                }
                Some(v) => {
                    let op = match m.modType {
                        ModType::Add => OpCode::Add(v.0.clone()),
                        ModType::Sub => OpCode::Sub(v.0.clone()),
                        ModType::Div => OpCode::Div(v.0.clone()),
                        ModType::Mul => OpCode::Mul(v.0.clone())
                    };
                    match evalExpr(&m.expr) {
                        None => {
                            ops.push(OpCode::PushLocal { index: v.1 });
                            genExpression(m.expr, ops, functionReturns, vTable)?;
                            ops.push(op);
                            ops.push(OpCode::SetLocal { index: v.1, typ: v.0.clone() });
                        }
                        Some(e) => {
                            if e.isType(&Int) && e.getNum() == 1 {
                                // panic!("this is good panic");
                                match m.modType {
                                    ModType::Add => {
                                        ops.push(Inc { typ: DataType::Int, index: v.1 });
                                        return Ok(())
                                    }
                                    ModType::Sub => {
                                        ops.push(Inc { typ: DataType::Int, index: v.1 });
                                        return Ok(())
                                    }
                                    ModType::Div => {}
                                    ModType::Mul => {}
                                }
                            }
                        }
                    }
                }
            }
        }
        Statement::ArrayAssign { left, right } => {
            genExpression(left.expr, ops, functionReturns, vTable)?;
            let t = right.toDataType(vTable, functionReturns)?.ok_or("cant assign void to array")?;
            genExpression(right, ops, functionReturns, vTable)?;
            genExpression(left.index, ops, functionReturns, vTable)?;
            ops.push(ArrayStore(t))
        }
        Statement::Continue => {
            let index = loopContext.ok_or("continue can be only used in loops")?;
            ops.push(Jmp { offset: (index - ops.len() + 2) as isize, jmpType: JmpType::Jmp })
        },
        Statement::Break => panic!(),
        Statement::Loop(body) => {
            let mut buf = vec![];
            let context = ops.len();
            for s in body {
                genStatement(s, &mut buf, functionReturns, vTable, Some(context))?;
            }
            let bufLen = buf.len() as isize;
            ops.extend(buf);
            ops.push(OpCode::Jmp { offset: (bufLen + 1) * -1, jmpType: JmpType::Jmp });
        }
    }
    Ok(())
}

fn genFunctionDef(
    fun: FunctionDef,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<Box<str>, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    ops.push(OpCode::FunBegin);
    ops.push(FunName {
        name: fun.name.clone().into_boxed_str(),
    });

    let mut idk1 = HashMap::new();
    let mut idk2 = vec![];

    for arg in fun.args {
        idk1.insert(String::from(arg.name.clone()).into_boxed_str(), (arg.typ.clone(), idk2.len()));
        idk2.push(arg.clone())
    }

    for s in &fun.body {
        buildLocalsTable(s, &mut idk1, &mut idk2, functionReturns)?;
    }

    // let vTable = constructVarTable(&fun, functionReturns)?;
    ops.push(LocalVarTable {
        typ: idk2.into_boxed_slice(),
        argsCount: fun.argCount,
    });
    ops.push(FunReturn { typ: fun.returnType });

    for statement in fun.body {
        genStatement(statement, ops, functionReturns, &idk1, None)?;
    }
    ops.push(OpCode::Return);
    ops.push(OpCode::FunEnd);
    Ok(())
}

pub fn complexBytecodeGen(operations: Vec<Operation>, localTypes: &mut Vec<DataType>, functionReturns: &mut HashMap<Box<str>, Option<DataType>>, mainLocals: &mut HashMap<Box<str>, (DataType, usize)>) -> Result<Vec<OpCode>, Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut ops = vec![];
    let mut counter = localTypes.len();

    for op in &operations {
        match op {
            Operation::FunctionDef(f) => match f {
                Node::FunctionDef(v) => {
                    functionReturns.insert(
                        genFunNameMeta(&v.name, &v.args.clone(), v.argCount).into_boxed_str(),
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
                            if !mainLocals.contains_key(&c.name.clone().into_boxed_str()) {
                                mainLocals.insert(c.name.clone().into_boxed_str(), (t.clone().unwrap(), counter));
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
                genStatement(s.clone(), &mut ops, &functionReturns, &mainLocals, None)?;
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
                        genFunNameMeta(&v.name, &v.args.clone(), v.argCount).into_boxed_str(),
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
                            mainLocals.insert(c.name.clone().into_boxed_str(), (t.clone().unwrap(), counter));
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
                genStatement(s.clone(), &mut ops, &functionReturns, &mainLocals, None)?;
            }
            Operation::Expression(e) => {
                genExpression(e.clone(), &mut ops, &functionReturns, &mainLocals)?;
            }
            _ => {}
        }
    }

    Ok((ops, localTypes))
}

pub fn buildLocalsTable(statement: &Statement, mainLocals: &mut HashMap<Box<str>, (DataType, usize)>, localTypes: &mut Vec<VariableMetadata>, functionReturns: &HashMap<Box<str>, Option<DataType>>) -> Result<(), Box<dyn Error>> {
    match statement {
        VariableCreate(c) => {
            let res = c.init.clone().ok_or("variable expected initializer")?;
            let t = res.toDataType(&mainLocals, &functionReturns)?;
            // println!("creating variable {} type {:?}", &c.name, &t);
            mainLocals.insert(c.name.clone().into_boxed_str(), (t.clone().unwrap(), localTypes.len()));
            localTypes.push(VariableMetadata { name: c.name.clone().into_boxed_str(), typ: t.unwrap().clone() });
        }
        Statement::While(w) => {
            for s in &w.body {
                buildLocalsTable(s, mainLocals, localTypes, functionReturns)?;
            }
        }
        Statement::If(i) => {
            for s in &i.body {
                buildLocalsTable(s, mainLocals, localTypes, functionReturns)?;
            }
            if let Some(body) = &i.elseBody {
                for s in body {
                    buildLocalsTable(s, mainLocals, localTypes, functionReturns)?;
                }
            }
        }
        Statement::Loop(body) => {
            for s in body {
                buildLocalsTable(s, mainLocals, localTypes, functionReturns)?;
            }
        }
        Statement::FunctionExpr(_) => {}
        Statement::VariableMod(_) => {}
        Statement::Return(_) => {}
        Statement::ArrayAssign { .. } => {}
        Statement::Continue => {}
        Statement::Break => {}
    }

    Ok(())
}

pub fn bytecodeGen2(operations: Vec<Operation>, functionReturns: &mut HashMap<Box<str>, Option<DataType>>) -> Result<(Vec<OpCode>, Vec<DataType>), Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut mainLocals = HashMap::new();
    let mut ops = vec![];
    let mut localTypes = vec![];

    for op in &operations {
        if let Operation::Statement(stat) = op {
            buildLocalsTable(stat, &mut mainLocals, &mut localTypes, functionReturns)?;
            inlineMain.push(op);
        } else if let Operation::Expression(Expression::FunctionCall(call)) = op {
            buildLocalsTable(&Statement::FunctionExpr(call.clone()), &mut mainLocals, &mut localTypes, functionReturns)?;
            inlineMain.push(op);
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
                genStatement(s.clone(), &mut ops, &functionReturns, &mainLocals, None)?;
            }
            Operation::Expression(e) => {
                genExpression(e.clone(), &mut ops, &functionReturns, &mainLocals)?;
            }
            _ => {}
        }
    }

    Ok((ops, localTypes.iter().map(|it| {
        it.typ.clone()
    }).collect()))
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