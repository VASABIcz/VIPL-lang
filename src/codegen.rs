use std::collections::HashMap;

use Statement::VariableCreate;

use crate::ast::{Expression, FunctionDef, Node, Op, Statement};


use crate::parser::{*};
use crate::vm::OpCode::{*};
use crate::lexer::*;
use crate::parser::ParsingUnitSearchType::*;
use crate::vm::{bootStrapVM, DataType, evaluateBytecode, genFunName, genFunNameMeta, OpCode, run, SeekableOpcodes, StackFrame, VariableMetadata};

fn constructVarTable(
    fun: &FunctionDef,
    functionReturns: &HashMap<String, Option<DataType>>,
) -> (Vec<VariableMetadata>, HashMap<String, (DataType, usize)>) {
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
                None => panic!("OnO"),
                Some(e) => {
                    let returnType = e.toDataType(&registeredVars, functionReturns).unwrap();
                    vTable.push(VariableMetadata {
                        name: v.name.clone(),
                        typ: returnType.clone(),
                    });
                    registeredVars.insert(v.name.clone(), (returnType.clone(), counter));
                    counter += 1;
                }
            }
        }
    }
    (vTable, registeredVars)
}

fn genExpression(
    exp: Expression,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<String, Option<DataType>>,
    vTable: &HashMap<String, (DataType, usize)>,
) {
    match exp {
        Expression::ArithmeticOp { left, right, op } => {
            let dataType = left.toDataType(vTable, functionReturns).unwrap();
            genExpression(*left, ops, functionReturns, vTable);
            genExpression(*right, ops, functionReturns, vTable);
            let t = match op {
                Op::Add => OpCode::Add(dataType),
                Op::Sub => OpCode::Sub(dataType),
                Op::Mul => OpCode::Mul(dataType),
                Op::Div => OpCode::Div(dataType),
                Op::Gt => OpCode::Greater(dataType),
                Op::Less => OpCode::Less(dataType),
                Op::Eq => OpCode::Equals(dataType),
            };
            ops.push(t)
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
                argTypes.push(arg.toDataType(vTable, functionReturns).unwrap());
                genExpression(arg, ops, functionReturns, vTable);
            }

            ops.push(Call {
                encoded: genFunName(&e.name, &argTypes.into_boxed_slice()),
            })
        }
        Expression::Variable(v) => ops.push(OpCode::PushLocal {
            index: vTable.get(&v).unwrap().1,
        }),
    }
}

fn genStatement(
    statement: Statement,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<String, Option<DataType>>,
    vTable: &HashMap<String, (DataType, usize)>,
) {
    match statement {
        Statement::FunctionExpr(e) => {
            let mut argTypes = vec![];

            for arg in e.arguments {
                argTypes.push(arg.toDataType(vTable, functionReturns).unwrap());
                genExpression(arg, ops, functionReturns, vTable);
            }

            ops.push(Call {
                encoded: genFunName(&e.name, &argTypes.into_boxed_slice()),
            })
        }
        VariableCreate(v) => match v.init {
            None => {}
            Some(e) => {
                let t = &e.toDataType(vTable, functionReturns);
                genExpression(e, ops, functionReturns, vTable);
                ops.push(OpCode::SetLocal {
                    index: vTable.get(&v.name).unwrap().1,
                    typ: t.clone().unwrap(),
                })
            }
        },
        Statement::While(_) => {}
        Statement::If(_) => {}
    }
}

fn genFunctionDef(
    fun: FunctionDef,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<String, Option<DataType>>,
) {
    ops.push(OpCode::FunBegin);
    ops.push(FunName {
        name: fun.name.clone(),
    });
    let vTable = constructVarTable(&fun, functionReturns);
    ops.push(LocalVarTable {
        typ: vTable.0.clone().into_boxed_slice(),
        argsCount: fun.argCount,
    });
    ops.push(FunReturn { typ: fun.returnType });

    for statement in fun.body {
        genStatement(statement, ops, functionReturns, &vTable.1);
    }
    ops.push(OpCode::Return);
    ops.push(OpCode::FunEnd);
}

pub fn bytecodeGen(operations: Vec<Operation>) -> (Vec<OpCode>, Vec<DataType>) {
    let mut inlineMain = vec![];
    let mut inlineLocals = HashMap::new();
    let mut ops = vec![];
    let mut functionReturns = HashMap::new();
    let mut counter = 0;
    let mut localTypes = vec![];

    for op in &operations {
        match op {
            Operation::FunctionDef(f) => match f {
                Node::FunctionDef(v) => {
                    functionReturns.insert(
                        genFunNameMeta(&v.name, &v.args.clone().into_boxed_slice()),
                        v.returnType.clone(),
                    );
                }
            },
            Operation::Statement(v) => {
                match v {
                    VariableCreate(c) => {
                        let t = c
                            .init
                            .clone()
                            .unwrap()
                            .toDataType(&inlineLocals, &functionReturns);
                        inlineLocals.insert(c.name.clone(), (t.clone().unwrap(), counter));
                        localTypes.push(t.unwrap().clone());
                        counter += 1;
                    }
                    _ => {}
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
                    genFunctionDef(v.clone(), &mut ops, &functionReturns);
                }
            }
        }
    }

    for op in &inlineMain {
        match op {
            Operation::Statement(s) => {
                genStatement(s.clone(), &mut ops, &functionReturns, &inlineLocals);
            }
            Operation::Expression(e) => {
                genExpression(e.clone(), &mut ops, &functionReturns, &inlineLocals);
            }
            _ => {}
        }
    }

    (ops, localTypes)
}

#[test]
pub fn testLexingUnits() {
    let lexingUnits = lexingUnits();
    // let input = "lol = 666 fn main() { x = -420.69 print(69*x) while x == 1 { print(69) } if true { test(1) } else { kys(1) }}";
    let input = "fn test(x: int): int { print(x) } test(25)";
    let src = SourceProvider {
        data: input,
        index: 0,
    };

    let parsers = parsingUnits();

    let tokens = tokenize(&mut lexingUnits.into_boxed_slice(), src);


    println!("tokens {:?}", &tokens);
    let res = parse(
        &mut TokenProvider::new(tokens),
        Ahead,
        &parsers.into_boxed_slice(),
    );
    println!("{:?}", &res);
    let bs = bytecodeGen(res);

    evaluateBytecode(bs.0, bs.1);
}
