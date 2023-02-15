use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;

use Statement::VariableCreate;

use crate::ast::{Expression, FunctionDef, ModType, Node, Op, Statement, StructDef};
use crate::lexer::*;
use crate::optimizer::evalExpr;
use crate::parser::{*};
use crate::vm::{DataType, Generic, genFunName, genFunNameMeta, JmpType, MyStr, OpCode, VariableMetadata};
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
    out: &mut String,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
    vTable: &HashMap<MyStr, (DataType, usize)>,
) -> Result<(), Box<dyn Error>> {
    match exp {
        Expression::ArithmeticOp { left, right, op } => {
            let dataType = left.toDataType(vTable, functionReturns)?;
            match dataType {
                None => {
                    return Err(Box::new(NoValue { msg: "expression must have return value".to_string() }));
                }
                Some(dat) => {
                    genExpression(*left, out, functionReturns, vTable)?;
                    let t = match op {
                        Op::Add => "+",
                        Op::Sub => "-",
                        Op::Mul => "*",
                        Op::Div => "/",
                        Op::Gt => ">",
                        Op::Less => "<",
                        Op::Eq => "==",
                        Op::And => "&&",
                        Op::Or => panic!()
                    };
                    out.push_str(t);
                    genExpression(*right, out, functionReturns, vTable)?;
                }
            }
        }
        Expression::IntLiteral(i) => out.push_str(&i),
        Expression::LongLiteral(i) => out.push_str(&i),
        Expression::FloatLiteral(i) => out.push_str(&i),
        Expression::DoubleLiteral(i) => out.push_str(&i),
        Expression::StringLiteral(i) => {
            out.push('"');
            out.push_str(&i);
            out.push('"');
        }
        Expression::BoolLiteral(i) => if i { out.push_str("true") } else { out.push_str("false") },
        Expression::FunctionCall(e) => {
            out.push_str(&e.name);
            out.push_str("(");

            let argsLen = e.arguments.len();

            for (i, arg) in e.arguments.into_iter().enumerate() {
                let t = arg.toDataType(vTable, functionReturns)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue { msg: String::from("aahhh") }));
                    }
                    Some(v) => {
                        genExpression(arg, out, functionReturns, vTable)?;
                        if i != argsLen - 1 {
                            out.push(',')
                        }
                    }
                }
            }
            out.push_str(")");
        }
        Expression::Variable(v) => {
            out.push_str(&v)
        }
        Expression::CharLiteral(c) => {
            panic!();
            out.push('c')
        }
        Expression::ArrayLiteral(i) => {
            out.push('{');

            let d = i.get(0).ok_or("array must have at least one element")?.toDataType(vTable, functionReturns)?.ok_or("array elements must have type")?;
            for (ind, exp) in i.iter().enumerate() {
                genExpression(exp.clone(), out, functionReturns, vTable)?;

                if ind != i.len() - 1 {
                    out.push(',')
                }
            }

            out.push('}');
        }
        Expression::ArrayIndexing(i) => {
            genExpression(i.expr, out, functionReturns, vTable)?;
            out.push('[');
            genExpression(i.index, out, functionReturns, vTable)?;
            out.push(']');
        }
        Expression::NotExpression(e) => {
            out.push('!');
            genExpression(*e, out, functionReturns, vTable)?;
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
    out: &mut String,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
    vTable: &HashMap<MyStr, (DataType, usize)>,
    loopContext: Option<usize>,
) -> Result<(), Box<dyn Error>> {
    match statement {
        Statement::FunctionExpr(e) => {
            out.push_str(&e.name);
            out.push_str("(");

            let argsLen = e.arguments.len();

            for (i, arg) in e.arguments.into_iter().enumerate() {
                let t = arg.toDataType(vTable, functionReturns)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue { msg: String::from("aahhh") }));
                    }
                    Some(v) => {
                        genExpression(arg, out, functionReturns, vTable)?;
                        if i != argsLen - 1 {
                            out.push(',')
                        }
                    }
                }
            }
            out.push_str(")");
            out.push(';');
        }
        VariableCreate(v) => match v.init {
            None => panic!(),
            Some(e) => {
                let t = &e.toDataType(vTable, functionReturns)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue { msg: String::from("idk") }));
                    }
                    Some(ve) => {
                        out.push_str(&v.name);
                        out.push('=');
                        genExpression(e, out, functionReturns, vTable)?;
                        out.push(';');
                    }
                }
            }
        },
        Statement::While(w) => {
            out.push_str("while (");
            genExpression(w.exp, out, functionReturns, vTable)?;
            out.push_str(") { ");

            for s in w.body {
                genStatement(s, out, functionReturns, vTable, None)?;
            }
            out.push_str(" }");
        }
        Statement::If(flow) => {
            out.push_str("if (");
            genExpression(flow.condition, out, functionReturns, vTable)?;
            out.push_str(") {");

            for s in flow.body {
                genStatement(s, out, functionReturns, vTable, None)?;
            }

            out.push_str(" }");

            match flow.elseBody {
                None => {}
                Some(v) => {
                    out.push_str(" else { ");

                    for s in v {
                        genStatement(s, out, functionReturns, vTable, None)?;
                    }

                    out.push_str(" }");
                }
            }
        }
        Statement::Return(ret) => {
            out.push_str("return ");
            genExpression(ret.exp, out, functionReturns, vTable)?;
            out.push_str(";");
        }
        Statement::VariableMod(m) => {
            panic!()
        }
        Statement::ArrayAssign { left, right } => {
            genExpression(left.expr, out, functionReturns, vTable)?;
            out.push('[');
            genExpression(left.index, out, functionReturns, vTable)?;
            out.push_str("] = ");
            genExpression(right, out, functionReturns, vTable)?;
            out.push(';');
        }
        Statement::Continue => {
            out.push_str("continue;")
        }
        Statement::Break => out.push_str("break;"),
        Statement::Loop(body) => {
            out.push_str("while (1) { ");
            for s in body {
                genStatement(s, out, functionReturns, vTable, None)?;
            }
            out.push_str(" }");
        }
    }
    Ok(())
}

fn genFunctionDef(
    fun: FunctionDef,
    out: &mut String,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    match fun.returnType {
        None => {
            out.push_str("void");
        }
        Some(v) => {
            out.push_str(&v.toString());
        }
    };
    out.push(' ');
    out.push_str(&fun.name);
    out.push('(');

    for i in 0..fun.argCount {
        let a = fun.args.get(i).unwrap();
        out.push_str(a.typ.toString());
        out.push(' ');
        out.push_str(a.name.as_str());
        if i != fun.argCount {
            out.push(',');
        }
    }

    out.push_str(") { ");

    let mut e = HashMap::new();

    for arg in fun.args {
        e.insert(arg.name.clone(), (arg.typ.clone(), 0usize));
    }

    for a in fun.body {
        genStatement(a, out, functionReturns, &e, None)?;
    }

    out.push_str(" }");

    Ok(())
}

pub fn genStructDef(
    struc: StructDef,
    ops: &mut Vec<OpCode>,
    _functionReturns: &HashMap<MyStr, Option<DataType>>,
    _structs: &HashMap<MyStr, HashMap<String, DataType>>,
) -> Result<(), Box<dyn Error>> {
    ops.push(ClassBegin);
    ops.push(ClassName { name: MyStr::Runtime(struc.name.into_boxed_str()) });

    for (n, typ) in struc.fields {
        ops.push(ClassField { name: n.into(), typ })
    }

    ops.push(ClassEnd);
    Ok(())
}

pub fn bytecodeGen(operations: Vec<Operation>) -> Result<String, Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut mainLocals = HashMap::new();
    let mut out = String::new();
    let mut functionReturns = HashMap::new();
    let mut counter = 0;
    let mut localTypes = vec![];
    let mut structs = HashMap::new();

    for op in &operations {
        match op {
            Operation::Global(f) => match f {
                Node::FunctionDef(v) => {
                    functionReturns.insert(
                        MyStr::Runtime(genFunNameMeta(&v.name, &v.args.clone(), v.argCount).into_boxed_str()),
                        v.returnType.clone(),
                    );
                }
                Node::StructDef(v) => {
                    structs.insert(MyStr::Runtime(v.name.clone().into_boxed_str()), v.fields.clone());
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
                            mainLocals.insert(MyStr::Runtime(c.name.clone().into_boxed_str()), (t.clone().unwrap(), counter));
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
        if let Operation::Global(f) = op {
            match f {
                Node::FunctionDef(v) => {
                    genFunctionDef(v.clone(), &mut out, &functionReturns)?;
                }
                Node::StructDef(v) => {
                    panic!()
                    //genStructDef(v.clone(), &mut out, &functionReturns, &mut structs)?;
                }
            }
        }
    }

    for op in &inlineMain {
        match op {
            Operation::Statement(s) => {
                genStatement(s.clone(), &mut out, &functionReturns, &mainLocals, None)?;
            }
            Operation::Expr(e) => {
                genExpression(e.clone(), &mut out, &functionReturns, &mainLocals)?;
            }
            _ => {}
        }
    }

    Ok(out)
}

pub fn buildLocalsTable(statement: &Statement, mainLocals: &mut HashMap<MyStr, (DataType, usize)>, localTypes: &mut Vec<VariableMetadata>, functionReturns: &HashMap<MyStr, Option<DataType>>) -> Result<(), Box<dyn Error>> {
    match statement {
        VariableCreate(c) => {
            let res = c.init.clone().ok_or("variable expected initializer")?;
            let t = res.toDataType(mainLocals, functionReturns)?;
            // println!("creating variable {} type {:?}", &c.name, &t);
            mainLocals.insert(MyStr::Runtime(c.name.clone().into_boxed_str()), (t.clone().unwrap(), localTypes.len()));
            localTypes.push(VariableMetadata { name: MyStr::Runtime(c.name.clone().into_boxed_str()), typ: t.unwrap() });
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

pub fn bytecodeGen2(operations: Vec<Operation>, functionReturns: &mut HashMap<MyStr, Option<DataType>>) -> Result<String, Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut mainLocals = HashMap::new();
    let mut out = String::new();
    let mut localTypes = vec![];

    for op in &operations {
        if let Operation::Statement(stat) = op {
            buildLocalsTable(stat, &mut mainLocals, &mut localTypes, functionReturns)?;
            inlineMain.push(op);
        } else if let Operation::Expr(Expression::FunctionCall(call)) = op {
            buildLocalsTable(&Statement::FunctionExpr(call.clone()), &mut mainLocals, &mut localTypes, functionReturns)?;
            inlineMain.push(op);
        }
    }

    println!("a");

    for op in &operations {
        if let Operation::Global(f) = op {
            match f {
                Node::FunctionDef(v) => {
                    genFunctionDef(v.clone(), &mut out, functionReturns)?;
                }
                Node::StructDef(v) => {
                    panic!();
                    // genStructDef(v.clone(), &mut out, functionReturns, &mut structs)?;
                }
            }
        }
    }

    println!("b");

    for op in &inlineMain {
        match op {
            Operation::Statement(s) => {
                genStatement(s.clone(), &mut out, functionReturns, &mainLocals, None)?;
            }
            Operation::Expr(e) => {
                genExpression(e.clone(), &mut out, functionReturns, &mainLocals)?;
            }
            _ => {}
        }
    }

    Ok(out)
}