use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;

use Statement::Variable;

use crate::ast::{Expression, FunctionDef, ModType, Node, Op, Statement, StructDef};


use crate::parser::*;
use crate::vm::{
    DataType, genFunName, genFunNameMeta, MyStr, OpCode,
    VariableMetadata,
};
use crate::vm::DataType::{Bool, Int};
use crate::vm::OpCode::*;

#[derive(Debug)]
pub struct NoValue {
    pub msg: String,
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
            let dataType = left.toDataType(vTable, functionReturns, None)?;
            match dataType {
                None => {
                    return Err(Box::new(NoValue {
                        msg: "expression must have return value".to_string(),
                    }));
                }
                Some(dat) => {
                    if let DataType::Object(v) = dat {
                        if v.name.as_str() != "String" {
                            panic!()
                        }

                        out.push_str("vm->nativeWrapper.strConcat(vm,frame,");
                        genExpression(*left, out, functionReturns, vTable)?;
                        out.push(',');
                        genExpression(*right, out, functionReturns, vTable)?;
                        out.push(')');

                        return Ok(());
                    }

                    genExpression(*left, out, functionReturns, vTable)?;
                    let t = match op {
                        Op::Add => "+",
                        Op::Sub => "-",
                        Op::Mul => "*",
                        Op::Div => "/",
                        Op::Gt => "<",
                        Op::Less => ">",
                        Op::Eq => "==",
                        Op::And => "&&",
                        Op::Or => panic!(),
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
            out.push_str("vm->nativeWrapper.stringNew(vm,frame,\"");
            out.push_str(&i);
            out.push_str("\")");
        }
        Expression::BoolLiteral(i) => {
            if i {
                out.push_str("true")
            } else {
                out.push_str("false")
            }
        }
        Expression::FunctionCall(e) => {
            let fName: MyStr = genFunName(
                e.name.as_str(),
                &e.arguments
                    .iter()
                    .map(|it| {
                        it.toDataType(vTable, functionReturns, None)
                            .unwrap()
                            .unwrap()
                    })
                    .collect::<Vec<DataType>>(),
            )
                .into_boxed_str()
                .into();


            let ret = functionReturns.get(&fName).unwrap().clone();

            out.push_str("({");

            for arg in e.arguments {
                let t = match arg.toDataType(vTable, functionReturns, None)?.unwrap() {
                    Int => "vm->nativeWrapper.pushInt(vm,",
                    DataType::Float => "vm->nativeWrapper.pushFloat(vm,",
                    Bool => "vm->nativeWrapper.pushBool(vm,",
                    DataType::Char => "vm->nativeWrapper.pushChar(vm,",
                    DataType::Object(_) => "vm->nativeWrapper.pushRef(vm,",
                    _ => panic!()
                };
                out.push_str(t);
                genExpression(arg, out, functionReturns, vTable)?;
                out.push_str(");");
            }
            out.push_str("vm->nativeWrapper.call(vm,\"");
            out.push_str(fName.as_str());
            out.push_str("\");");
            if let Some(v) = ret {
                let s = match v {
                    Int => "vm->nativeWrapper.popInt(vm);",
                    DataType::Float => "vm->nativeWrapper.popFloat(vm);",
                    Bool => "vm->nativeWrapper.popBool(vm);",
                    DataType::Char => "vm->nativeWrapper.popChar(vm);",
                    DataType::Object(_) => "vm->nativeWrapper.popRef(vm,frame);",
                    DataType::Void => panic!(),
                    DataType::Function { .. } => {}
                };
                out.push_str(s)
            }

            out.push_str("})");
        }
        Expression::Variable(v) => out.push_str(&v),
        Expression::CharLiteral(c) => {
            //panic!();
            out.push('\'');
            out.push(c);
            out.push('\'');
        }
        Expression::ArrayLiteral(_i) => {
            panic!();
            /*
            out.push('{');

            let _d = i
                .get(0)
                .ok_or("array must have at least one element")?
                .toDataType(vTable, functionReturns, None)?
                .ok_or("array elements must have type")?;
            for (ind, exp) in i.iter().enumerate() {
                genExpression(exp.clone(), out, functionReturns, vTable)?;

                if ind != i.len() - 1 {
                    out.push(',')
                }
            }

            out.push('}');

             */
        }
        Expression::ArrayIndexing(i) => {
            let t = i.expr.toDataType(vTable, functionReturns, None)?.unwrap();
            match t {
                DataType::Object(o) => match o.name.as_str() {
                    "String" => {
                        out.push_str("vm->nativeWrapper.stringGetChar(vm,");
                        genExpression(i.expr, out, functionReturns, vTable)?;
                        out.push(',');
                        genExpression(i.index, out, functionReturns, vTable)?;
                        out.push(')')
                    }
                    "Array" => {
                        let c = o.generics.first().unwrap();
                        let t = c.clone().ok_or("expected generic type not any")?;
                        let s = match t {
                            Int => "vm->nativeWrapper.arrGetInt(vm,",
                            DataType::Float => "vm->nativeWrapper.arrGetFloat(vm,",
                            Bool => "vm->nativeWrapper.arrGetBool(vm,",
                            DataType::Char => "vm->nativeWrapper.stringGetChar(vm,",
                            DataType::Object(_) => "vm->nativeWrapper.arrGetRef(vm,frame,",
                            DataType::Void => panic!()
                        };
                        out.push_str(s);
                        genExpression(i.expr, out, functionReturns, vTable)?;
                        out.push(',');
                        genExpression(i.index, out, functionReturns, vTable)?;
                        out.push(')')
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
        Expression::NotExpression(e) => {
            out.push('!');
            genExpression(*e, out, functionReturns, vTable)?;
        }
        Expression::NamespaceAccess(_, i) => todo!()
    }
    Ok(())
}

#[derive(Debug)]
pub struct VariableNotFound {
    pub name: String,
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
    _loopContext: Option<usize>,
) -> Result<(), Box<dyn Error>> {
    match statement {
        Statement::FunctionExpr(e) => {
            let fName: MyStr = genFunName(
                e.name.as_str(),
                &e.arguments
                    .iter()
                    .map(|it| {
                        it.toDataType(vTable, functionReturns, None)
                            .unwrap()
                            .unwrap()
                    })
                    .collect::<Vec<DataType>>(),
            )
                .into_boxed_str()
                .into();
            let ret = functionReturns.get(&fName).unwrap().clone();

            for arg in e.arguments {
                let t = match arg.toDataType(vTable, functionReturns, None)?.unwrap() {
                    Int => "vm->nativeWrapper.pushInt(vm,",
                    DataType::Float => "vm->nativeWrapper.pushFloat(vm,",
                    Bool => "vm->nativeWrapper.pushBool(vm,",
                    DataType::Char => "vm->nativeWrapper.pushChar(vm,",
                    DataType::Object(_) => "vm->nativeWrapper.pushRef(vm,",
                    _ => panic!()
                };
                out.push_str(t);
                genExpression(arg, out, functionReturns, vTable)?;
                out.push_str(");");
            }
            out.push_str("vm->nativeWrapper.call(vm,\"");
            out.push_str(fName.as_str());
            out.push_str("\");");
            if let Some(v) = ret {
                let s = match v {
                    Int => "vm->nativeWrapper.popInt(vm);",
                    DataType::Float => "vm->nativeWrapper.popFloat(vm);",
                    Bool => "vm->nativeWrapper.popBool(vm);",
                    DataType::Char => "vm->nativeWrapper.popChar(vm);",
                    DataType::Object(_) => "vm->nativeWrapper.popRef(vm,frame);",
                    DataType::Void => panic!()
                };
                out.push_str(s)
            }

            /*
            out.push_str(e.name.as_str());
            out.push_str("(");

            let argsLen = e.arguments.len();

            for (i, arg) in e.arguments.into_iter().enumerate() {
                let t = arg.toDataType(vTable, functionReturns, None)?;
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

             */
        }
        Variable(v) => match v.init {
            None => panic!(),
            Some(e) => {
                // println!("{:?}", e);
                let t = &e.toDataType(vTable, functionReturns, None)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue {
                            msg: String::from("idk"),
                        }));
                    }
                    Some(_ve) => {
                        out.push_str(&v.name);
                        out.push('=');
                        genExpression(e, out, functionReturns, vTable)?;
                        out.push(';');
                    }
                }
            }
        },
        Statement::While(w) => {
            out.push_str("while(");
            genExpression(w.exp, out, functionReturns, vTable)?;
            out.push_str("){ ");

            for s in w.body {
                genStatement(s, out, functionReturns, vTable, None)?;
            }
            out.push('}');
        }
        Statement::If(flow) => {
            out.push_str("if(");
            genExpression(flow.condition, out, functionReturns, vTable)?;
            out.push_str("){");

            for s in flow.body {
                genStatement(s, out, functionReturns, vTable, None)?;
            }

            out.push('}');

            match flow.elseBody {
                None => {}
                Some(v) => {
                    out.push_str("else{");

                    for s in v {
                        genStatement(s, out, functionReturns, vTable, None)?;
                    }

                    out.push('}');
                }
            }
        }
        Statement::Return(ret) => {
            let d = ret.exp.toDataType(vTable, functionReturns, None)?;
            match d {
                None => {}
                Some(v) => {
                    let v = match v {
                        Int => "vm->nativeWrapper.pushInt(vm,",
                        DataType::Float => "vm->nativeWrapper.pushFloat(vm,",
                        Bool => "vm->nativeWrapper.pushBool(vm,",
                        DataType::Char => "vm->nativeWrapper.pushChar(vm,",
                        DataType::Object(_) => "vm->nativeWrapper.pushRef(vm,",
                        DataType::Void => panic!()
                    };
                    out.push_str(v);
                    genExpression(ret.exp, out, functionReturns, vTable)?;
                    out.push_str(");")
                }
            }
            out.push_str("return;");
            /*
            out.push_str("return ");
            genExpression(ret.exp, out, functionReturns, vTable)?;
            out.push_str(";");

             */
        }
        Statement::VariableMod(m) => {
            let s = match m.modType {
                ModType::Add => "+=",
                ModType::Sub => "-=",
                ModType::Div => "/=",
                ModType::Mul => "*=",
            };
            out.push_str(&m.varName);
            out.push_str(s);
            genExpression(m.expr, out, functionReturns, vTable)?;
            out.push(';');
        }
        Statement::ArrayAssign { left: _, right: _ } => {
            panic!();
            /*
            genExpression(left.expr, out, functionReturns, vTable)?;
            out.push('[');
            genExpression(left.index, out, functionReturns, vTable)?;
            out.push_str("] = ");
            genExpression(right, out, functionReturns, vTable)?;
            out.push(';');

             */
        }
        Statement::Continue => out.push_str("continue;"),
        Statement::Break => out.push_str("break;"),
        Statement::Loop(body) => {
            out.push_str("while (1) { ");
            for s in body {
                genStatement(s, out, functionReturns, vTable, None)?;
            }
            out.push_str(" }");
        }
        Statement::NamespaceFunction(_, _) => todo!()
    }
    Ok(())
}

pub fn genFunctionDef(
    fun: FunctionDef,
    out: &mut String,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    match fun.returnType {
        None => {
            out.push_str("void");
        }
        Some(_v) => {
            out.push_str("void");
            // out.push_str(&v.toString());
        }
    };
    out.push(' ');
    out.push_str("call"); //out.push_str(&fun.name);
    out.push_str("(VirtualMachine* vm, StackFrame* frame){");

    let mut e = HashMap::new();

    let mut idk2 = vec![];
    for (it, arg) in fun.localsMeta.iter().enumerate() {
        out.push_str(arg.typ.toCString());
        out.push(' ');
        out.push_str(arg.name.as_str());
        out.push('=');
        let ee = match arg.typ {
            Int => "vm->nativeWrapper.getLocalsInt(frame,",
            DataType::Float => "vm->nativeWrapper.getLocalsFloat(frame,",
            Bool => "vm->nativeWrapper.getLocalsBool(frame,",
            DataType::Char => "vm->nativeWrapper.getLocalsChar(frame,",
            DataType::Object(_) => "vm->nativeWrapper.getLocalsRef(frame,",
            DataType::Void => panic!()
        };
        out.push_str(ee);

        out.push_str(&it.to_string());

        out.push_str(");");
        e.insert(arg.name.clone(), (arg.typ.clone(), 0usize));
    }

    for s in &fun.body {
        buildLocalsTable(s, &mut e, &mut idk2, functionReturns)?;
    }

    for local in &idk2 {
        out.push_str(local.typ.toCString());
        out.push(' ');
        out.push_str(local.name.as_str());
        out.push(';');
    }

    for a in fun.body {
        genStatement(a, out, functionReturns, &e, None)?;
    }

    out.push('}');

    Ok(())
}

pub fn genStructDef(
    struc: StructDef,
    ops: &mut Vec<OpCode>,
    _functionReturns: &HashMap<MyStr, Option<DataType>>,
    _structs: &HashMap<MyStr, HashMap<String, DataType>>,
) -> Result<(), Box<dyn Error>> {
    ops.push(ClassBegin);
    ops.push(ClassName {
        name: MyStr::Runtime(struc.name.into_boxed_str()),
    });

    for (n, typ) in struc.fields {
        ops.push(ClassField {
            name: n.into(),
            typ,
        })
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
                        MyStr::Runtime(
                            genFunNameMeta(&v.name, &v.localsMeta.clone(), v.argsCount).into_boxed_str(),
                        ),
                        v.returnType.clone(),
                    );
                }
                Node::StructDef(v) => {
                    structs.insert(
                        MyStr::Runtime(v.name.clone().into_boxed_str()),
                        v.fields.clone(),
                    );
                }
                Node::Import(_) => todo!()
            },
            Operation::Statement(v) => {
                if let Variable(c) = v {
                    match c.init {
                        None => {
                            return Err(Box::new(NoValue {
                                msg: "ahhh".to_string(),
                            }));
                        }
                        Some(ref ex) => {
                            let t = ex.clone().toDataType(&mainLocals, &functionReturns, None)?;
                            mainLocals.insert(
                                MyStr::Runtime(c.name.clone().into_boxed_str()),
                                (t.clone().unwrap(), counter),
                            );
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
                Node::StructDef(_v) => {
                    panic!()
                    //genStructDef(v.clone(), &mut out, &functionReturns, &mut structs)?;
                }
                Node::Import(_) => todo!()
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

pub fn buildLocalsTable(
    statement: &Statement,
    mainLocals: &mut HashMap<MyStr, (DataType, usize)>,
    localTypes: &mut Vec<VariableMetadata>,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    match statement {
        Variable(c) => {
            let res = c.init.clone().ok_or("variable expected initializer")?;
            let t = res.toDataType(mainLocals, functionReturns, None)?;
            // println!("creating variable {} type {:?}", &c.name, &t);
            mainLocals.insert(
                MyStr::Runtime(c.name.clone().into_boxed_str()),
                (t.clone().unwrap(), localTypes.len()),
            );
            localTypes.push(VariableMetadata {
                name: MyStr::Runtime(c.name.clone().into_boxed_str()),
                typ: t.unwrap(),
            });
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
        Statement::NamespaceFunction(_, _) => {}
    }

    Ok(())
}

pub fn statementFi(operations: Vec<Operation>) -> Vec<Operation> {
    let mut buf = vec![];
    for op in operations {
        match op {
            Operation::Expr(e) => match e {
                Expression::FunctionCall(c) => {
                    buf.push(Operation::Statement(Statement::FunctionExpr(c)))
                }
                _ => {}
            },
            a => buf.push(a),
        }
    }
    buf
}

pub fn bytecodeGen2(
    operations: Vec<Operation>,
    functionReturns: &mut HashMap<MyStr, Option<DataType>>,
) -> Result<String, Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut mainLocals = HashMap::new();
    let mut out = String::new();
    let mut localTypes = vec![];

    for op in &operations {
        if let Operation::Statement(stat) = op {
            buildLocalsTable(stat, &mut mainLocals, &mut localTypes, functionReturns)?;
            inlineMain.push(op);
        } else if let Operation::Expr(Expression::FunctionCall(call)) = op {
            buildLocalsTable(
                &Statement::FunctionExpr(call.clone()),
                &mut mainLocals,
                &mut localTypes,
                functionReturns,
            )?;
            inlineMain.push(op);
        } else if let Operation::Global(Node::FunctionDef(d)) = op {
            functionReturns.insert(
                genFunNameMeta(d.name.as_str(), &d.localsMeta, d.argsCount).into(),
                d.returnType.clone(),
            );
        }
    }

    for op in &operations {
        if let Operation::Global(f) = op {
            match f {
                Node::FunctionDef(v) => {
                    genFunctionDef(v.clone(), &mut out, functionReturns)?;
                }
                Node::StructDef(_v) => {
                    panic!();
                    // genStructDef(v.clone(), &mut out, functionReturns, &mut structs)?;
                }
                Node::Import(_) => todo!()
            }
        }
    }

    out.push_str("void call(VirtualMachine* vm,StackFrame* frame){");

    for local in &mainLocals {
        out.push_str(local.1.0.toCString());
        out.push(' ');
        out.push_str(local.0.as_str());
        out.push(';');
    }

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

    out.push('}');

    Ok(out)
}

fn genFunctionDef(
    fun: FunctionDef,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    if fun.isNative {
        let c = fun.argsCount;
        let mut buf = String::new();
        crate::cGen::genFunctionDef(fun.clone(), &mut buf, functionReturns)?;
        let resPath = crate::gccWrapper::compile(&buf)?;

        ops.push(OpCode::StrNew(MyStr::Runtime(resPath.into_boxed_str())));
        ops.push(StrNew(MyStr::Runtime(
            genFunNameMeta(&fun.name, &fun.localsMeta, c).into_boxed_str(),
        )));
        ops.push(OpCode::PushInt(fun.argsCount as isize));
        ops.push(OpCode::Call {
            encoded: MyStr::Runtime(Box::from("loadNative(String, String, int)")),
        });

        return Ok(());
    }

    ops.push(OpCode::FunBegin);
    ops.push(FunName {
        name: MyStr::Runtime(fun.name.clone().into_boxed_str()),
    });

    let mut idk1 = HashMap::new();
    let mut idk2 = vec![];

    for arg in fun.localsMeta {
        idk1.insert(arg.name.clone(), (arg.typ.clone(), idk2.len()));
        idk2.push(arg)
    }

    for s in &fun.body {
        buildLocalsTable(s, &mut idk1, &mut idk2, functionReturns)?;
    }

    // let vTable = constructVarTable(&fun, functionReturns)?;
    ops.push(LocalVarTable {
        typ: idk2.into_boxed_slice(),
        argsCount: fun.argsCount,
    });
    ops.push(FunReturn {
        typ: fun.returnType,
    });

    for statement in fun.body {
        let ctx = StatementCtx {
            statement: &statement,
            ops,
            functionReturns,
            vTable: &idk1,
            loopContext: None,
            clearStack: true,
        };
        genStatement(ctx)?;
    }
    ops.push(OpCode::Return);
    ops.push(OpCode::FunEnd);
    Ok(())
}

/*
function = black box that takes locals context and optionally returns value


call("calculate()")
** function lookup
** native call
void callable(void* vm, void* locals) {
    int x;
    x = getLocals(locals, 0);
    while (x < 10000000) {
        pushStackInt(20);
        internalCall(vm, "fact");
        popStackInt(vm);
        x+=1;
    }
}
**

 */
