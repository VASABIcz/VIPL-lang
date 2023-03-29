use std::collections::HashMap;
use std::env::args;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;

use Statement::Variable;

use crate::ast::{Expression, FunctionDef, ModType, Node, Op, Statement, StructDef};
use crate::lexer::*;
use crate::optimizer::{evalE, evalExpr};
use crate::parser::*;
use crate::vm::{
    DataType, evaluateBytecode, Generic, genFunName, genFunNameMeta, JmpType, MyStr, OpCode, Value,
    VariableMetadata,
};
use crate::vm::DataType::{Bool, Int};
use crate::vm::OpCode::*;

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

pub struct ExpressionCtx<'a> {
    pub exp: &'a Expression,
    pub ops: &'a mut Vec<OpCode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub typeHint: Option<DataType>,
}

pub struct PartialExprCtx<'a> {
    pub ops: &'a mut Vec<OpCode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub typeHint: Option<DataType>,
}

impl PartialExprCtx<'_> {
    pub fn genPushInt(&mut self, value: isize) {
        if value == 1 {
            self.ops.push(PushIntOne())
        }
        else if value == 0 {
            self.ops.push(PushIntZero())
        }
        else {
            self.ops.push(PushInt(value))
        }
    }
}

impl PartialExprCtx<'_> {
    pub fn constructCtx<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: None,
        }
    }
}

impl ExpressionCtx<'_> {
    pub fn reduce<'a>(&'a mut self) -> (&Expression, PartialExprCtx<'a>) {
        let p = PartialExprCtx {
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: self.typeHint.clone(),
        };
        let e = self.exp;

        (e, p)
    }
}

pub struct StatementCtx<'a> {
    pub statement: &'a Statement,
    pub ops: &'a mut Vec<OpCode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub loopContext: Option<usize>,
    pub clearStack: bool,
}

impl ExpressionCtx<'_> {
    pub fn copy<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: None,
        }
    }
}

impl StatementCtx<'_> {
    pub fn makeExpressionCtx<'a>(
        &'a mut self,
        exp: &'a Expression,
        typeHint: Option<DataType>,
    ) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint,
        }
    }

    pub fn copy<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            loopContext: self.loopContext,
            clearStack: self.clearStack,
        }
    }
}

fn genExpression(mut ctx: ExpressionCtx) -> Result<(), Box<dyn Error>> {
    let (e, mut r) = ctx.reduce();
    let mut d = evalE(&e);
    let e = match &mut d {
        None => &*e,
        Some(v) => v,
    };

    match e {
        Expression::ArithmeticOp { left, right, op } => {
            let dataType = left.toDataType(r.vTable, r.functionReturns, None)?;
            match dataType {
                None => {
                    return Err(Box::new(NoValue {
                        msg: "expression must have return value".to_string(),
                    }));
                }
                Some(dat) => {
                    genExpression(r.constructCtx(left))?;
                    genExpression(r.constructCtx(right))?;
                    let t = match op {
                        Op::Add => OpCode::Add(dat),
                        Op::Sub => OpCode::Sub(dat),
                        Op::Mul => OpCode::Mul(dat),
                        Op::Div => OpCode::Div(dat),
                        Op::Gt => OpCode::Greater(dat),
                        Op::Less => OpCode::Less(dat),
                        Op::Eq => OpCode::Equals(dat),
                        Op::And => OpCode::And,
                        Op::Or => OpCode::Or,
                    };
                    ctx.ops.push(t);
                }
            }
        }
        Expression::IntLiteral(i) => r.genPushInt(i.parse::<isize>().unwrap()),
        Expression::LongLiteral(i) => r.genPushInt(i.parse::<isize>().unwrap()),
        Expression::FloatLiteral(i) => r.ops.push(OpCode::PushFloat(i.parse::<f64>().unwrap())),
        Expression::DoubleLiteral(i) => r.ops.push(OpCode::PushFloat(i.parse::<f64>().unwrap())),
        Expression::StringLiteral(i) => {
            r.ops
                .push(StrNew(MyStr::Runtime(i.clone().into_boxed_str())));
        }
        Expression::BoolLiteral(i) => r.ops.push(OpCode::PushBool(*i)),
        Expression::FunctionCall(e) => {
            let mut argTypes = vec![];

            for arg in &e.arguments {
                let t = arg.toDataType(r.vTable, r.functionReturns, None)?;
                match t {
                    None => {
                        return Err(Box::new(NoValue {
                            msg: String::from("aahhh"),
                        }));
                    }
                    Some(v) => {
                        argTypes.push(v);
                        genExpression(r.constructCtx(&arg))?;
                    }
                }
            }

            r.ops.push(Call {
                encoded: MyStr::Runtime(genFunName(e.name.as_str(), &argTypes).into_boxed_str()),
            })
        }
        Expression::Variable(v) => {
            let _res = match r.vTable.get(&MyStr::Runtime(v.clone().into_boxed_str())) {
                None => {
                    return Err(Box::new(VariableNotFound { name: v.clone() }));
                }
                Some(v) => v,
            };
            r.ops.push(OpCode::PushLocal {
                index: r
                    .vTable
                    .get(&MyStr::Runtime(v.clone().into_boxed_str()))
                    .unwrap()
                    .1,
            })
        }
        Expression::CharLiteral(c) => r.ops.push(PushChar(*c)),
        Expression::ArrayLiteral(i) => {
            let d = match r.typeHint {
                None => Some(
                    i.get(0)
                        .ok_or("array must have at least one element")?
                        .toDataType(r.vTable, r.functionReturns, None)?
                        .ok_or("array elements must have type")?,
                ),
                Some(ref v) => match v {
                    DataType::Object(v) => match v.generics.first() {
                        None => None,
                        Some(v) => match v {
                            Generic::Any => None,
                            Generic::Type(v) => Some(v.clone()),
                        },
                    },
                    _ => None,
                },
            };
            let e = d.ok_or("")?;
            // let d = i.get(0).ok_or("array must have at least one element")?.toDataType(vTable, functionReturns, None)?.ok_or("array elements must have type")?;
            r.genPushInt(i.len() as isize);
            r.ops.push(ArrayNew(e.clone()));
            for (ind, exp) in i.iter().enumerate() {
                r.ops.push(Dup);
                genExpression(r.constructCtx(exp))?;
                r.genPushInt(ind as isize);
                r.ops.push(ArrayStore(e.clone()));
            }
        }
        Expression::ArrayIndexing(i) => {
            // println!("{:?}", i.expr);
            let d = i
                .expr
                .toDataType(r.vTable, r.functionReturns, None)?
                .ok_or("ewgergreg")?;
            match d {
                DataType::Object(o) => {
                    // println!("{:?}", o);
                    if o.name.as_str() == "String" {
                        genExpression(r.constructCtx(&i.expr))?;
                        genExpression(r.constructCtx(&i.index))?;
                        r.ops.push(GetChar);
                        return Ok(());
                    }

                    match o.generics.first().unwrap() {
                        Generic::Type(v) => {
                            genExpression(r.constructCtx(&i.expr))?;
                            genExpression(r.constructCtx(&i.index))?;

                            ctx.ops.push(OpCode::ArrayLoad(v.clone()));
                        }
                        Generic::Any => panic!(),
                    }
                }
                v => panic!("{v:?}"),
            }
        }
        Expression::NotExpression(e) => {
            genExpression(r.constructCtx(e))?;
            ctx.ops.push(Not)
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

fn genStatement(mut ctx: StatementCtx) -> Result<(), Box<dyn Error>> {
    match ctx.statement {
        Statement::FunctionExpr(ref e) => {
            let mut argTypes = vec![];

            for arg in &e.arguments {
                let t = arg
                    .toDataType(ctx.vTable, ctx.functionReturns, None)
                    .unwrap();
                match t {
                    None => {
                        return Err(Box::new(NoValue {
                            msg: "e".to_string(),
                        }))
                    }
                    Some(v) => {
                        argTypes.push(v);
                        genExpression(ctx.makeExpressionCtx(&arg, None))?;
                    }
                }
            }

            let funName = genFunName(e.name.as_str(), &argTypes).into_boxed_str();
            let s = MyStr::from(funName);

            let mut shouldPop = false;

            if ctx.clearStack {
                match ctx.functionReturns.get(&s) {
                    None => {}
                    Some(v) => match v {
                        None => {}
                        Some(_) => {
                            shouldPop = true;
                        }
                    },
                }
            }

            ctx.ops.push(Call { encoded: s });
            if shouldPop {
                ctx.ops.push(Pop)
            }
        }
        Variable(v) => match &v.init {
            None => {}
            Some(e) => {
                let t = &e.toDataType(ctx.vTable, ctx.functionReturns, v.typeHint.clone())?;
                match t {
                    None => {
                        return Err(Box::new(NoValue {
                            msg: String::from("idk"),
                        }));
                    }
                    Some(ve) => {
                        genExpression(ctx.makeExpressionCtx(&e, Some(ve.clone())))?;
                        // println!("{}", &v.name);
                        ctx.ops.push(OpCode::SetLocal {
                            index: ctx
                                .vTable
                                .get(&MyStr::Runtime(v.name.clone().into_boxed_str()))
                                .unwrap()
                                .1,
                            typ: ve.clone(),
                        });
                    }
                }
            }
        },
        Statement::While(w) => {
            let ret = w.exp.toDataType(ctx.vTable, ctx.functionReturns, None)?;
            match ret {
                None => {
                    return Err(Box::new(NoValue {
                        msg: format!("expression {:?} must return bool", w.exp),
                    }));
                }
                Some(ve) => {
                    if ve != Bool {
                        return Err(Box::new(NoValue {
                            msg: format!("expected bool got {:?} {:?}", ve, w.exp),
                        }));
                    }
                    let size = ctx.ops.len();
                    genExpression(ctx.makeExpressionCtx(&w.exp, None))?;
                    let mut bodyBuf = vec![];
                    for s in &w.body {
                        let mut ctx2 = ctx.copy(&s);
                        ctx2.ops = &mut bodyBuf;
                        println!("{}", size);
                        ctx2.loopContext = Some(size);
                        genStatement(ctx2)?;
                    }
                    let len = bodyBuf.len();
                    ctx.ops.push(OpCode::Jmp {
                        offset: len as isize + 1,
                        jmpType: JmpType::False,
                    });
                    ctx.ops.extend(bodyBuf);
                    ctx.ops.push(OpCode::Jmp {
                        offset: -(ctx.ops.len() as isize - size as isize + 1),
                        jmpType: JmpType::Jmp,
                    })
                }
            }
        }
        Statement::If(flow) => {
            let mut buf = vec![];
            for s in &flow.body {
                let mut cop = ctx.copy(&s);
                cop.ops = &mut buf;
                genStatement(cop)?;
            }

            genExpression(ctx.makeExpressionCtx(&flow.condition, None))?;
            let mut jumpDist = buf.len() as isize;
            if flow.elseBody.is_some() {
                jumpDist += 1;
            }
            ctx.ops.push(Jmp {
                offset: jumpDist,
                jmpType: JmpType::False,
            });
            ctx.ops.extend(buf);

            match &flow.elseBody {
                None => {}
                Some(els) => {
                    buf = vec![];
                    for s in els {
                        let mut ctx1 = ctx.copy(&s);
                        ctx1.ops = &mut buf;
                        genStatement(ctx1)?;
                    }

                    ctx.ops.push(OpCode::Jmp {
                        offset: buf.len() as isize,
                        jmpType: JmpType::Jmp,
                    });
                    ctx.ops.extend(buf);
                }
            }
        }
        Statement::Return(ret) => {
            genExpression(ctx.makeExpressionCtx(&ret.exp, None))?;
            ctx.ops.push(OpCode::Return)
        }
        Statement::VariableMod(m) => {
            match ctx.vTable.get(&MyStr::Runtime(m.clone().varName.into_boxed_str())) {
                None => {
                    return Err(Box::new(VariableNotFound { name: m.varName.clone() }));
                }
                Some(local) => {
                    let dataType = m.expr.toDataType(ctx.vTable, ctx.functionReturns, None)?.expect("expected return value");
                    ctx.ops.push(PushLocal { index: local.1 });
                    genExpression(ctx.makeExpressionCtx(&m.expr, None))?;
              /*      if *ctx.ops.last().unwrap() == PushIntOne() {
                        ctx.ops.pop();
                        ctx.ops.push(Inc { typ: dataType, index: local.1 })
                    }*/
                    // else {
                        let op = match m.modType {
                            ModType::Add => Add(dataType.clone()),
                            ModType::Sub => Sub(dataType.clone()),
                            ModType::Div => Div(dataType.clone()),
                            ModType::Mul => Mul(dataType.clone())
                        };
                        ctx.ops.push(op);
                        ctx.ops.push(SetLocal { index: local.1, typ: dataType })
                    // }
                }
            }
        }
        Statement::ArrayAssign { left, right } => {
            genExpression(ctx.makeExpressionCtx(&left.expr, None))?;
            let t = right
                .toDataType(ctx.vTable, ctx.functionReturns, None)?
                .ok_or("cant assign void to array")?;
            genExpression(ctx.makeExpressionCtx(&right, None))?;
            genExpression(ctx.makeExpressionCtx(&left.index, None))?;
            ctx.ops.push(ArrayStore(t))
        }
        Statement::Continue => {
            let index = ctx
                .loopContext
                .ok_or("continue can be only used in loops")?;
            ctx.ops.push(Jmp {
                offset: (index - ctx.ops.len() + 2) as isize,
                jmpType: JmpType::Jmp,
            })
        }
        Statement::Break => panic!(),
        Statement::Loop(body) => {
            let mut buf = vec![];
            let context = ctx.ops.len();
            for s in body {
                let mut cop = ctx.copy(s);
                cop.ops = &mut buf;
                cop.loopContext = Some(context);
                genStatement(cop)?;
            }
            let bufLen = buf.len() as isize;
            ctx.ops.extend(buf);
            ctx.ops.push(Jmp {
                offset: -(bufLen + 1),
                jmpType: JmpType::Jmp,
            });
        }
    }
    Ok(())
}

fn genFunctionDef(
    fun: FunctionDef,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
) -> Result<(), Box<dyn Error>> {
    if fun.isNative {
        let c = fun.argCount;
        let mut buf = String::new();
        crate::cGen::genFunctionDef(fun.clone(), &mut buf, functionReturns)?;
        let resPath = crate::gccWrapper::compile(&buf)?;

        ops.push(OpCode::StrNew(MyStr::Runtime(resPath.into_boxed_str())));
        ops.push(StrNew(MyStr::Runtime(
            genFunNameMeta(&fun.name, &fun.args, c).into_boxed_str(),
        )));
        ops.push(OpCode::PushInt(fun.argCount as isize));
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

    for arg in fun.args {
        idk1.insert(arg.name.clone(), (arg.typ.clone(), idk2.len()));
        idk2.push(arg)
    }

    for s in &fun.body {
        buildLocalsTable(s, &mut idk1, &mut idk2, functionReturns)?;
    }

    // let vTable = constructVarTable(&fun, functionReturns)?;
    ops.push(LocalVarTable {
        typ: idk2.into_boxed_slice(),
        argsCount: fun.argCount,
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
    }

    Ok(())
}

pub fn complexBytecodeGen(
    operations: Vec<Operation>,
    localTypes: &mut Vec<DataType>,
    functionReturns: &mut HashMap<MyStr, Option<DataType>>,
    mainLocals: &mut HashMap<MyStr, (DataType, usize)>,
    structs: &mut HashMap<MyStr, HashMap<String, DataType>>,
    clearStack: bool,
) -> Result<Vec<OpCode>, Box<dyn Error>> {
    let mut inlineMain = vec![];
    let mut ops = vec![];

    for op in &operations {
        match op {
            Operation::Global(f) => match f {
                Node::FunctionDef(v) => {
                    functionReturns.insert(
                        MyStr::Runtime(
                            genFunNameMeta(v.name.as_str(), &v.args, v.argCount).into_boxed_str(),
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
            },
            Operation::Statement(v) => {
                if let Variable(c) = v {
                    if !mainLocals.contains_key(&MyStr::from(c.name.clone())) {
                        match c.init {
                            None => {
                                return Err(Box::new(NoValue {
                                    msg: "ahhh".to_string(),
                                }));
                            }
                            Some(ref ex) => {
                                let t = ex.clone().toDataType(
                                    mainLocals,
                                    functionReturns,
                                    c.typeHint.clone(),
                                )?;
                                mainLocals.insert(
                                    c.name.clone().into_boxed_str().into(),
                                    (t.clone().unwrap(), mainLocals.len()),
                                );
                                localTypes.push(t.unwrap());
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
        if let Operation::Global(f) = op {
            match f {
                Node::FunctionDef(v) => {
                    genFunctionDef(v.clone(), &mut ops, functionReturns)?;
                }
                Node::StructDef(v) => {
                    genStructDef(v.clone(), &mut ops, functionReturns, structs)?;
                }
            }
        }
    }

    for op in &inlineMain {
        match op {
            Operation::Statement(s) => {
                let ctx = StatementCtx {
                    statement: s,
                    ops: &mut ops,
                    functionReturns,
                    vTable: mainLocals,
                    loopContext: None,
                    clearStack,
                };
                genStatement(ctx)?;
            }
            Operation::Expr(e) => {
                let ctx = ExpressionCtx {
                    exp: &e,
                    ops: &mut ops,
                    functionReturns,
                    vTable: &mainLocals,
                    typeHint: None,
                };
                genExpression(ctx)?;
            }
            _ => {}
        }
    }

    Ok(ops)
}

pub fn bytecodeGen(
    operations: Vec<Operation>,
) -> Result<(Vec<OpCode>, Vec<DataType>), Box<dyn Error>> {
    let mut mainLocals = HashMap::new();
    let mut functionReturns = HashMap::new();
    let mut localTypes = vec![];
    let mut structs = HashMap::new();

    let res = complexBytecodeGen(
        operations,
        &mut localTypes,
        &mut functionReturns,
        &mut mainLocals,
        &mut structs,
        true,
    )?;

    Ok((res, localTypes))
}

pub fn bytecodeGen2(
    operations: Vec<Operation>,
    functionReturns: &mut HashMap<MyStr, Option<DataType>>,
) -> Result<(Vec<OpCode>, Vec<DataType>), Box<dyn Error>> {
    let mut mainLocals = HashMap::new();
    let mut localTypes = vec![];
    let mut structs = HashMap::new();

    let res = complexBytecodeGen(
        operations,
        &mut localTypes,
        functionReturns,
        &mut mainLocals,
        &mut structs,
        true,
    )?;

    Ok((res, localTypes))
}
