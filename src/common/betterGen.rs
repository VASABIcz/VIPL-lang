use std::collections::HashMap;

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Deref;

use Statement::Variable;

use crate::ast::{Expression, FunctionDef, ModType, Node, Op, Statement, StructDef};
use crate::errors::{InvalidTypeException, NoValue, TypeNotFound, SymbolNotFound};
use crate::lexer::*;
use crate::namespace::{FunctionMeta, FunctionTypeMeta, Namespace};
use crate::optimizer::{evalE};
use crate::parser::*;
use crate::vm::{DataType, Generic, genFunName, genFunNameMeta, JmpType, MyStr, ObjectMeta, OpCode, VariableMetadata, VirtualMachine};
use crate::vm::DataType::{Bool, Char, Object};
use crate::vm::Generic::Any;
use crate::vm::OpCode::*;



#[derive(Debug)]
pub struct ExpressionCtx<'a> {
    pub exp: &'a Expression,
    pub ops: &'a mut Vec<OpCode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a Namespace,
    pub vm: &'a VirtualMachine<'a>
}

impl PartialExprCtx<'_> {
    pub fn lookupFunctionByBaseName(&self, name: &str) -> Option<String> {
        println!("funcs {:?}", self.functionReturns.keys());
        let idk = format!("{}::{}(", self.currentNamespace.name, name);
        println!("finding {}", idk);

        for (k, _) in self.functionReturns {
            if k.as_str().starts_with(&idk) {
                println!("found {}", k);
                return Some(k.to_string());
            }
        }
        return None;
    }
}

impl ExpressionCtx<'_> {
    pub fn lookupFunctionByBaseName(&self, name: &str) -> Option<String> {
        println!("funcs {:?}", self.functionReturns.keys());
        let idk = format!("{}::{}(", self.currentNamespace.name, name);
        println!("finding {}", idk);

        for (k, v) in self.functionReturns {
            if k.as_str().starts_with(&idk) {
                println!("found {}", k);
                return Some(k.to_string());
            }
        }
        return None;
    }

    pub fn transfer<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
        }
    }

    pub fn toDataType(&mut self) -> Result<Option<DataType>, Box<dyn Error>> {
        match self.exp {
            Expression::ArithmeticOp {
                left,
                right: _,
                op: o,
            } => {
                match o {
                    Op::Gt => return Ok(Some(Bool)),
                    Op::Less => return Ok(Some(Bool)),
                    Op::Eq => return Ok(Some(Bool)),
                    Op::And => return Ok(Some(Bool)),
                    Op::Or => return Ok(Some(Bool)),
                    _ => {}
                }
                let _leftType = self.transfer(left).toDataType().unwrap();

                Ok(_leftType)
            }
            Expression::IntLiteral(_) => Ok(Some(DataType::Int)),
            Expression::LongLiteral(_) => {
                // FIXME
                Ok(Some(DataType::Int))
            }
            Expression::FloatLiteral(_) => Ok(Some(DataType::Float)),
            Expression::DoubleLiteral(_) => {
                // FIXME
                Ok(Some(DataType::Float))
            }
            Expression::StringLiteral(_) => Ok(Some(DataType::str())),
            Expression::FunctionCall(f) => {
                let types = f
                    .arguments
                    .iter()
                    .filter_map(|x| self.transfer(x).toDataType().ok()?)
                    .collect::<Vec<DataType>>();

                let enc = genFunName(f.name.as_str(), &types);
                match self.functionReturns.get(&MyStr::Runtime(enc.clone().into_boxed_str())) {
                    None => {
                        match self.functionReturns.get(&format!("{}::{}", self.currentNamespace.name, enc).into()) {
                            None => {
                                panic!();
                                Err(Box::new(TypeNotFound { typ: enc }))
                            }
                            Some(v) => match v {
                                None => {
                                    panic!();
                                    Err(Box::new(TypeNotFound { typ: enc }))
                                }
                                Some(v) => Ok(Some(v.clone()))
                            }
                        }
                    }
                    Some(v) => Ok(v.clone()),
                }
            }
            Expression::Variable(name) => {
                match self.vTable.get(&MyStr::Runtime(name.clone().into_boxed_str())) {
                    None => {
                        println!("JEBE");
                        let funcName = self.lookupFunctionByBaseName(name).ok_or(Box::new(TypeNotFound {
                            typ: format!("variable {name} not found"),
                        }))?;
                        let f = self.currentNamespace.getFunctionByName(&funcName).unwrap();
                        return Ok(Some(f.0.toFunctionType()));

                        println!("hint {:?}", self.typeHint);
                        Err(Box::new(TypeNotFound {
                            typ: format!("variable {name} not found"),
                        }))
                    },
                    Some(v) => Ok(Some(v.0.clone())),
                }
            }
            Expression::BoolLiteral(_) => Ok(Some(DataType::Bool)),
            Expression::CharLiteral(_) => Ok(Some(Char)),
            Expression::ArrayLiteral(e) => {
                let l = &self.typeHint;
                let c = l.clone().ok_or("cannot infer type of empty array consider adding type hint")?;
                if e.is_empty() {
                    match c
                    {
                        Object(o) => {
                            if o.name.as_str() == "Array" {
                                let e = o
                                    .generics
                                    .first()
                                    .ok_or("array type must have genneric type")?;
                                Ok(Some(DataType::arr(e.clone())))
                            } else {
                                Err(Box::new(InvalidTypeException {
                                    expected: DataType::Object(ObjectMeta {
                                        name: MyStr::from("Array"),
                                        generics: Box::new([Any]),
                                    }),
                                    actual: Some(Object(o.clone())),
                                }))
                            }
                        }
                        v => Err(Box::new(InvalidTypeException {
                            expected: DataType::arr(Any),
                            actual: Some(v.clone()),
                        })),
                    }
                } else {
                    let t = self.transfer(e
                        .get(0)
                        .ok_or("array must have least one value")?).toDataType()?.ok_or("array item must have tyoe")?;
                    Ok(Some(DataType::arr(Generic::Type(t))))
                }
            }
            Expression::ArrayIndexing(i) => {
                let e = self.transfer(&i.expr).toDataType()?.ok_or("cannot array index none")?;
                match e {
                    Object(o) => {
                        if o.name.as_str() == "String" {
                            return Ok(Some(Char));
                        }
                        Ok(Some(
                            o.generics
                                .first()
                                .ok_or("array must have one generic parameter")?
                                .clone()
                                .ok_or("")?,
                        ))
                    }
                    _ => panic!(),
                }
            }
            Expression::NotExpression(i) => {
                let d = self.transfer(i).toDataType()?.ok_or("expected return type")?;

                match d {
                    DataType::Bool => Ok(Some(DataType::Bool)),
                    _ => {
                        panic!()
                    }
                }
            }
            Expression::NamespaceAccess(n) => todo!(),
            Expression::Lambda(_, _) => panic!(),
            Expression::Callable(prev, args) => {
                unsafe {
                    if let Expression::Variable(v) = &**(prev as *const Box<Expression>) {
                        if !self.vTable.contains_key(&MyStr::Static(&v)) {
                            println!("hello {:?} {:?} {:?}", prev, args, self.vTable);
                            let genName = genFunName(&v, &args.iter().map(|it| { self.constructCtx(it).toDataType().unwrap().unwrap() }).collect::<Vec<_>>());
                            let funcId = self.currentNamespace.getFunctionByName(&genName).ok_or(format!("could not find function with type {}", &genName))?;
                            return Ok(funcId.0.returnType.clone())
                        }
                        else {
                            let var = self.vTable.get(&MyStr::Static(&v)).unwrap();
                            match &var.0 {
                                DataType::Function { args, ret } => {
                                    return Ok(Some(*ret.clone()))
                                }
                                _ => panic!()
                            }
                        }
                    }
                    else if let Expression::NamespaceAccess(v) = &**(prev as *const Box<Expression>) {
                        let genName = genFunName(&v.join("::"), &args.iter().map(|it| { self.constructCtx(it).toDataType().unwrap().unwrap() }).collect::<Vec<_>>());
                        let funcId = self.currentNamespace.getFunctionByName(&genName).ok_or(format!("could not find function with type {}", &genName)).unwrap();
                        return Ok(funcId.0.returnType.clone())
                    }
                }
                todo!("{:?} {:?}", prev, args)
            }
            Expression::StructInit(name, _) => {
                Ok(Some(DataType::Object(ObjectMeta{ name: name.clone().into(), generics: Box::new([]) })))
            }
            Expression::FieldAccess(prev, fieldName) => {
                let e = self.transfer(prev).toDataType()?.ok_or("cannot array index none")?;
                match e {
                    Object(o) => {
                        let structID = self.currentNamespace.structLookup.get(o.name.as_str()).unwrap();
                        let structMeta = self.currentNamespace.structs.get(*structID).unwrap();
                        let fieldID = structMeta.fieldsLookup.get(fieldName).unwrap();
                        let t = structMeta.fields.get(*fieldID).unwrap();
                        Ok(Some(t.typ.clone()))
                    }
                    _ => panic!(),
                }
            }
        }
    }
}

pub struct PartialExprCtx<'a> {
    pub ops: &'a mut Vec<OpCode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a Namespace,
    pub vm: &'a VirtualMachine<'a>
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
            currentNamespace: self.currentNamespace,
            vm: self.vm,
        }
    }
}

impl ExpressionCtx<'_> {
    pub fn constructCtx<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
        }
    }
}

impl ExpressionCtx<'_> {
    pub fn reduce(&mut self) -> (&Expression, PartialExprCtx<'_>) {
        let p = PartialExprCtx {
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: self.typeHint.clone(),
            currentNamespace: self.currentNamespace,
            vm: self.vm,
        };
        let e = self.exp;

        (e, p)
    }
}

#[derive(Debug)]
pub struct StatementCtx<'a> {
    pub statement: &'a Statement,
    pub ops: &'a mut Vec<OpCode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a mut HashMap<MyStr, (DataType, usize)>,
    pub loopContext: Option<usize>,
    pub clearStack: bool,
    pub currentNamespace: &'a Namespace,
    pub vm: &'a VirtualMachine<'a>
}

impl StatementCtx<'_> {
    pub fn transfer<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            loopContext: self.loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            clearStack: false
        }
    }
}

impl ExpressionCtx<'_> {
    pub fn copy<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
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
            currentNamespace: self.currentNamespace,
            vm: self.vm,
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
            currentNamespace: self.currentNamespace,
            vm: self.vm,
        }
    }
}

pub fn buildLocalsTable(ctx: &mut StatementCtx, locals: &mut Vec<VariableMetadata>) -> Result<(), Box<dyn Error>> {
    match ctx.statement {
        Variable(c) => {
            let res = c.init.clone().ok_or("variable expected initializer")?;
            let t = ctx.makeExpressionCtx(&res, None).toDataType().unwrap_or(c.typeHint.clone());


            let k = MyStr::Runtime(c.name.clone().into_boxed_str());
            if ctx.vTable.contains_key(&k) {
               return Ok(());
            }

            ctx.vTable.insert(
                MyStr::Runtime(c.name.clone().into_boxed_str()),
                (t.clone().unwrap(), locals.len()),
            );
            locals.push(VariableMetadata {
                name: MyStr::Runtime(c.name.clone().into_boxed_str()),
                typ: t.unwrap(),
            });
        }
        Statement::While(w) => {
            for s in &w.body {
                buildLocalsTable(&mut ctx.transfer(s), locals)?;
            }
        }
        Statement::If(i) => {
            for s in &i.body {
                buildLocalsTable(&mut ctx.transfer(s), locals)?;
            }
            if let Some(body) = &i.elseBody {
                for s in body {
                    buildLocalsTable(&mut ctx.transfer(s), locals)?;
                }
            }
        }
        Statement::Loop(body) => {
            for s in body {
                buildLocalsTable(&mut ctx.transfer(s), locals)?;
            }
        }
        Statement::FunctionExpr(_) => {}
        Statement::VariableMod(_) => {}
        Statement::Return(_) => {}
        Statement::ArrayAssign { .. } => {}
        Statement::Continue => {}
        Statement::Break => {}
        Statement::NamespaceFunction(_, _) => {}
        Statement::StatementExpression(_) => {}
    }

    Ok(())
}

pub fn genFunctionDef(
    fun: &FunctionMeta,
    ops: &mut Vec<OpCode>,
    functionReturns: &HashMap<MyStr, Option<DataType>>,
    vm: &VirtualMachine,
    currentNamespace: &Namespace
) -> Result<Vec<VariableMetadata>, Box<dyn Error>> {
    let mut vTable = HashMap::new();
    let mut locals = vec![];

    for arg in fun.localsMeta.iter() {
        vTable.insert(arg.name.clone(), (arg.typ.clone(), locals.len()));
        locals.push(arg.clone())
    }

    if let FunctionTypeMeta::Runtime(body) = &fun.functionType {
        for s in body {
            let mut ctx = StatementCtx{
                statement: &Statement::Continue,
                ops,
                functionReturns,
                vTable: &mut vTable,
                loopContext: None,
                clearStack: false,
                currentNamespace,
                vm,
            };
            buildLocalsTable(&mut ctx.transfer(s), &mut locals)?;
        }

        for statement in body {
            println!("gening {:?}", statement);
            genStatement(StatementCtx{
                statement,
                ops,
                functionReturns,
                vTable: &mut vTable,
                loopContext: None,
                clearStack: false,
                currentNamespace,
                vm,
            })?;
        }
        ops.push(Return);

        Ok(locals)
    }
    else {
        panic!()
    }
}

pub fn genStatement(mut ctx: StatementCtx) -> Result<(), Box<dyn Error>> {
    match ctx.statement {
        Statement::FunctionExpr(ref e) => {
                let mut argTypes = vec![];

                for arg in &e.arguments {
                    let t = ctx.makeExpressionCtx(arg, None).toDataType()?;
                    match t {
                        None => { return Err(Box::new(NoValue { msg: String::from("aahhh"), })); }
                        Some(v) => {
                            argTypes.push(v);
                            genExpression(ctx.makeExpressionCtx(arg, None))?;
                        }
                    }
                }

                let n = genFunName(e.name.as_str(), &argTypes);
                println!("{:?} {}", ctx.currentNamespace.functionsLookup, n);
                let res = ctx.currentNamespace.functionsLookup.get(&n).unwrap();

            let mut shouldPop = false;

            if ctx.clearStack {
                match ctx.functionReturns.get(&n.into()) {
                    None => {}
                    Some(v) => match v {
                        None => {}
                        Some(_) => {
                            shouldPop = true;
                        }
                    },
                }
            }

            ctx.ops.push(SCall { id: *res });
            if shouldPop {
                ctx.ops.push(Pop)
            }
        }
        Variable(v) => match &v.init {
            None => {}
            Some(e) => {
                println!("aids {:?}", ctx);
                let t = ctx.makeExpressionCtx(e, v.typeHint.clone()).toDataType()?;
                match t {
                    None => {
                        return Err(Box::new(NoValue {
                            msg: String::from("idk"),
                        }));
                    }
                    Some(ve) => {
                        genExpression(ctx.makeExpressionCtx(e, Some(ve.clone())))?;
                        println!("{}", &v.name);
                        println!("vTable {:?}", ctx.vTable);
                        ctx.ops.push(OpCode::SetLocal {
                            index: ctx.vTable.get(&MyStr::Runtime(v.name.clone().into_boxed_str())).unwrap().1,
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
                        let mut ctx2 = ctx.copy(s);
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
                let mut cop = ctx.copy(s);
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
                        let mut ctx1 = ctx.copy(s);
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
                    return Err(Box::new(SymbolNotFound { name: m.varName.clone() }));
                }
                Some(local) => {
                    let dataType = m.expr.toDataType(ctx.vTable, ctx.functionReturns, None)?.expect("expected return value");
                    ctx.ops.push(PushLocal { index: local.1 });
                    let xd = local.1;
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
                    ctx.ops.push(SetLocal { index: xd, typ: dataType })
                    // }
                }
            }
        }
        Statement::ArrayAssign { left, right } => {
            genExpression(ctx.makeExpressionCtx(&left.expr, None))?;
            let t = right
                .toDataType(ctx.vTable, ctx.functionReturns, None)?
                .ok_or("cant assign void to array")?;
            genExpression(ctx.makeExpressionCtx(right, None))?;
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
        Statement::NamespaceFunction(path, f) => {
            println!("{:?}", ctx.functionReturns);
            let t = f.arguments.iter().map(|it| {
                ctx.makeExpressionCtx(it, None).toDataType().unwrap().unwrap()
            }).collect::<Vec<_>>();

            let r = path.join("::");
            let namespaceId = ctx.vm.namespaceLookup.get(&r).unwrap();
            let namespace = ctx.vm.namespaces.get(*namespaceId).unwrap();
            let n = genFunName(f.name.as_str(), &t);
            println!("{}", n);
            println!("{:?}", namespace.functionsLookup);
            let funcId = namespace.functionsLookup.get(&n).unwrap();

            for arg in &f.arguments {
                genExpression(ctx.makeExpressionCtx(arg, None))?;
            }

            ctx.ops.push(LCall { namespace: *namespaceId, id: *funcId })
        }
        Statement::StatementExpression(v) => {
            let eCtx = ctx.makeExpressionCtx(v, None);
            genExpression(eCtx)?;
        }
    }
    Ok(())
}

fn genExpression(mut ctx: ExpressionCtx) -> Result<(), Box<dyn Error>> {
    let (e, mut r) = ctx.reduce();
    let mut d = evalE(e);
    let e = match &mut d {
        None => e,
        Some(v) => v,
    };

    match e {
        Expression::ArithmeticOp { left, right, op } => {
            let dataType = r.constructCtx(left).toDataType()?;
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
                    r.ops.push(t);
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
                    None => { return Err(Box::new(NoValue { msg: String::from("aahhh"), })); }
                    Some(v) => {
                        argTypes.push(v);
                        genExpression(r.constructCtx(arg))?;
                    }
                }
            }

            let n = genFunName(e.name.as_str(), &argTypes);
            println!("{:?}", r.currentNamespace.functionsLookup);
            let res = r.currentNamespace.functionsLookup.get(&n).unwrap();
            r.ops.push(OpCode::SCall { id: *res })
        }
        Expression::Variable(v) => unsafe {
            if r.vTable.contains_key(&v.clone().into()) {
                r.ops.push(OpCode::PushLocal {
                    index: r
                        .vTable
                        .get(&MyStr::Runtime(v.clone().into_boxed_str()))
                        .ok_or(Box::new(SymbolNotFound { name: v.clone() }))?
                        .1,
                });
            }
            else {
                let f = r.lookupFunctionByBaseName(v).unwrap();
                let a = r.currentNamespace.getFunctionByName(&f).unwrap();
                r.ops.push(PushFunction(r.currentNamespace.id as u32, a.1 as u32))
            }
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

                            r.ops.push(OpCode::ArrayLoad(v.clone()));
                        }
                        Generic::Any => panic!(),
                    }
                }
                v => panic!("{v:?}"),
            }
        }
        Expression::NotExpression(e) => {
            genExpression(r.constructCtx(e))?;
            r.ops.push(Not)
        }
        Expression::NamespaceAccess(_) => todo!(),
        Expression::Lambda(_, _) => todo!(),
        Expression::Callable(prev, args) => {
            let mut argTypes = vec![];

            for arg in args {
                let t = r.constructCtx(arg).toDataType()?;
                match t {
                    None => { return Err(Box::new(NoValue { msg: String::from("aahhh"), })); }
                    Some(v) => {
                        argTypes.push(v);
                        genExpression(r.constructCtx(arg))?;
                    }
                }
            }

            println!("COZE? {:?}", prev);
            unsafe {
                if let Expression::Variable(v) = &**(prev as *const Box<Expression>) {
                    if !r.vTable.contains_key(&MyStr::Static(&v)) {
                        let genName = genFunName(&v, &args.iter().map(|it| { r.constructCtx(it).toDataType().unwrap().unwrap() }).collect::<Vec<_>>());
                        let funcId = r.currentNamespace.functionsLookup.get(&genName).ok_or(format!("could not find function with type {}", &genName)).unwrap();

                        r.ops.push(SCall { id: *funcId });
                    }
                    else {
                        genExpression(r.constructCtx(prev))?;
                        r.ops.push(DynamicCall)
                    }
                }
                else if let Expression::NamespaceAccess(v) = &**(prev as *const Box<Expression>) {
                    let genName = genFunName(v.last().unwrap(), &args.iter().map(|it| { r.constructCtx(it).toDataType().unwrap().unwrap() }).collect::<Vec<_>>());
                    let namespaceName = v[0..v.len()-1].join("::");
                    let funcId = r.vm.namespaces.get(*r.vm.namespaceLookup.get(&namespaceName).unwrap()).unwrap().functionsLookup.get(&genName).ok_or(format!("could not find function with type {}", &genName)).unwrap();

                    r.ops.push(LCall { namespace: 0, id: *funcId });
                }
                else {
                    genExpression(r.constructCtx(prev))?;
                    r.ops.push(DynamicCall)
                }
            }
        }
        Expression::StructInit(name, init) => {
            let structID  = *r.currentNamespace.structLookup.get(name).unwrap();
            let structMeta = r.currentNamespace.structs.get(structID).unwrap();
            let namespaceID = r.currentNamespace.id;

            r.ops.push(New { namespaceID, structID });

            for (fieldName, value) in init {
                r.ops.push(Dup);
                let fieldID = *structMeta.fieldsLookup.get(fieldName.into()).unwrap();
                let ctx = r.constructCtx(value);
                genExpression(ctx)?;
                r.ops.push(SetField {
                    namespaceID,
                    structID,
                    fieldID,
                })
            }
        }
        Expression::FieldAccess(prev, fieldName) => {
            let ctx = r.constructCtx(prev);
            genExpression(ctx)?;

            let e = r.constructCtx(prev).toDataType()?.ok_or("cannot array index none")?;
            match e {
                Object(o) => {
                    let structID = r.currentNamespace.structLookup.get(o.name.as_str()).unwrap();
                    let structMeta = r.currentNamespace.structs.get(*structID).unwrap();
                    let fieldID = structMeta.fieldsLookup.get(fieldName).unwrap();


                    r.ops.push(GetField {
                        namespaceID: r.currentNamespace.id,
                        structID: *structID,
                        fieldID: *fieldID,
                    });
                }
                _ => panic!(),
            };
        }
    }
    Ok(())
}