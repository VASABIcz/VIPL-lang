use std::cell::UnsafeCell;
use std::collections::HashMap;

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Deref;
use libc::open;

use crate::ast::{Expression, FunctionDef, ModType, Node, BinaryOp, Statement, StructDef, ArithmeticOp};
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::errors::{InvalidTypeException, NoValue, TypeNotFound, SymbolNotFound, CodeGenError, SymbolType, TypeError};
use crate::errors::CodeGenError::{UnexpectedVoid, UntypedEmptyArray};
use crate::lexer::*;
use crate::optimizer::{evalE};
use crate::parser::*;
use crate::utils::genFunName;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::dataType::DataType::{Bool, Char, Int, Object, Void};
use crate::vm::dataType::Generic::Any;
use crate::vm::myStr::MyStr;
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta, Namespace};
use crate::vm::vm::{JmpType, OpCode, VirtualMachine};
use crate::vm::vm::JmpType::{False, True};
use crate::vm::vm::OpCode::{Add, ArrayLength, ArrayLoad, ArrayNew, ArrayStore, Div, Dup, DynamicCall, GetChar, GetField, Jmp, LCall, Less, Mul, New, Not, Pop, PushChar, PushFunction, PushInt, PushIntOne, PushIntZero, GetLocal, Return, SCall, SetField, SetLocal, StringLength, StrNew, Sub, Swap, SetGlobal};

const DEBUG: bool = true;

#[derive(Debug, Clone)]
pub enum SymbolicOpcode {
    Op(OpCode),
    Continue,
    Break,
    Jmp(usize, JmpType),
    // zero sized
    LoopBegin,
    LoopEnd,
    LoopLabel(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    statements: Vec<Statement>
}

impl Body {
    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }
}

pub struct SimpleCtx<'a> {
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub handle: fn(&mut StatementCtx, DataType) -> (),
    pub labelCounter: &'a mut usize
}

impl SimpleCtx<'_> {
    pub fn inflate<'a>(&'a mut self, statement: &'a Statement, vTable: &'a mut HashMap<MyStr, (DataType, usize)>, loopContext: Option<usize>) -> StatementCtx<'a> {
        StatementCtx {
            statement: &statement,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable,
            loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
        }
    }
}

impl Body {
    pub fn new(b: Vec<Statement>) -> Self {
        Self {
            statements: b,
        }
    }

    pub fn buildLocalsTable(&self, mut ctx: SimpleCtx, locals: &mut HashMap<MyStr, (DataType, usize)>) -> Result<(), CodeGenError> {
        let mut buf = vec![];


        for statement in &self.statements {
            let mut statementCtx = ctx.inflate(statement, locals, None);

            buildLocalsTable(&mut statementCtx, &mut buf)?;
        }

        Ok(())
    }

    pub fn generate(&self, mut ctx: SimpleCtx, locals: &mut HashMap<MyStr, (DataType, usize)>) -> Result<(), CodeGenError> {
        for statement in &self.statements {
            let y = ctx.inflate(statement, locals, None);
            genStatement(y)?;
        }

        Ok(())
    }
}


impl Into<SymbolicOpcode> for OpCode {
    fn into(self) -> SymbolicOpcode {
        Op(self)
    }
}

impl SymbolicOpcode {
    pub fn isOp(&self, op: OpCode) -> bool {
        match self {
            Op(v) => v == &op,
            _ => false
        }
    }
}

pub fn emitOpcodes(syms: Vec<SymbolicOpcode>) -> Vec<OpCode> {
    let mut buf = vec![];
    let mut loopBegins = vec![];
    let mut labelLookup = HashMap::new();


    let mut counter = 0usize;
    for sym in &syms {
        match sym {
            SymbolicOpcode::Op(_) | SymbolicOpcode::Continue | SymbolicOpcode::Break | SymbolicOpcode::Jmp(_, _) => counter += 1,
            SymbolicOpcode::LoopLabel(id) => { labelLookup.insert(id, counter); },
            _ => {}
        }
    }

    for (i, sym) in syms.iter().enumerate() {
        match sym {
            SymbolicOpcode::Op(op) => buf.push(op.clone()),
            SymbolicOpcode::Continue => {
                let i = *loopBegins.last().unwrap();
                buf.push(OpCode::Jmp { offset: i as isize-(buf.len()+1) as isize, jmpType: JmpType::Jmp })
            }
            SymbolicOpcode::Break => {
                let mut depth = 0usize;
                let mut retId = None;

                for (i, op) in syms[i..].iter().enumerate() {
                    match op {
                        SymbolicOpcode::LoopBegin => {
                            depth += 1
                        }
                        SymbolicOpcode::LoopEnd => {
                            if depth == 0 {
                                retId = Some(i);
                            }
                            else {
                                depth -= 1;
                            }
                        }
                        _ => {}
                    }
                }

                buf.push(OpCode::Jmp { offset: retId.unwrap() as isize-1, jmpType: JmpType::Jmp })
            }
            SymbolicOpcode::Jmp(id, t) => {
                let i = *labelLookup.get(&id).unwrap();
                buf.push(OpCode::Jmp { offset: i as isize-(buf.len()+1) as isize, jmpType: *t })
            }
            SymbolicOpcode::LoopBegin => loopBegins.push(buf.len()),
            SymbolicOpcode::LoopEnd => { loopBegins.pop().unwrap(); }
            SymbolicOpcode::LoopLabel(_) => {}
        }
    }

    buf
}


#[derive(Debug)]
pub struct ExpressionCtx<'a> {
    pub exp: &'a Expression,
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub labelCounter: &'a mut usize
}

impl ExpressionCtx<'_> {
    pub fn genExpression(mut self) -> Result<(), CodeGenError> {
        genExpression(self)
    }
}

impl PartialExprCtx<'_> {
    pub fn lookupFunctionByBaseName(&mut self, name: &str) -> Option<String> {
        println!("funcs {:?}", self.functionReturns.keys());
        let idk = format!("{}::{}(", self.currentNamespace.get_mut().name, name);
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
    pub fn hasField(&self, t: DataType) -> bool {
        todo!()
    }

    pub fn lookupFunctionByBaseName(&mut self, name: &str) -> Result<String, CodeGenError> {
        println!("funcs {:?}", self.functionReturns.keys());
        let idk = format!("{}::{}(", self.currentNamespace.get_mut().name, name);
        println!("finding {}", idk);

        for (k, v) in self.functionReturns {
            if k.as_str().starts_with(&idk) {
                println!("found {}", k);
                return Ok(k.to_string());
            }
        }
        return Err(CodeGenError::SymbolNotFound(SymbolNotFound{ name: name.to_string(), typ: SymbolType::Function }));
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
            labelCounter: self.labelCounter,
        }
    }

    pub fn toDataType(&mut self) -> Result<DataType, CodeGenError> {
        match self.exp {
            Expression::BinaryOperation {
                left,
                right: _,
                op: o,
            } => {
                match o {
                    BinaryOp::Gt => return Ok(Bool),
                    BinaryOp::Less => return Ok(Bool),
                    BinaryOp::Eq => return Ok(Bool),
                    BinaryOp::And => return Ok(Bool),
                    BinaryOp::Or => return Ok(Bool),
                    _ => {}
                }
                let _leftType = self.transfer(left).toDataType().unwrap();

                Ok(_leftType)
            }
            Expression::IntLiteral(_) => Ok(DataType::Int),
            Expression::LongLiteral(_) => {
                // FIXME
                Ok(DataType::Int)
            }
            Expression::FloatLiteral(_) => Ok(DataType::Float),
            Expression::DoubleLiteral(_) => {
                // FIXME
                Ok(DataType::Float)
            }
            Expression::StringLiteral(_) => Ok(DataType::str()),
            Expression::Variable(name) => {
                match self.vTable.get(&MyStr::Runtime(name.clone().into_boxed_str())) {
                    None => {
                        println!("JEBE");
                        let funcName = self.lookupFunctionByBaseName(name)?;
                        let f = self.currentNamespace.get_mut().findFunction(&funcName).unwrap();
                        return Ok(f.0.toFunctionType());
                    },
                    Some(v) => Ok(v.0.clone()),
                }
            }
            Expression::BoolLiteral(_) => Ok(DataType::Bool),
            Expression::CharLiteral(_) => Ok(Char),
            Expression::ArrayLiteral(e) => {
                if e.is_empty() {
                    let l = &self.typeHint;
                    let c = l.clone().ok_or(CodeGenError::UntypedEmptyArray)?;
                    match c
                    {
                        Object(o) => {
                            if o.name.as_str() == "Array" {
                                let e = o
                                    .generics
                                    .first()
                                    .ok_or(CodeGenError::ArrayWithoutGenericParameter)?;
                                Ok(DataType::arr(e.clone()))
                            } else {
                                Err(CodeGenError::TypeError(TypeError {
                                    expected: DataType::Object(ObjectMeta {
                                        name: MyStr::from("Array"),
                                        generics: Box::new([Any]),
                                    }),
                                    actual: Object(o.clone()),
                                    exp: Some(e.first().unwrap().clone()),
                                }))
                            }
                        }
                        v => Err(CodeGenError::TypeError(TypeError {
                            expected: DataType::arr(Any),
                            actual: v.clone(),
                            exp: Some(e.first().unwrap().clone())
                        })),
                    }
                } else {
                    let t = self.transfer(e
                        .get(0)
                        .ok_or(CodeGenError::UntypedEmptyArray)?).toDataType()?;


                    if t.isVoid() {
                        return Err(UnexpectedVoid)
                    }

                    Ok(DataType::arr(Generic::Type(t)))
                }
            }
            Expression::ArrayIndexing(i) => {
                let e = self.transfer(&i.expr).toDataType()?;

                if e.isVoid() {
                    return Err(UnexpectedVoid)
                }

                match e {
                    Object(o) => {
                        if o.name.as_str() == "String" {
                            return Ok(Char);
                        }
                        Ok(
                            o.generics
                                .first()
                                .ok_or(CodeGenError::UntypedEmptyArray)?
                                .clone()
                                .ok_or(CodeGenError::UntypedEmptyArray)?,
                        )
                    }
                    _ => panic!(),
                }
            }
            Expression::NotExpression(i, _) => {
                self.transfer(i).toDataType()?.assertType(Bool)?;


                Ok(Bool)
            }
            Expression::NamespaceAccess(n) => {
                let (namespace, namespaceId) = self.vm.get_mut().findNamespaceParts(&n[..n.len()-1])?;

                let global = namespace.findGlobal(n.last().unwrap())?;

                Ok(global.0.typ.clone())
            },
            Expression::Lambda(l, body, ret) => {
                Ok(DataType::Function { args: l.iter().map(|it| it.typ.clone()).collect(), ret: Box::new(ret.clone().unwrap_or(DataType::Void)) })
                //Ok(self.typeHint.clone())
            },
            Expression::Callable(prev, args) => {
                unsafe {
                    match &**(prev as *const Box<Expression>) {
                        Expression::Variable(v) => {
                            if !self.vTable.contains_key(&MyStr::Static(&v)) {
                                let genName = genFunName(&v, &args.iter().map(|it| { self.transfer(it).toDataType().unwrap() }).collect::<Vec<_>>());
                                let funcId = self.currentNamespace.get_mut().findFunction(&genName)?;
                                return Ok(funcId.0.returnType.clone())
                            }

                            let var = self.vTable.get(&MyStr::Static(&v)).unwrap();
                            match &var.0 {
                                DataType::Function { args, ret } => {
                                    return Ok(*ret.clone())
                                }
                                _ => panic!()
                            }
                        }
                        Expression::NamespaceAccess(v) => {
                            let (namespace, _) = (&mut *self.vm.get()).findNamespaceParts(&v[..v.len()-1])?;

                            let genName = genFunName(&v.join("::"), &args.iter().map(|it| { self.transfer(it).toDataType().unwrap() }).collect::<Vec<_>>());

                            let funcId = namespace.findFunction(&genName)?;
                            return Ok(funcId.0.returnType.clone())
                        }
                        Expression::FieldAccess(a, b) => {
                            let t = self.transfer(a).toDataType()?;
                            todo!()
                        }
                        _ => panic!()
                    }
                }
            }
            Expression::StructInit(name, _) => {
                Ok(DataType::Object(ObjectMeta{ name: name.clone().into(), generics: Box::new([]) }))
            }
            Expression::FieldAccess(prev, fieldName) => {
                let e = self.transfer(prev).toDataType()?;
                match e {
                    Object(o) => {
                        match o.name.as_str() {
                            "Array" | "String" => {
                                return Ok(Int)
                            }
                            _ => {}
                        }

                        let (structMeta, _) = self.currentNamespace.get_mut().findStruct(o.name.as_str())?;
                        let fieldID = structMeta.fieldsLookup.get(fieldName).unwrap();
                        let t = structMeta.fields.get(*fieldID).unwrap();
                        Ok(t.typ.clone())
                    }
                    _ => panic!(),
                }
            }
            Expression::Null => Ok(self.typeHint.clone().unwrap_or(Void)),
            Expression::TernaryOperator(cond, tr, fal) => {
                let a = self.transfer(&tr).toDataType()?;
                let b = self.transfer(&fal).toDataType()?;
                let c = self.transfer(&cond).toDataType()?;

                a.assertType(b)?;
                c.assertType(Bool)?;

                Ok(a)
            }
        }
    }
}

#[derive(Debug)]
struct OpCodeManager<'a> {
    pub ops: &'a mut Vec<OpCode>
}

impl OpCodeManager<'_> {
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(op)
    }
}

pub struct PartialExprCtx<'a> {
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a HashMap<MyStr, (DataType, usize)>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub labelCounter: &'a mut usize
}

impl PartialExprCtx<'_> {
    #[inline(always)]
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(SymbolicOpcode::Op(op))
    }

    pub fn beginLoop(&mut self) {
        self.ops.push(SymbolicOpcode::LoopBegin)
    }

    pub fn endLoop(&mut self) {
        self.ops.push(SymbolicOpcode::LoopEnd)
    }

    pub fn opContinue(&mut self) {
        self.ops.push(SymbolicOpcode::Continue)
    }

    pub fn opBreak(&mut self) {
        self.ops.push(SymbolicOpcode::Break)
    }

    pub fn nextLabel(&mut self) -> usize {
        let id = *self.labelCounter;
        *self.labelCounter += 1;

        id
    }

    pub fn makeLabel(&mut self, id: usize) {
        self.ops.push(SymbolicOpcode::LoopLabel(id));
    }

    pub fn opLabel(&mut self) -> usize {
        let id = *self.labelCounter;
        *self.labelCounter += 1;
        self.ops.push(SymbolicOpcode::LoopLabel(id));

        id
    }

    pub fn opJmp(&mut self, label: usize, jmpType: JmpType) {
        self.ops.push(SymbolicOpcode::Jmp(label, jmpType))
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
            labelCounter: self.labelCounter,
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
            labelCounter: self.labelCounter,
        };
        let e = self.exp;

        (e, p)
    }
}

#[derive(Debug)]
pub struct StatementCtx<'a> {
    pub statement: &'a Statement,
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub functionReturns: &'a HashMap<MyStr, Option<DataType>>,
    pub vTable: &'a mut HashMap<MyStr, (DataType, usize)>,
    pub loopContext: Option<usize>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub handle: fn(&mut StatementCtx, DataType) -> (),
    pub labelCounter: &'a mut usize
}

impl StatementCtx<'_> {
    pub fn deflate(&mut self) -> (SimpleCtx, &mut HashMap<MyStr, (DataType, usize)>) {
        (SimpleCtx {
            ops: self.ops,
            functionReturns: self.functionReturns,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
        }, self.vTable)
    }

    pub fn transfer<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            loopContext: self.loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
        }
    }

    pub fn beginLoop(&mut self) {
        self.ops.push(SymbolicOpcode::LoopBegin)
    }

    pub fn endLoop(&mut self) {
        self.ops.push(SymbolicOpcode::LoopEnd)
    }

    pub fn opContinue(&mut self) {
        self.ops.push(SymbolicOpcode::Continue)
    }

    pub fn opBreak(&mut self) {
        self.ops.push(SymbolicOpcode::Break)
    }

    pub fn opLabel(&mut self) -> usize {
        let id = *self.labelCounter;
        *self.labelCounter += 1;
        self.ops.push(SymbolicOpcode::LoopLabel(id));

        id
    }

    pub fn nextLabel(&mut self) -> usize {
        let id = *self.labelCounter;
        *self.labelCounter += 1;

        id
    }

    pub fn makeLabel(&mut self, id: usize) {
        self.ops.push(SymbolicOpcode::LoopLabel(id));
    }

    pub fn opJmp(&mut self, label: usize, jmpType: JmpType) {
        self.ops.push(SymbolicOpcode::Jmp(label, jmpType))
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
            labelCounter: self.labelCounter,
        }
    }
}

impl ExpressionCtx<'_> {
    #[inline]
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(SymbolicOpcode::Op(op))
    }
}

impl StatementCtx<'_> {
    #[inline]
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(SymbolicOpcode::Op(op))
    }

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
            labelCounter: self.labelCounter,
        }
    }

    pub fn copy<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            functionReturns: self.functionReturns,
            vTable: self.vTable,
            loopContext: self.loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
        }
    }
}

pub fn buildLocalsTable(ctx: &mut StatementCtx, locals: &mut Vec<VariableMetadata>) -> Result<(), CodeGenError> {
    match ctx.statement {
        Statement::While(w) => {
            for s in &w.body.statements {
                buildLocalsTable(&mut ctx.transfer(s), locals)?;
            }
        }
        Statement::If(i) => {
            for s in &i.body.statements {
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
        Statement::Assignable(prev, init, t) => {
            if *t != None {
                return Ok(())
            }
            match prev {
                Expression::Variable(c) => {
                    if !ctx.vTable.contains_key(&c.clone().into()) {
                        let t = ctx.makeExpressionCtx(init, None).toDataType()?;
                        if t.isVoid() {
                            panic!()
                        }
                        ctx.vTable.insert(
                            c.clone().into(),
                            (t.clone(), locals.len()),
                        );
                        locals.push(VariableMetadata {
                            name: c.clone().into(),
                            typ: t,
                        });
                    }
                }
                _ => {}
            }
        },
        Statement::ForLoop(var, exp, body) => {
            let t = ctx.makeExpressionCtx(exp, None).toDataType()?;
            let arr = t.asArray()?;
            let typ = arr.generics.first().unwrap().clone().ok_or(UntypedEmptyArray)?;

            if !ctx.vTable.contains_key(&var.clone().into()) {
                ctx.vTable.insert(
                    var.clone().into(),
                    (typ.clone(), locals.len()),
                );
                locals.push(VariableMetadata {
                    name: var.clone().into(),
                    typ,
                });
            }

            for s in body {
                buildLocalsTable(&mut ctx.transfer(s), locals)?;
            }
        }
        _ => {}
    }

    Ok(())
}

pub fn genFunctionDef(fun: &FunctionMeta, mut ctx: SimpleCtx) -> Result<Vec<VariableMetadata>, CodeGenError> {
    let mut vTable = HashMap::new();
    let mut locals = vec![];

    for arg in fun.localsMeta.iter() {
        vTable.insert(arg.name.clone(), (arg.typ.clone(), locals.len()));
        locals.push(arg.clone())
    }

    if let FunctionTypeMeta::Runtime(body) = &fun.functionType {
        for s in &body.statements {
            let mut sCtx = ctx.inflate(&s, &mut vTable, None);

            buildLocalsTable(&mut sCtx, &mut locals)?;
        }

        for statement in &body.statements {
            if DEBUG {
                println!("gening {:?}", statement)
            }
            let sCtx = ctx.inflate(&statement, &mut vTable, None);
            genStatement(sCtx)?;
        }
        match ctx.ops.last() {
            None => ctx.ops.push(Op(Return)),
            Some(op) => {
                if !op.isOp(Return) {
                    ctx.ops.push(Op(Return));
                }
            }
        }

        Ok(locals)
    }
    else {
        panic!()
    }
}

pub fn genStatement(mut ctx: StatementCtx) -> Result<(), CodeGenError> {
    match ctx.statement {
        Statement::While(w) => {
            ctx.makeExpressionCtx(&w.exp, None).toDataType()?.assertType(Bool)?;

            let loopEnd = ctx.nextLabel();

            ctx.beginLoop();

            genExpression(ctx.makeExpressionCtx(&w.exp, None))?;

            ctx.opJmp(loopEnd, False);

            let (newCtx, vTable) = ctx.deflate();

            w.body.generate(newCtx, vTable)?;

            ctx.opContinue();

            ctx.makeLabel(loopEnd);
            ctx.endLoop();

        }
        Statement::If(flow) => {
            ctx.makeExpressionCtx(&flow.condition, None).toDataType()?.assertType(Bool)?;

            let endLabel = ctx.nextLabel();

            let mut ifLabels = vec![];

            for _ in 0..flow.elseIfs.len() {
                ifLabels.push(ctx.nextLabel());
            }

            if flow.elseBody.is_some() {
                ifLabels.push(ctx.nextLabel());
            }

            genExpression(ctx.makeExpressionCtx(&flow.condition, None))?;
            if let Some(v) = ifLabels.first() {
                ctx.opJmp(*v, False);
            }
            else {
                ctx.opJmp(endLabel, False)
            }

            let (newCtx, vTable) = ctx.deflate();

            flow.body.generate(newCtx, vTable)?;

            ctx.opJmp(endLabel, JmpType::Jmp);

            for (i, els) in flow.elseIfs.iter().enumerate() {
                ctx.makeLabel(*ifLabels.get(i).unwrap());

                ctx.makeExpressionCtx(&els.0, None).toDataType()?.assertType(Bool)?;
                ctx.makeExpressionCtx(&els.0, None).genExpression()?;

                if let Some(v) = ifLabels.get(i+1) {
                    ctx.opJmp(*v, False);
                }
                else {
                    ctx.opJmp(endLabel, False);
                }

                for s in &els.1 {
                    let op = ctx.copy(s);
                    genStatement(op)?;
                }
                ctx.opJmp(endLabel, JmpType::Jmp);
            }

            if let Some(els) = &flow.elseBody {
                ctx.makeLabel(*ifLabels.last().unwrap());
                for s in els {
                    let ctx1 = ctx.copy(s);
                    genStatement(ctx1)?;
                }
            }
            ctx.makeLabel(endLabel);
        }
        Statement::Return(ret) => {
            genExpression(ctx.makeExpressionCtx(&ret, None))?;
            ctx.push(Return)
        }
        Statement::Continue => ctx.opContinue(),
        Statement::Break => ctx.opBreak(),
        Statement::Loop(body) => {
            let context = ctx.ops.len();
            ctx.beginLoop();
            for s in body {
                let mut cop = ctx.copy(s);
                cop.loopContext = Some(context);
                genStatement(cop)?;
            }
            ctx.opContinue();
            ctx.endLoop();
        }
        Statement::NamespaceFunction(path, f) => {
            let t = f.arguments.iter().map(|it| {
                ctx.makeExpressionCtx(it, None).toDataType().unwrap()
            }).collect::<Vec<_>>();

            let r = path.join("::");
            let (namespace, namespaceId) = ctx.vm.get_mut().findNamespace(&r).unwrap();
            let n = genFunName(f.name.as_str(), &t);

            let funcId = namespace.findFunction(&n)?.1;

            for arg in &f.arguments {
                genExpression(ctx.makeExpressionCtx(arg, None))?;
            }

            ctx.push(LCall { namespace: namespaceId, id: funcId })
        }
        Statement::StatementExpression(v) => {
            let mut eCtx = ctx.makeExpressionCtx(v, None);

            let ret = eCtx.toDataType()?;

            genExpression(eCtx)?;

            if ret != Void {
                (ctx.handle)(&mut ctx, ret);
            }
        }
        Statement::Assignable(dest, value, t) => {
            if ctx.makeExpressionCtx(value, None).toDataType()?.isVoid() {
                return Err(CodeGenError::UnexpectedVoid)
            }

            match dest {
                Expression::Variable(_) => {}
                Expression::ArrayIndexing(v) => {
                    ctx.makeExpressionCtx(&v.expr, None).genExpression()?;
                }
                Expression::NamespaceAccess(_) => {},
                Expression::FieldAccess(obj, _) => {
                    let mut cd = ctx.makeExpressionCtx(obj, None);

                    cd.genExpression()?;
                }
                _ => panic!()
            }

            match t {
                None => {
                    ctx.makeExpressionCtx(value, None).genExpression()?;
                }
                Some(v) => {
                    let mut c1 = ctx.makeExpressionCtx(dest, None);
                    let t = c1.toDataType()?;
                    c1.genExpression()?;
                    ctx.makeExpressionCtx(value, None).genExpression()?;
                    let o = match v {
                        ArithmeticOp::Add => OpCode::Add(t),
                        ArithmeticOp::Sub => OpCode::Sub(t),
                        ArithmeticOp::Mul => OpCode::Mul(t),
                        ArithmeticOp::Div => OpCode::Div(t)
                    };
                    ctx.push(o);
                }
            }

            match dest {
                Expression::Variable(v) => {
                    let var = ctx.vTable.get(&v.clone().into()).unwrap();

                    ctx.push(SetLocal { index: var.1 })
                }
                Expression::ArrayIndexing(v) => {
                    ctx.makeExpressionCtx(&v.index, None).genExpression()?;

                    ctx.push(ArrayStore)
                },
                Expression::NamespaceAccess(v) => unsafe {
                    let namespace = (&mut *ctx.vm.get()).findNamespaceParts(&v[..v.len()-1])?;
                    let global = namespace.0.findGlobal(v.last().unwrap())?;

                    ctx.push(SetGlobal { namespaceID: namespace.1, globalID: global.1 })
                },
                Expression::FieldAccess(obj, field) => {
                    let mut cd = ctx.makeExpressionCtx(obj, None);
                    let class = match cd.toDataType()? {
                        Object(v) => v,
                        _ => panic!()
                    };

                    let namespaceID = cd.currentNamespace.get_mut().id;
                    let (str, structID) = cd.currentNamespace.get_mut().findStruct(class.name.as_str())?;
                    let fieldID = *str.fieldsLookup.get(field).unwrap();

                    ctx.push(SetField {
                        namespaceID,
                        structID,
                        fieldID,
                    })
                }
                _ => panic!()
            }
        }
        Statement::ForLoop(var, arr, body) => {
            let varId = ctx.vTable.get(&var.clone().into()).unwrap().1;
            let endLabel = ctx.nextLabel();

            // BEGIN

            // iteration counter
            ctx.push(PushIntZero);

            ctx.beginLoop();

            ctx.push(Dup);

            ctx.makeExpressionCtx(arr, None).genExpression()?;

            // dec array length bcs less-eq is not implemented
            ctx.push(ArrayLength);
            ctx.push(PushIntOne);
            ctx.push(Sub(Int));

            ctx.push(Less(Int));

            ctx.opJmp(endLabel, True);

            ctx.push(Dup);
            ctx.makeExpressionCtx(arr, None).genExpression()?;
            ctx.push(Swap);

            ctx.push(ArrayLoad);
            ctx.push(SetLocal { index: varId });

            for s in body {
                let sCtx = ctx.transfer(s);
                genStatement(sCtx)?;
            }

            ctx.opJmp(endLabel, True);

            ctx.push(PushIntOne);
            ctx.push(Add(DataType::Int));

            ctx.opContinue();

            ctx.endLoop();
            ctx.makeLabel(endLabel);
            ctx.push(Pop);
        }
    }
    Ok(())
}

fn genExpression(mut ctx: ExpressionCtx) -> Result<(), CodeGenError> {
    let (e, mut r) = ctx.reduce();

    unsafe {
        match e {
            Expression::BinaryOperation { left, right, op } => {
                let dat = r.constructCtx(left).toDataType()?.assertNotVoid()?;
                genExpression(r.constructCtx(left))?;
                genExpression(r.constructCtx(right))?;
                let t = match op {
                    BinaryOp::Add => OpCode::Add(dat),
                    BinaryOp::Sub => OpCode::Sub(dat),
                    BinaryOp::Mul => OpCode::Mul(dat),
                    BinaryOp::Div => OpCode::Div(dat),
                    BinaryOp::Gt => OpCode::Greater(dat),
                    BinaryOp::Less => OpCode::Less(dat),
                    BinaryOp::Eq => OpCode::Equals(dat),
                    BinaryOp::And => OpCode::And,
                    BinaryOp::Or => OpCode::Or,
                };
                r.push(t);
            }
            Expression::IntLiteral(i) => r.push(PushInt(i.parse::<isize>().unwrap())),
            Expression::LongLiteral(i) => r.push(PushInt(i.parse::<isize>().unwrap())),
            Expression::FloatLiteral(i) => r.push(OpCode::PushFloat(i.parse::<f64>().unwrap())),
            Expression::DoubleLiteral(i) => r.push(OpCode::PushFloat(i.parse::<f64>().unwrap())),
            Expression::StringLiteral(i) => {
                unsafe { r.push(StrNew((&mut *r.currentNamespace.get()).allocateOrGetString(i))); }
            }
            Expression::BoolLiteral(i) => r.push(OpCode::PushBool(*i)),
            Expression::Variable(v) => {
                unsafe {
                    if r.vTable.contains_key(&v.clone().into()) {
                        r.push(OpCode::GetLocal {
                            index: r
                                .vTable
                                .get(&MyStr::Runtime(v.clone().into_boxed_str()))
                                .ok_or(CodeGenError::SymbolNotFound(SymbolNotFound { name: v.clone(), typ: SymbolType::Variable }))?
                                .1,
                        });
                    } else {
                        let f = r.lookupFunctionByBaseName(v).unwrap();
                        let a = (&mut *r.currentNamespace.get()).findFunction(&f).unwrap();
                        r.push(PushFunction((&*r.currentNamespace.get()).id as u32, a.1 as u32))
                    }
                }
            }
            Expression::CharLiteral(c) => r.push(PushChar(*c)),
            Expression::ArrayLiteral(i) => {
                let d = match r.typeHint {
                    None => Some(
                        r.constructCtx(i.first().ok_or(UntypedEmptyArray)?).toDataType()?.assertNotVoid()?
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
                let e = d.ok_or(UnexpectedVoid)?;
                // let d = i.get(0).ok_or("array must have at least one element")?.toDataType(vTable, functionReturns, None)?.ok_or("array elements must have type")?;
                r.push(PushInt(i.len() as isize));
                r.push(ArrayNew(e.clone()));
                for (ind, exp) in i.iter().enumerate() {
                    r.push(Dup);
                    genExpression(r.constructCtx(exp))?;
                    r.push(PushInt(ind as isize));
                    r.push(ArrayStore);
                }
            }
            Expression::ArrayIndexing(i) => {
                // println!("{:?}", i.expr);
                let d = r.constructCtx(&i.expr).toDataType()?.assertNotVoid()?;
                match d {
                    DataType::Object(o) => {
                        // println!("{:?}", o);
                        if o.name.as_str() == "String" {
                            genExpression(r.constructCtx(&i.expr))?;
                            genExpression(r.constructCtx(&i.index))?;
                            r.push(GetChar);
                            return Ok(());
                        }

                        match o.generics.first().unwrap() {
                            Generic::Type(v) => {
                                genExpression(r.constructCtx(&i.expr))?;
                                genExpression(r.constructCtx(&i.index))?;

                                r.push(OpCode::ArrayLoad);
                            }
                            Generic::Any => panic!(),
                        }
                    }
                    v => panic!("{v:?}"),
                }
            }
            Expression::NotExpression(e, _) => {
                genExpression(r.constructCtx(e))?;
                r.push(Not)
            }
            Expression::NamespaceAccess(parts) => {
                let namespace = (&*r.vm.get()).findNamespaceParts(&parts[..parts.len() - 1])?;

                let global = namespace.0.findGlobal(parts.last().unwrap())?;

                r.push(OpCode::GetGlobal { namespaceID: namespace.1, globalID: global.1 })
            },
            Expression::Lambda(args, body, ret) => {
                let meta = FunctionMeta::makeRuntime(
                    "lambda".to_string(),
                    args.clone().into_boxed_slice(),
                    0,
                    ret.clone().unwrap_or(Void),
                    body.clone()
                );

                // ctx.currentNamespace.registerFunctionDef()
            },
            Expression::Callable(prev, args) => {
                let mut argTypes = vec![];

                for arg in args {
                    let t = r.constructCtx(arg).toDataType()?.assertNotVoid()?;
                    argTypes.push(t);
                    genExpression(r.constructCtx(arg))?;
                }

                unsafe {
                    if let Expression::Variable(v) = &**(prev as *const Box<Expression>) {
                        if !r.vTable.contains_key(&MyStr::Static(&v)) {
                            let genName = genFunName(&v, &args.iter().map(|it| { r.constructCtx(it).toDataType().unwrap() }).collect::<Vec<_>>());
                            let funcId = r.currentNamespace.get_mut().findFunction(&genName)?.1;

                            r.push(SCall { id: funcId });
                        } else {
                            genExpression(r.constructCtx(prev))?;
                            r.push(DynamicCall)
                        }
                    } else if let Expression::NamespaceAccess(v) = &**(prev as *const Box<Expression>) {
                        let genName = genFunName(v.last().unwrap(), &args.iter().map(|it| { r.constructCtx(it).toDataType().unwrap() }).collect::<Vec<_>>());

                        let (namespace, namespaceId) = r.vm.get_mut().findNamespaceParts(&v[0..v.len() - 1]).unwrap();

                        let (func, funcId) = namespace.findFunction(&genName)?;

                        r.push(LCall { namespace: namespaceId, id: funcId });
                    } else {
                        genExpression(r.constructCtx(prev))?;
                        r.push(DynamicCall)
                    }
                }
            }
            Expression::StructInit(name, init) => {
                // FIXME doesnt support other namespaces
                let (structMeta, structID) = (&*r.currentNamespace.get()).findStruct(name)?;
                let namespaceID = (&*r.currentNamespace.get()).id;

                r.push(New { namespaceID, structID });

                for (fieldName, value) in init {
                    r.push(Dup);
                    let (_, fieldID) = structMeta.findField(fieldName)?;
                    let ctx = r.constructCtx(value);
                    genExpression(ctx)?;
                    r.push(SetField {
                        namespaceID,
                        structID,
                        fieldID,
                    })
                }
            }
            Expression::FieldAccess(prev, fieldName) => {
                let ctx = r.constructCtx(prev);
                genExpression(ctx)?;

                let e = r.constructCtx(prev).toDataType()?.assertNotVoid()?;
                match e {
                    Object(ref o) => {
                        if fieldName == "size" || fieldName == "length" {
                            if e.isArray() {
                                r.push(ArrayLength);
                                return Ok(())
                            } else if e.isString() {
                                r.push(StringLength);
                                return Ok(())
                            }
                        }

                        let (structMeta, structID, field, fieldID) = (&*r.currentNamespace.get()).findStructField(o.name.as_str(), fieldName)?;


                        r.push(GetField {
                            namespaceID: (&*r.currentNamespace.get()).id,
                            structID,
                            fieldID,
                        });
                    }
                    _ => panic!(),
                };
            }
            Expression::Null => {
                ctx.push(OpCode::PushIntZero)
            }
            Expression::TernaryOperator(cond, tr, fal) => {
                let a = r.constructCtx(&tr).toDataType()?;
                let b = r.constructCtx(&fal).toDataType()?;
                let c = r.constructCtx(&cond).toDataType()?;

                a.assertType(b)?;
                c.assertType(Bool)?;

                let falseLabel = r.nextLabel();
                let endLabel = r.nextLabel();


                println!("condition {:?}", cond);

                r.constructCtx(cond).genExpression()?;

                r.opJmp(falseLabel, False);

                r.constructCtx(tr).genExpression()?;
                r.opJmp(endLabel, JmpType::Jmp);

                r.makeLabel(falseLabel);
                r.constructCtx(fal).genExpression()?;

                r.makeLabel(endLabel);
            }
        }
    }
    Ok(())
}