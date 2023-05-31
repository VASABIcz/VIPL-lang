use core::slice::sort::quicksort;
use std::cell::UnsafeCell;
use std::collections::HashMap;

use libc::open;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Deref;

use crate::ast::{
    ArithmeticOp, BinaryOp, Expression, FunctionDef, ModType, Node, Statement, StructDef,
};
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::errors::CodeGenError::{LiteralParseError, UnexpectedVoid, UntypedEmptyArray};
use crate::errors::{
    CodeGenError, InvalidTypeException, NoValue, SymbolNotFoundE, SymbolType, TypeError,
    TypeNotFound,
};
use crate::lexer::*;
use crate::parser::*;
use crate::symbolManager::SymbolManager;
use crate::utils::genFunName;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int, Object, Void};
use crate::vm::dataType::Generic::Any;
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta, Namespace};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::JmpType::{False, True};
use crate::vm::vm::OpCode::{Add, ArrayLength, ArrayLoad, ArrayNew, ArrayStore, Div, Dup, DynamicCall, GetChar, GetField, GetLocal, Greater, Jmp, LCall, Less, Mul, New, Not, Pop, PushChar, PushFunction, PushInt, PushIntOne, PushIntZero, Return, SCall, SetField, SetGlobal, SetLocal, StrNew, StringLength, Sub, Swap, I2F, F2I};
use crate::vm::vm::{JmpType, OpCode, VirtualMachine};

const DEBUG: bool = false;

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
    statements: Vec<Statement>,
}

impl Body {
    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }
}

pub struct SimpleCtx<'a> {
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub handle: fn(&mut StatementCtx, DataType) -> (),
    pub labelCounter: &'a mut usize,
    pub symbols: &'a mut SymbolManager
}

impl SimpleCtx<'_> {
    pub fn inflate<'a>(
        &'a mut self,
        statement: &'a Statement,
        loopContext: Option<usize>,
    ) -> StatementCtx<'a> {
        StatementCtx {
            statement: &statement,
            ops: self.ops,
            loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }
}

impl Body {
    pub fn new(b: Vec<Statement>) -> Self {
        Self { statements: b }
    }

    pub fn generate(
        &self,
        mut ctx: SimpleCtx,
    ) -> Result<(), CodeGenError> {
        ctx.symbols.enterScope();
        for statement in &self.statements {
            let y = ctx.inflate(statement, None);
            genStatement(y)?;
        }
        ctx.symbols.exitScope();

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
            _ => false,
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
            SymbolicOpcode::Op(_)
            | SymbolicOpcode::Continue
            | SymbolicOpcode::Break
            | SymbolicOpcode::Jmp(_, _) => counter += 1,
            SymbolicOpcode::LoopLabel(id) => {
                labelLookup.insert(id, counter);
            }
            _ => {}
        }
    }

    for (i, sym) in syms.iter().enumerate() {
        match sym {
            SymbolicOpcode::Op(op) => buf.push(op.clone()),
            SymbolicOpcode::Continue => {
                let i = *loopBegins.last().unwrap();
                buf.push(OpCode::Jmp {
                    offset: i as isize - (buf.len() + 1) as isize,
                    jmpType: JmpType::Jmp,
                })
            }
            SymbolicOpcode::Break => {
                let mut depth = 0usize;
                let mut retId = None;

                for (i, op) in syms[i..].iter().enumerate() {
                    match op {
                        SymbolicOpcode::LoopBegin => depth += 1,
                        SymbolicOpcode::LoopEnd => {
                            if depth == 0 {
                                retId = Some(i);
                            } else {
                                depth -= 1;
                            }
                        }
                        _ => {}
                    }
                }

                buf.push(OpCode::Jmp {
                    offset: retId.unwrap() as isize - 1,
                    jmpType: JmpType::Jmp,
                })
            }
            SymbolicOpcode::Jmp(id, t) => {
                let i = *labelLookup.get(&id).unwrap();
                buf.push(OpCode::Jmp {
                    offset: i as isize - (buf.len() + 1) as isize,
                    jmpType: *t,
                })
            }
            SymbolicOpcode::LoopBegin => loopBegins.push(buf.len()),
            SymbolicOpcode::LoopEnd => {
                loopBegins.pop().unwrap();
            }
            SymbolicOpcode::LoopLabel(_) => {}
        }
    }

    buf
}

#[derive(Debug)]
pub struct ExpressionCtx<'a> {
    pub exp: &'a Expression,
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub labelCounter: &'a mut usize,
    pub symbols: &'a SymbolManager
}

impl ExpressionCtx<'_> {
    pub fn getVariable(&self, varName: &str) -> Result<&(DataType, usize), CodeGenError> {
        self.symbols.getLocal(varName)
        // self.vTable.get(varName).ok_or(CodeGenError::SymbolNotFound(SymbolNotFoundE::var(varName)))
    }

    pub fn genExpression(mut self) -> Result<(), CodeGenError> {
        genExpression(self)
    }
}

impl PartialExprCtx<'_> {
    pub fn lookupFunctionByBaseName(&mut self, name: &str) -> Result<String, CodeGenError> {
        todo!()
/*        let idk = format!("{}::{}(", self.currentNamespace.get_mut().name, name);

        for (k, _) in self.functionReturns {
            if k.as_str().starts_with(&idk) {
                return Ok(k.to_string());
            }
        }
        return Err(CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)));*/
    }

    pub fn getVariable(&self, varName: &str) -> Result<&(DataType, usize), CodeGenError> {
        self.symbols.getLocal(varName)
    }
}

impl ExpressionCtx<'_> {
    pub fn hasField(&self, t: DataType) -> bool {
        todo!()
    }

    pub fn lookupFunctionByBaseName(&mut self, name: &str) -> Result<String, CodeGenError> {
        for (k, v) in &self.symbols.functions {
            if k.as_str().starts_with(&name) {
                return Ok(k.to_string());
            }
        }
        return Err(CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)));
    }

    pub fn transfer<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }

    pub fn toDataType(&mut self) -> Result<DataType, CodeGenError> {
        match self.exp {
            Expression::BinaryOperation {
                left,
                right: _,
                op: o,
            } => {
                let leftT = self.transfer(left).toDataType()?;
                let rightT = self.transfer(left).toDataType()?;

                if leftT != rightT {
                    return Err(CodeGenError::TypeError(TypeError{
                        expected: leftT,
                        actual: rightT,
                        exp: Some(self.exp.clone()),
                    }))
                }

                let t = match o {
                    BinaryOp::Gt => return Ok(Bool),
                    BinaryOp::Less => return Ok(Bool),
                    BinaryOp::Eq => return Ok(Bool),
                    BinaryOp::And => return Ok(Bool),
                    BinaryOp::Or => return Ok(Bool),
                    BinaryOp::Add => leftT,
                    BinaryOp::Sub => leftT,
                    BinaryOp::Mul => leftT,
                    BinaryOp::Div => Float
                };

                Ok(t)
            }
            Expression::IntLiteral(_) => Ok(DataType::Int),
            Expression::FloatLiteral(_) => Ok(DataType::Float),
            Expression::StringLiteral(_) => Ok(DataType::str()),
            Expression::Variable(name) => match self.getVariable(name) {
                Err(_) => {
                    let funcName = self.lookupFunctionByBaseName(name)?;
                    let f = self
                        .currentNamespace
                        .get_mut()
                        .findFunction(&funcName)?;
                    return Ok(f.0.toFunctionType());
                }
                Ok(v) => Ok(v.0.clone()),
            },
            Expression::BoolLiteral(_) => Ok(DataType::Bool),
            Expression::CharLiteral(_) => Ok(Char),
            Expression::ArrayLiteral(e) => {
                if e.is_empty() {
                    let l = &self.typeHint;
                    let c = l.clone().ok_or(CodeGenError::UntypedEmptyArray)?;
                    match c {
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
                                        name: "Array".to_string(),
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
                            exp: Some(e.first().unwrap().clone()),
                        })),
                    }
                } else {
                    let t = self
                        .transfer(e.get(0).ok_or(CodeGenError::UntypedEmptyArray)?)
                        .toDataType()?;

                    if t.isVoid() {
                        return Err(UnexpectedVoid);
                    }

                    Ok(DataType::arr(Generic::Type(t)))
                }
            }
            Expression::ArrayIndexing(i) => {
                let e = self.transfer(&i.expr).toDataType()?;

                if e.isVoid() {
                    return Err(UnexpectedVoid);
                }

                match e {
                    Object(o) => {
                        if o.name.as_str() == "String" {
                            return Ok(Char);
                        }
                        Ok(o.generics
                            .first()
                            .ok_or(CodeGenError::UntypedEmptyArray)?
                            .clone()
                            .ok_or(CodeGenError::UntypedEmptyArray)?)
                    }
                    _ => panic!(),
                }
            }
            Expression::NotExpression(i, _) => {
                self.transfer(i).toDataType()?.assertType(Bool)?;

                Ok(Bool)
            }
            Expression::NamespaceAccess(n) => {
                let (namespace, _) =
                    self.vm.get_mut().findNamespaceParts(&n[..n.len() - 1])?;

                let global = namespace.findGlobal(n.last().unwrap())?;

                Ok(global.0.typ.clone())
            }
            Expression::Lambda(l, _, ret) => {
                Ok(DataType::Function {
                    args: l.iter().map(|it| it.typ.clone()).collect(),
                    ret: Box::new(ret.clone().unwrap_or(Void)),
                })
            }
            Expression::Callable(prev, args) => unsafe {
                match &**(prev as *const Box<Expression>) {
                    Expression::Variable(v) => {
                        if self.getVariable(v).is_err() {
                            let genName = genFunName(
                                &v,
                                &args
                                    .iter()
                                    .map(|it| self.transfer(it).toDataType().unwrap())
                                    .collect::<Vec<_>>(),
                            );

                            let funcId = self.symbols.getFunction(&genName)?;
                            return Ok(funcId.1.clone());
                        }


                        let var = self.getVariable(v)?;
                        match &var.0 {
                            DataType::Function { ret, .. } => return Ok(*ret.clone()),
                            _ => panic!(),
                        }
                    }
                    Expression::NamespaceAccess(v) => {
                        let argz = args
                            .iter()
                            .map(|it| self.transfer(it).toDataType().unwrap())
                            .collect::<Vec<_>>();

                        Ok(self.symbols.getFunctionPartsArgs(&v, &argz)?.1.clone())
                    }
                    Expression::FieldAccess(a, b) => {
                        let t = self.transfer(a).toDataType()?;

                        match t {
                            DataType::Function { ret, .. } => Ok(*ret.clone()),
                            t => {
                                let mut fArgs = vec![];
                                fArgs.push(t);
                                fArgs.extend(
                                    args.iter()
                                        .map(|it| self.transfer(it).toDataType().unwrap()),
                                );

                                let f =
                                    self.currentNamespace.get_mut().lookupFunction(b, &fArgs)?;

                                Ok(f.0.returnType.clone())
                            }
                        }
                    }
                    _ => panic!(),
                }
            },
            Expression::StructInit(name, _) => Ok(DataType::Object(ObjectMeta {
                name: name.clone().into(),
                generics: Box::new([]),
            })),
            Expression::FieldAccess(prev, fieldName) => {
                let e = self.transfer(prev).toDataType()?;
                match e {
                    Object(o) => {
                        match o.name.as_str() {
                            "Array" | "String" => return Ok(Int),
                            _ => {}
                        }

                        let (structMeta, _) = self
                            .currentNamespace
                            .get_mut()
                            .findStruct(o.name.as_str())?;
                        let (t, _) = structMeta.findField(fieldName).unwrap();
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
            // FIXME check if cast is possible
            Expression::TypeCast(_, t) => Ok(t.clone())
        }
    }
}

#[derive(Debug)]
struct OpCodeManager<'a> {
    pub ops: &'a mut Vec<OpCode>,
}

impl OpCodeManager<'_> {
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(op)
    }
}

pub struct PartialExprCtx<'a> {
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub labelCounter: &'a mut usize,
    pub symbols: &'a SymbolManager
}

impl PartialExprCtx<'_> {
    #[inline(always)]
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(Op(op))
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

impl StatementCtx<'_> {
    pub fn specify(&mut self) -> PartialExprCtx {
        PartialExprCtx {
            ops: self.ops,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }
}

impl PartialExprCtx<'_> {
    pub fn constructCtx<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }
}

impl ExpressionCtx<'_> {
    pub fn reduce(&mut self) -> (&Expression, PartialExprCtx<'_>) {
        let p = PartialExprCtx {
            ops: self.ops,
            typeHint: self.typeHint.clone(),
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        };
        let e = self.exp;

        (e, p)
    }
}

#[derive(Debug)]
pub struct StatementCtx<'a> {
    pub statement: &'a Statement,
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub loopContext: Option<usize>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub handle: fn(&mut StatementCtx, DataType) -> (),
    pub labelCounter: &'a mut usize,
    pub symbols: &'a mut SymbolManager
}

impl StatementCtx<'_> {
    pub fn deflate(&mut self) -> SimpleCtx {
        SimpleCtx {
            ops: self.ops,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }

    pub fn transfer<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            loopContext: self.loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }

    pub fn beginLoop(&mut self) {
        self.symbols.enterScope();
        self.ops.push(SymbolicOpcode::LoopBegin)
    }

    pub fn endLoop(&mut self) {
        self.symbols.exitScope();
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

    pub fn registerVariable(&mut self, name: &str, t: DataType) {
        self.symbols.registerLocal(name, t)
    }

    pub fn registerVariableIfNotExists(&mut self, name: &str, t: DataType) {
        // FIXME
        self.symbols.registerLocal(name, t)
    }
}

impl ExpressionCtx<'_> {
    pub fn copy<'a>(&'a mut self, exp: &'a Expression) -> ExpressionCtx {
        ExpressionCtx {
            exp,
            ops: self.ops,
            typeHint: None,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
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
            typeHint,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }

    pub fn copy<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            loopContext: self.loopContext,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labelCounter: self.labelCounter,
            symbols: self.symbols,
        }
    }
}

pub fn genFunctionDef(
    fun: &FunctionMeta,
    ctx: &mut SimpleCtx,
) -> Result<(), CodeGenError> {
    if let FunctionTypeMeta::Runtime(body) = &fun.functionType {
        for statement in &body.statements {
            if DEBUG {
                println!("gening {:?}", statement)
            }
            let sCtx = ctx.inflate(&statement, None);
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
        Ok(())
    }
    else {
        panic!()
    }
}

pub fn genStatement(mut ctx: StatementCtx) -> Result<(), CodeGenError> {
    match ctx.statement {
        Statement::While(w) => {
            ctx.makeExpressionCtx(&w.exp, None)
                .toDataType()?
                .assertType(Bool)?;

            let loopEnd = ctx.nextLabel();

            ctx.beginLoop();

            genExpression(ctx.makeExpressionCtx(&w.exp, None))?;

            ctx.opJmp(loopEnd, False);

            w.body.generate(ctx.deflate())?;

            ctx.opContinue();

            ctx.makeLabel(loopEnd);
            ctx.endLoop();
        }
        Statement::If(flow) => {
            ctx.makeExpressionCtx(&flow.condition, None)
                .toDataType()?
                .assertType(Bool)?;

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
            } else {
                ctx.opJmp(endLabel, False)
            }

            flow.body.generate(ctx.deflate())?;

            ctx.opJmp(endLabel, JmpType::Jmp);

            for (i, els) in flow.elseIfs.iter().enumerate() {
                ctx.makeLabel(*ifLabels.get(i).unwrap());

                ctx.makeExpressionCtx(&els.0, None)
                    .toDataType()?
                    .assertType(Bool)?;
                ctx.makeExpressionCtx(&els.0, None).genExpression()?;

                if let Some(v) = ifLabels.get(i + 1) {
                    ctx.opJmp(*v, False);
                } else {
                    ctx.opJmp(endLabel, False);
                }

                els.1.generate(ctx.deflate())?;
                ctx.opJmp(endLabel, JmpType::Jmp);
            }

            if let Some(els) = &flow.elseBody {
                ctx.makeLabel(*ifLabels.last().unwrap());
                els.generate(ctx.deflate())?;
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
            ctx.beginLoop();

            body.generate(ctx.deflate())?;

            ctx.opContinue();
            ctx.endLoop();
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
            let typ = ctx.makeExpressionCtx(value, None).toDataType()?.assertNotVoid()?;

            match dest {
                Expression::Variable(v) => {
                    match ctx.specify().getVariable(v) {
                        Ok(_) => {}
                        Err(_) => {
                            ctx.registerVariable(v, typ)
                        }
                    }
                }
                Expression::ArrayIndexing(v) => {
                    ctx.makeExpressionCtx(&v.expr, None).genExpression()?;
                }
                Expression::NamespaceAccess(_) => {}
                Expression::FieldAccess(obj, _) => {
                    let mut cd = ctx.makeExpressionCtx(obj, None);

                    cd.genExpression()?;
                }
                _ => panic!(),
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
                        ArithmeticOp::Div => OpCode::Div(t),
                    };
                    ctx.push(o);
                }
            }

            match dest {
                Expression::Variable(v) => {
                    let var = ctx.specify().getVariable(v)?.1;

                    ctx.push(SetLocal { index: var })
                }
                Expression::ArrayIndexing(v) => {
                    ctx.makeExpressionCtx(&v.index, None).genExpression()?;

                    ctx.push(ArrayStore)
                }
                Expression::NamespaceAccess(v) => unsafe {
                    let namespace = (&mut *ctx.vm.get()).findNamespaceParts(&v[..v.len() - 1])?;
                    let global = namespace.0.findGlobal(v.last().unwrap())?;

                    ctx.push(SetGlobal {
                        namespaceID: namespace.1,
                        globalID: global.1,
                    })
                },
                Expression::FieldAccess(obj, field) => {
                    let mut cd = ctx.makeExpressionCtx(obj, None);
                    let class = match cd.toDataType()? {
                        Object(v) => v,
                        _ => panic!(),
                    };

                    let namespaceID = cd.currentNamespace.get_mut().id;

                    let (_, structID, _, fieldID) = cd
                        .currentNamespace
                        .get_mut()
                        .findStructField(&class.name, field)?;

                    ctx.push(SetField {
                        namespaceID,
                        structID,
                        fieldID,
                    })
                }
                _ => panic!(),
            }
        }
        Statement::ForLoop(var, arr, body) => {
            let varId = ctx.specify().getVariable(var)?.1;
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

            body.generate(ctx.deflate())?;

            ctx.opJmp(endLabel, True);

            ctx.push(PushIntOne);
            ctx.push(Add(DataType::Int));

            ctx.opContinue();

            ctx.endLoop();
            ctx.makeLabel(endLabel);
            ctx.push(Pop);
        }
        Statement::Repeat(var, count, body) => {
            let endLoop = ctx.nextLabel();
            let varId = ctx.specify().getVariable(var)?.1;

            ctx.beginLoop();

            ctx.push(GetLocal { index: varId });
            ctx.push(PushInt(*count as isize));
            ctx.push(Greater(Int));
            ctx.opJmp(endLoop, False);

            body.generate(ctx.deflate())?;

            ctx.push(OpCode::Inc {
                typ: Int,
                index: varId,
            });
            ctx.opContinue();

            ctx.endLoop();
            ctx.makeLabel(endLoop);
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
            Expression::IntLiteral(i) => r.push(PushInt(i.parse::<isize>().map_err(|_| LiteralParseError)?)),
            Expression::FloatLiteral(i) => r.push(OpCode::PushFloat(i.parse::<f64>().map_err(|_| LiteralParseError)?)),
            Expression::StringLiteral(i) => {
                r.push(StrNew(
                    (&mut *r.currentNamespace.get()).allocateOrGetString(i),
                ));
            },
            Expression::BoolLiteral(i) => r.push(OpCode::PushBool(*i)),
            Expression::Variable(v) => {
                match r.getVariable(v) {
                    Ok(v) => {
                        r.push(OpCode::GetLocal {
                            index: v.1,
                        });
                    }
                    Err(_) => {
                        let f = r.lookupFunctionByBaseName(v)?;
                        let a = (&mut *r.currentNamespace.get()).findFunction(&f).unwrap();
                        r.push(PushFunction(
                            (&*r.currentNamespace.get()).id as u32,
                            a.1 as u32,
                        ))
                    }
                }
            },
            Expression::CharLiteral(c) => r.push(PushChar(*c)),
            Expression::ArrayLiteral(i) => {
                let d = match r.typeHint {
                    None => Some(
                        r.constructCtx(i.first().ok_or(UntypedEmptyArray)?)
                            .toDataType()?
                            .assertNotVoid()?,
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

                r.push(OpCode::GetGlobal {
                    namespaceID: namespace.1,
                    globalID: global.1,
                })
            }
            Expression::Lambda(args, body, ret) => {
                let meta = FunctionMeta::makeRuntime(
                    "lambda".to_string(),
                    args.clone().into_boxed_slice(),
                    0,
                    ret.clone().unwrap_or(Void),
                    body.clone(),
                );

                // ctx.currentNamespace.registerFunctionDef()
            }
            Expression::Callable(prev, args) => unsafe {
                match &**prev {
                    Expression::Variable(v) => {
                        for arg in args {
                            r.constructCtx(arg).toDataType()?.assertNotVoid()?;
                            genExpression(r.constructCtx(arg))?;
                        }

                        if r.getVariable(v).is_err() {
                            let genName = genFunName(
                                &v,
                                &args
                                    .iter()
                                    .map(|it| r.constructCtx(it).toDataType().unwrap())
                                    .collect::<Vec<_>>(),
                            );
                            let funcId = r.symbols.getFunction(&genName)?;

                            let nn = r.currentNamespace.get_mut();
                            let nId = nn.id;


                            if funcId.2 == nId {
                                r.push(SCall { id: funcId.3 });
                            }
                            else {
                                r.push(LCall { namespace: funcId.2, id: funcId.3 });
                            }
                        } else {
                            genExpression(r.constructCtx(prev))?;
                            r.push(DynamicCall)
                        }
                    }
                    Expression::NamespaceAccess(v) => {
                        for arg in args {
                            r.constructCtx(arg).toDataType()?.assertNotVoid()?;
                            genExpression(r.constructCtx(arg))?;
                        }

                        let f = r.symbols.getFunctionPartsArgs(v, &args
                            .iter()
                            .map(|it| r.constructCtx(it).toDataType().unwrap())
                            .collect::<Vec<_>>())?;

                        r.push(LCall {
                            namespace: f.2,
                            id: f.3,
                        });
                    }
                    Expression::FieldAccess(v, name) => {
                        let prevT = r.constructCtx(v).toDataType()?.assertNotVoid()?;

                        if prevT.isFunction() {
                            for arg in args {
                                r.constructCtx(arg).toDataType()?.assertNotVoid()?;
                                genExpression(r.constructCtx(arg))?;
                            }

                            genExpression(r.constructCtx(prev))?;

                            r.push(DynamicCall);
                            return Ok(());
                        }

                        let mut argsB = vec![];

                        println!("fType: {:?}", prevT);
                        argsB.push(prevT);
                        argsB.extend(
                            args.iter()
                                .map(|it| r.constructCtx(it).toDataType().unwrap()),
                        );

                        r.constructCtx(v).genExpression()?;

                        for arg in args {
                            r.constructCtx(arg).toDataType()?.assertNotVoid()?;
                            genExpression(r.constructCtx(arg))?;
                        }

                        let n = r.currentNamespace.get_mut();
                        let nId = n.id;

                        let (_, fId) = n.lookupFunction(name, &argsB)?;

                        r.push(LCall {
                            namespace: nId,
                            id: fId,
                        })
                    }

                    _ => {
                        genExpression(r.constructCtx(prev))?;
                        r.push(DynamicCall)
                    }
                }
            },
            Expression::StructInit(name, init) => {
                // FIXME doesnt support other namespaces
                let (structMeta, structID) = (&*r.currentNamespace.get()).findStruct(name)?;
                let namespaceID = (&*r.currentNamespace.get()).id;

                r.push(New {
                    namespaceID,
                    structID,
                });

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
                                return Ok(());
                            } else if e.isString() {
                                r.push(StringLength);
                                return Ok(());
                            }
                        }

                        let (structMeta, structID, field, fieldID) =
                            (&*r.currentNamespace.get())
                                .findStructField(o.name.as_str(), fieldName)?;

                        r.push(GetField {
                            namespaceID: (&*r.currentNamespace.get()).id,
                            structID,
                            fieldID,
                        });
                    }
                    _ => panic!(),
                };
            }
            Expression::Null => ctx.push(OpCode::PushIntZero),
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
            Expression::TypeCast(exp, target) => {
                let src = r.constructCtx(exp).toDataType()?.assertNotVoid()?;

                if target.isValue() || src.isValue() {
                    r.constructCtx(exp).genExpression()?;
                    return Ok(())
                }

                if &src == target {
                    r.constructCtx(exp).genExpression()?;
                    return Ok(())
                }

                if src.isInt() && target.isFloat() {
                    r.constructCtx(exp).genExpression()?;
                    r.push(I2F);
                    return Ok(())
                }

                if target.isInt() && src.isFloat() {
                    r.constructCtx(exp).genExpression()?;
                    r.push(F2I);
                    return Ok(())
                }

                todo!()
            }
        }
    }
    Ok(())
}
