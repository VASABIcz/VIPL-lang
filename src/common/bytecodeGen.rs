use core::slice::sort::quicksort;
use std::cell::UnsafeCell;
use std::collections::HashMap;

use libc::open;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Deref;

use crate::ast::{ArithmeticOp, BinaryOp, RawExpression, FunctionDef, ModType, RawNode, RawStatement, StructDef, Statement, Expression, ASTNode};
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::errors::CodeGenError::{LiteralParseError, UnexpectedVoid, UntypedEmptyArray};
use crate::errors::{
    CodeGenError, InvalidTypeException, NoValue, SymbolNotFoundE, SymbolType, TypeError,
    TypeNotFound,
};
use crate::lexer::*;
use crate::lexingUnits::TokenType::In;
use crate::parser::*;
use crate::symbolManager::SymbolManager;
use crate::utils::genFunName;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int, Object, Value, Void};
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
    Jmp(usize, JmpType),
    LoopLabel(usize),
}

#[derive(Debug, Clone)]
pub struct Body {
    statements: Vec<Statement>,
}

impl Body {
    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }
}

#[derive(Debug, Default, Clone)]
pub struct LabelManager {
    labelCounter: usize,
    loopContext: Vec<(usize, usize)>
}

impl LabelManager {
    pub fn nextLabel(&mut self) -> usize {
        let l = self.labelCounter;

        self.labelCounter += 1;

        l
    }

    pub fn enterLoop(&mut self) -> (usize, usize) {
        let start = self.nextLabel();
        let end = self.nextLabel();

        self.loopContext.push((start, end));

        (start, end)
    }

    pub fn exitLoop(&mut self) {
        self.loopContext.pop();
    }

    pub fn getContext(&self) -> Option<(usize, usize)> {
        self.loopContext.last().map(|it| (it.0, it.1))
    }
}

pub struct SimpleCtx<'a> {
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub handle: fn(&mut StatementCtx, DataType) -> (),
    pub labels: &'a mut LabelManager,
    pub symbols: &'a mut SymbolManager
}

impl SimpleCtx<'_> {
    pub fn inflate<'a>(
        &'a mut self,
        statement: &'a Statement
    ) -> StatementCtx<'a> {
        StatementCtx {
            statement: &statement,
            ops: self.ops,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labels: self.labels,
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
            let y = ctx.inflate(statement);
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

pub fn emitOpcodes(syms: Vec<SymbolicOpcode>) -> Result<Vec<OpCode>, CodeGenError> {
    let mut buf = vec![];
    let mut labelLookup = HashMap::new();

    let mut counter = 0usize;
    for sym in &syms {
        match sym {
            SymbolicOpcode::Op(_)
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
            SymbolicOpcode::Jmp(id, t) => {
                let i = *labelLookup.get(&id).ok_or_else(||CodeGenError::VeryBadState)?;
                buf.push(OpCode::Jmp {
                    offset: i as i32 - (buf.len() + 1) as i32,
                    jmpType: *t,
                })
            }
            SymbolicOpcode::LoopLabel(_) => {}
        }
    }

    Ok(buf)
}

#[derive(Debug)]
pub struct ExpressionCtx<'a> {
    pub exp: &'a Expression,
    pub ops: &'a mut Vec<SymbolicOpcode>,
    pub typeHint: Option<DataType>,
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub labelCounter: &'a mut LabelManager,
    pub symbols: &'a SymbolManager
}

impl ExpressionCtx<'_> {
    pub fn assertType(&mut self, t: DataType) -> Result<(), CodeGenError> {
        let t1 = self.toDataType()?;

        if t != t1 {
            return Err(CodeGenError::TypeError(TypeError{
                expected: t.clone(),
                actual: t1.clone(),
                exp: Some(self.exp.clone()),
            }))
        }

        Ok(())
    }

    pub fn getVariable(&self, varName: &str) -> Result<&(DataType, usize), CodeGenError> {
        self.symbols.getLocal(varName)
        // self.vTable.get(varName).ok_or_else(||CodeGenError::SymbolNotFound(SymbolNotFoundE::var(varName)))
    }

    pub fn genExpression(mut self) -> Result<(), CodeGenError> {
        genExpression(self)
    }
}

impl PartialExprCtx<'_> {
    pub fn lookupFunctionByBaseName(&mut self, name: &str) -> Result<String, CodeGenError> {
        for (k, v) in &self.symbols.functions {
            if k.as_str().starts_with(&name) {
                return Ok(k.to_string());
            }
        }
        return Err(CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)));
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
        let modName = format!("{}(", name);

        for k in self.symbols.functions.keys() {
            if k.as_str().starts_with(&modName) {
                return Ok(k.to_string());
            }
        }
        Err(CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)))
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

    pub fn toDataTypeNotVoid(&mut self) -> Result<DataType, CodeGenError> {
        let t = self.toDataType()?;

        if t.isVoid() {
            return Err(UnexpectedVoid(ASTNode::Expr(self.exp.clone())));
        }

        Ok(t)
    }

    pub fn toDataType(&mut self) -> Result<DataType, CodeGenError> {
        match &(self.exp).exp {
            RawExpression::BinaryOperation {
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
            RawExpression::IntLiteral(_) => Ok(DataType::Int),
            RawExpression::FloatLiteral(_) => Ok(DataType::Float),
            RawExpression::StringLiteral(_) => Ok(DataType::str()),
            RawExpression::Variable(name) => match self.getVariable(name) {
                Err(_) => {
                    let funcName = self.lookupFunctionByBaseName(name)?;
                    let f = self
                        .currentNamespace
                        .get_mut()
                        .findFunction(&funcName)?;

                    println!("THE FUCK {:?}", f);

                    Ok(f.0.toFunctionType())
                }
                Ok(v) => {
                    println!("found var {}", name);
                    Ok(v.0.clone())
                },
            },
            RawExpression::BoolLiteral(_) => Ok(DataType::Bool),
            RawExpression::CharLiteral(_) => Ok(Char),
            RawExpression::ArrayLiteral(e) => {
                if e.is_empty() {
                    let l = &self.typeHint;
                    let c = l.clone().ok_or_else(||CodeGenError::UntypedEmptyArray)?;
                    match c {
                        Object(o) => {
                            if o.name.as_str() == "Array" {
                                let e = o
                                    .generics
                                    .first()
                                    .ok_or_else(||CodeGenError::ArrayWithoutGenericParameter)?;
                                Ok(DataType::arr(e.clone()))
                            } else {
                                Err(CodeGenError::TypeError(TypeError {
                                    expected: DataType::Object(ObjectMeta {
                                        name: "Array".to_string(),
                                        generics: Box::new([Any]),
                                    }),
                                    actual: Object(o.clone()),
                                    exp: Some(e.first().ok_or_else(||CodeGenError::VeryBadState)?.clone()),
                                }))
                            }
                        }
                        v => Err(CodeGenError::TypeError(TypeError {
                            expected: DataType::arr(Any),
                            actual: v.clone(),
                            exp: Some(e.first().ok_or_else(||CodeGenError::VeryBadState)?.clone()),
                        })),
                    }
                } else {
                    let t = self
                        .transfer(e.get(0).ok_or_else(||CodeGenError::UntypedEmptyArray)?)
                        .toDataTypeNotVoid()?;

                    Ok(DataType::arr(Generic::Type(t)))
                }
            }
            RawExpression::ArrayIndexing(i) => {
                let e = self.transfer(&i.expr).toDataTypeNotVoid()?;

                match e {
                    Object(o) => {
                        if o.name.as_str() == "String" {
                            return Ok(Char);
                        }
                        Ok(o.generics
                            .first()
                            .ok_or_else(||CodeGenError::UntypedEmptyArray)?
                            .clone()
                            .ok_or_else(||CodeGenError::UntypedEmptyArray)?)
                    }
                    _ => {
                        Err(CodeGenError::ExpectedReference)
                    },
                }
            }
            RawExpression::NotExpression(i, _) => {
                self.transfer(i).assertType(Bool)?;

                Ok(Bool)
            }
            RawExpression::NamespaceAccess(n) => {
                let (namespace, _) =
                    self.vm.get_mut().findNamespaceParts(&n[..n.len() - 1])?;

                let global = namespace.findGlobal(n.last().ok_or_else(||CodeGenError::VeryBadState)?)?;

                Ok(global.0.typ.clone())
            }
            RawExpression::Lambda(l, _, ret) => {
                Ok(DataType::Function {
                    args: l.iter().map(|it| it.typ.clone()).collect(),
                    ret: Box::new(ret.clone().unwrap_or(Void)),
                })
            }
            RawExpression::Callable(prev, args) => unsafe {
                match &prev.exp {
                    RawExpression::Variable(v) => {
                        if self.getVariable(v).is_err() {
                            let genName = genFunName(
                                &v,
                                &args
                                    .iter()
                                    .map(|it| self.transfer(it).toDataType())
                                    .collect::<Result<Vec<_>, _>>()?,
                            );

                            let funcId = self.symbols.getFunction(&genName)?;
                            return Ok(funcId.1.clone());
                        }


                        let var = self.getVariable(v)?;
                        match &var.0 {
                            DataType::Function { ret, .. } => Ok(*ret.clone()),
                            _ => Err(CodeGenError::ExpectedLambda),
                        }
                    }
                    RawExpression::NamespaceAccess(v) => {
                        let argz = args
                            .iter()
                            .map(|it| self.transfer(it).toDataType())
                            .collect::<Result<Vec<_>, _>>()?;

                        Ok(self.symbols.getFunctionPartsArgs(&v, &argz)?.1.clone())
                    }
                    RawExpression::FieldAccess(a, b) => {
                        let t = self.transfer(a).toDataType()?;

                        match t {
                            DataType::Function { ret, .. } => Ok(*ret.clone()),
                            t => {
                                let mut fArgs = vec![];
                                fArgs.push(t);
                                fArgs.extend(
                                    args.iter()
                                        .map(|it| self.transfer(it).toDataType()).collect::<Result<Vec<_>, _>>()?,
                                );

                                let f =
                                    self.currentNamespace.get_mut().lookupFunction(b, &fArgs)?;

                                Ok(f.0.returnType.clone())
                            }
                        }
                    }
                    _ => {
                        return Err(CodeGenError::ExpectedCallable)
                    },
                }
            },
            RawExpression::StructInit(name, _) => Ok(DataType::Object(ObjectMeta {
                name: name.clone().into(),
                generics: Box::new([]),
            })),
            RawExpression::FieldAccess(prev, fieldName) => {
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
                        let (t, _) = structMeta.findField(fieldName)?;
                        Ok(t.typ.clone())
                    }
                    _ => {
                        Err(CodeGenError::ExpectedReference)
                    },
                }
            }
            RawExpression::Null => Ok(self.typeHint.clone().unwrap_or(Int)),
            RawExpression::TernaryOperator(cond, tr, fal) => {
                let b = self.transfer(&fal).toDataType()?;
                self.transfer(&tr).assertType(b.clone())?;
                self.transfer(&cond).assertType(Bool)?;

                Ok(b)
            }
            // FIXME check if cast is possible
            RawExpression::TypeCast(_, t) => Ok(t.clone())
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
    pub labelCounter: &'a mut LabelManager,
    pub symbols: &'a SymbolManager
}

impl PartialExprCtx<'_> {
    #[inline(always)]
    pub fn push(&mut self, op: OpCode) {
        self.ops.push(Op(op))
    }

    pub fn beginLoop(&mut self) -> (usize, usize) {
        let (start, end) = self.labelCounter.enterLoop();
        self.makeLabel(start);

        (start, end)
    }

    pub fn endLoop(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.labelCounter.getContext().ok_or_else(|| CodeGenError::VeryBadState)?;

        self.makeLabel(ctx.1);
        self.labelCounter.exitLoop();

        Ok(())
    }

    pub fn nextLabel(&mut self) -> usize {
        self.labelCounter.nextLabel()
    }

    pub fn makeLabel(&mut self, id: usize) {
        self.ops.push(SymbolicOpcode::LoopLabel(id));
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
            labelCounter: self.labels,
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
    pub currentNamespace: &'a mut UnsafeCell<Namespace>,
    pub vm: &'a mut UnsafeCell<VirtualMachine>,
    pub handle: fn(&mut StatementCtx, DataType) -> (),
    pub labels: &'a mut LabelManager,
    pub symbols: &'a mut SymbolManager
}

impl StatementCtx<'_> {
    pub fn deflate(&mut self) -> SimpleCtx {
        SimpleCtx {
            ops: self.ops,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labels: self.labels,
            symbols: self.symbols,
        }
    }

    pub fn transfer<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labels: self.labels,
            symbols: self.symbols,
        }
    }

    pub fn beginLoop(&mut self) -> (usize, usize) {
        let (start, end) = self.labels.enterLoop();
        self.makeLabel(start);

        (start, end)
    }

    pub fn endLoop(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.labels.getContext().ok_or_else(|| CodeGenError::VeryBadState)?;

        self.makeLabel(ctx.1);
        self.labels.exitLoop();

        Ok(())
    }

    pub fn opContinue(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.labels.getContext().ok_or_else(|| CodeGenError::ContinueOutsideLoop(self.statement.clone()))?;

        self.opJmp(ctx.0, JmpType::Jmp);

        Ok(())
    }

    pub fn opBreak(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.labels.getContext().ok_or_else(|| CodeGenError::BreakOutsideLoop(self.statement.clone()))?;

        self.opJmp(ctx.1, JmpType::Jmp);

        Ok(())
    }

    pub fn nextLabel(&mut self) -> usize {
        self.labels.nextLabel()
    }

    pub fn makeLabel(&mut self, id: usize) {
        self.ops.push(SymbolicOpcode::LoopLabel(id));
    }

    pub fn opJmp(&mut self, label: usize, jmpType: JmpType) {
        self.ops.push(SymbolicOpcode::Jmp(label, jmpType))
    }

    pub fn registerVariable(&mut self, name: &str, t: DataType) {
        self.symbols.registerLocal(name, t);
    }

    pub fn registerVariableIfNotExists(&mut self, name: &str, t: DataType) -> (DataType, usize) {
        if let Ok(v) = self.symbols.getLocal(name) {
            return v.clone();
        }
        else {
            self.symbols.registerLocal(name, t)
        }
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
            labelCounter: self.labels,
            symbols: self.symbols,
        }
    }

    pub fn copy<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx {
        StatementCtx {
            statement,
            ops: self.ops,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labels: self.labels,
            symbols: self.symbols,
        }
    }
}

pub fn genFunctionDef(
    fun: &FunctionMeta,
    ctx: &mut SimpleCtx,
) -> Result<(), Vec<CodeGenError>> {
    let mut errors = vec![];

    if let FunctionTypeMeta::Runtime(body) = &fun.functionType {
        for statement in &body.statements {
            if DEBUG {
                println!("gening {:?}", statement)
            }
            let sCtx = ctx.inflate(&statement);
            if let Err(e) = genStatement(sCtx) {
                errors.push(e)
            }
        }
        if !errors.is_empty() {
            return Err(errors)
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
        println!("this should never get called :3");
        Err(vec![CodeGenError::VeryBadState])
    }
}

pub fn genStatement(mut ctx: StatementCtx) -> Result<(), CodeGenError> {
    match &ctx.statement.exp {
        RawStatement::While(w) => {
            ctx.makeExpressionCtx(&w.exp, None)
                .assertType(Bool)?;

            let (loopStart, loopEnd) = ctx.beginLoop();

            genExpression(ctx.makeExpressionCtx(&w.exp, None))?;

            ctx.opJmp(loopEnd, False);

            w.body.generate(ctx.deflate())?;

            ctx.opContinue()?;

            ctx.endLoop()?;
        }
        RawStatement::If(flow) => {
            ctx.makeExpressionCtx(&flow.condition, None)
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
                ctx.makeLabel(*ifLabels.get(i).ok_or_else(||CodeGenError::VeryBadState)?);

                ctx.makeExpressionCtx(&els.0, None)
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
                ctx.makeLabel(*ifLabels.last().ok_or_else(||CodeGenError::VeryBadState)?);
                els.generate(ctx.deflate())?;
            }
            ctx.makeLabel(endLabel);
        }
        RawStatement::Return(ret) => {
            genExpression(ctx.makeExpressionCtx(&ret, None))?;
            ctx.push(Return)
        }
        RawStatement::Continue => ctx.opContinue()?,
        RawStatement::Break => ctx.opBreak()?,
        RawStatement::Loop(body) => {
            ctx.beginLoop();

            body.generate(ctx.deflate())?;

            ctx.opContinue()?;
            ctx.endLoop()?;
        }
        RawStatement::StatementExpression(v) => {
            let mut eCtx = ctx.makeExpressionCtx(v, None);

            let ret = eCtx.toDataType()?;

            genExpression(eCtx)?;

            if ret != Void {
                (ctx.handle)(&mut ctx, ret);
            }
        }
        RawStatement::Assignable(dest, value, t) => {
            println!("dest: {:?}", dest.exp);

            let a = ctx.makeExpressionCtx(dest, None).toDataType();

            println!("a: {:?}", a);

            let b = ctx.makeExpressionCtx(value, None).toDataType()?;

            if let Ok(t) = a {
                if t != b || t.isVoid() || b.isVoid() {
                    return Err(CodeGenError::AssignableTypeError{
                        var: dest.clone(),
                        varType: t,
                        exp: value.clone(),
                        expType: b,
                    })
                }
            }

            ctx.makeExpressionCtx(value, None).toDataTypeNotVoid()?;

            match &dest.exp {
                RawExpression::Variable(v) => {
                    match ctx.specify().getVariable(v) {
                        Ok(v) => {
                            let d = v.0.clone();
                            ctx.makeExpressionCtx(value, None).assertType(d)?;
                        }
                        Err(_) => {
                            let t = ctx.makeExpressionCtx(value, None).toDataType()?;
                            ctx.registerVariable(v, t)
                        }
                    }
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.makeExpressionCtx(&v.expr, None).genExpression()?;
                }
                RawExpression::NamespaceAccess(_) => {}
                RawExpression::FieldAccess(obj, _) => {
                    ctx.makeExpressionCtx(obj, None).genExpression()?;
                }
                _ => {
                    return Err(CodeGenError::ExpressionIsNotAssignable)
                },
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
                        ArithmeticOp::Add => OpCode::Add(t.toRawType()?),
                        ArithmeticOp::Sub => OpCode::Sub(t.toRawType()?),
                        ArithmeticOp::Mul => OpCode::Mul(t.toRawType()?),
                        ArithmeticOp::Div => OpCode::Div(t.toRawType()?),
                    };
                    ctx.push(o);
                }
            }

            match &dest.exp {
                RawExpression::Variable(v) => {
                    let var = ctx.specify().getVariable(v)?.1;

                    ctx.push(SetLocal { index: var })
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.makeExpressionCtx(&v.index, None).genExpression()?;

                    ctx.push(ArrayStore)
                }
                RawExpression::NamespaceAccess(v) => unsafe {
                    let namespace = (*ctx.vm.get()).findNamespaceParts(&v[..v.len() - 1])?;
                    let global = namespace.0.findGlobal(v.last().ok_or_else(||CodeGenError::VeryBadState)?)?;

                    ctx.push(SetGlobal {
                        namespaceID: namespace.1 as u32,
                        globalID: global.1 as u32,
                    })
                },
                RawExpression::FieldAccess(obj, field) => {
                    let mut cd = ctx.makeExpressionCtx(obj, None);

                    let class = cd.toDataType()?.getRef()?;

                    let (_, structID, _, fieldID) = cd
                        .currentNamespace
                        .get_mut()
                        .findStructField(&class.name, field)?;

                    ctx.push(SetField {
                        fieldID,
                    })
                }
                _ => {
                    return Err(CodeGenError::ExpressionIsNotAssignable)
                },
            }
        }
        RawStatement::ForLoop(var, arr, body) => {
            let startLabel = ctx.nextLabel();

            let t = ctx.makeExpressionCtx(arr, None).toDataTypeNotVoid()?.getArrayType()?;

            // iteration counter
            ctx.push(PushIntZero);

            ctx.opJmp(startLabel, JmpType::Jmp);

            let (_, endLabel) = ctx.beginLoop();

            ctx.push(PushIntOne);
            ctx.push(Add(Int.toRawType()?));

            ctx.makeLabel(startLabel);

            ctx.registerVariable(var, t);
            let varId = ctx.specify().getVariable(var)?.1;

            ctx.push(Dup);

            ctx.makeExpressionCtx(arr, None).genExpression()?;

            // dec array length bcs less-eq is not implemented
            ctx.push(ArrayLength);
            ctx.push(PushIntOne);
            ctx.push(Sub(Int.toRawType()?));

            ctx.push(Less(Int.toRawType()?));

            ctx.opJmp(endLabel, True);

            ctx.push(Dup);
            ctx.makeExpressionCtx(arr, None).genExpression()?;
            ctx.push(Swap);

            ctx.push(ArrayLoad);
            ctx.push(SetLocal { index: varId });

            body.generate(ctx.deflate())?;

            ctx.opContinue();

            ctx.endLoop();
            ctx.makeLabel(endLabel);
            ctx.push(Pop);
        }
        RawStatement::Repeat(var, count, body) => {
            let loopStart = ctx.nextLabel();

            ctx.opJmp(loopStart, JmpType::Jmp);
            let (_, endLoop) = ctx.beginLoop();
            let (_, varId) = ctx.registerVariableIfNotExists(var, DataType::Int);

            ctx.push(OpCode::Inc {
                typ: Int.toRawType()?,
                index: varId as u32,
            });
            ctx.makeLabel(loopStart);

            ctx.push(GetLocal { index: varId });
            ctx.push(PushInt(*count as isize));
            ctx.push(Greater(Int.toRawType()?));
            ctx.opJmp(endLoop, False);

            body.generate(ctx.deflate())?;

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
        match &e.exp {
            RawExpression::BinaryOperation { left, right, op } => {
                let dat = r.constructCtx(left).toDataTypeNotVoid()?;
                genExpression(r.constructCtx(left))?;
                genExpression(r.constructCtx(right))?;
                let t = match op {
                    BinaryOp::Add => OpCode::Add(dat.toRawType()?),
                    BinaryOp::Sub => OpCode::Sub(dat.toRawType()?),
                    BinaryOp::Mul => OpCode::Mul(dat.toRawType()?),
                    BinaryOp::Div => OpCode::Div(dat.toRawType()?),
                    BinaryOp::Gt => OpCode::Greater(dat.toRawType()?),
                    BinaryOp::Less => OpCode::Less(dat.toRawType()?),
                    BinaryOp::Eq => OpCode::Equals(dat.toRawType()?),
                    BinaryOp::And => OpCode::And,
                    BinaryOp::Or => OpCode::Or,
                };
                r.push(t);
            }
            RawExpression::IntLiteral(i) => r.push(PushInt(i.parse::<isize>().map_err(|_| LiteralParseError)?)),
            RawExpression::FloatLiteral(i) => r.push(OpCode::PushFloat(i.parse::<f64>().map_err(|_| LiteralParseError)?)),
            RawExpression::StringLiteral(i) => {
                r.push(StrNew(
                    (*r.currentNamespace.get()).allocateOrGetString(i.strip_prefix('\"').ok_or_else(||CodeGenError::VeryBadState)?.strip_suffix('\"').ok_or_else(||CodeGenError::VeryBadState)?),
                ));
            },
            RawExpression::BoolLiteral(i) => r.push(OpCode::PushBool(*i)),
            RawExpression::Variable(v) => {
                match r.getVariable(v) {
                    Ok(v) => {
                        r.push(OpCode::GetLocal {
                            index: v.1,
                        });
                    }
                    Err(_) => {
                        let f = r.lookupFunctionByBaseName(v)?;
                        let a = (&mut *r.currentNamespace.get()).findFunction(&f)?;
                        r.push(PushFunction(
                            (&*r.currentNamespace.get()).id as u32,
                            a.1 as u32,
                        ))
                    }
                }
            },
            RawExpression::CharLiteral(c) => r.push(PushChar(*c)),
            RawExpression::ArrayLiteral(i) => {
                let d = match r.typeHint {
                    None => Some(
                        r.constructCtx(i.first().ok_or_else(||UntypedEmptyArray)?)
                            .toDataTypeNotVoid()?,
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
                r.push(PushInt(i.len() as isize));
                r.push(ArrayNew);
                for (ind, exp) in i.iter().enumerate() {
                    r.push(Dup);
                    genExpression(r.constructCtx(exp))?;
                    r.push(PushInt(ind as isize));
                    r.push(ArrayStore);
                }
            }
            RawExpression::ArrayIndexing(i) => {
                let d = r.constructCtx(&i.expr).toDataTypeNotVoid()?;

                if d.isString() {
                    genExpression(r.constructCtx(&i.expr))?;
                    genExpression(r.constructCtx(&i.index))?;
                    r.push(GetChar);
                    return Ok(())
                }
                else if d.isArray() {
                    genExpression(r.constructCtx(&i.expr))?;
                    genExpression(r.constructCtx(&i.index))?;

                    r.push(ArrayLoad);

                    return Ok(())
                }

                return Err(CodeGenError::ExpectedReference)
            }
            RawExpression::NotExpression(e, _) => {
                genExpression(r.constructCtx(e))?;
                r.push(Not)
            }
            RawExpression::NamespaceAccess(parts) => {
                let namespace = (&*r.vm.get()).findNamespaceParts(&parts[..parts.len() - 1])?;

                let global = namespace.0.findGlobal(parts.last().ok_or_else(||CodeGenError::VeryBadState)?)?;

                r.push(OpCode::GetGlobal {
                    namespaceID: namespace.1 as u32,
                    globalID: global.1 as u32,
                })
            }
            RawExpression::Lambda(args, body, ret) => {
                let meta = FunctionMeta::makeRuntime(
                    "lambda".to_string(),
                    args.clone().into_boxed_slice(),
                    0,
                    ret.clone().unwrap_or(Void),
                    body.clone(),
                );

                // ctx.currentNamespace.registerFunctionDef()
            }
            RawExpression::Callable(prev, args) => unsafe {
                match &prev.exp {
                    RawExpression::Variable(v) => {
                        for arg in args {
                            r.constructCtx(arg).toDataTypeNotVoid()?;
                            genExpression(r.constructCtx(arg))?;
                        }

                        if r.getVariable(v).is_err() {
                            let genName = genFunName(
                                &v,
                                &args
                                    .iter()
                                    .map(|it| r.constructCtx(it).toDataType())
                                    .collect::<Result<Vec<_>, _>>()?,
                            );
                            let funcId = r.symbols.getFunction(&genName)?;

                            let nn = r.currentNamespace.get_mut();
                            let nId = nn.id;


                            if funcId.2 == nId {
                                r.push(SCall { id: funcId.3 });
                            }
                            else {
                                r.push(LCall { namespace: funcId.2 as u32, id: funcId.3 as u32 });
                            }
                        } else {
                            let (args, prevT) = r.constructCtx(prev).toDataType()?.getFunction()?;

                            genExpression(r.constructCtx(prev))?;
                            r.push(DynamicCall(!prevT.isVoid(), args.len()))
                        }
                    }
                    RawExpression::NamespaceAccess(v) => {
                        for arg in args {
                            r.constructCtx(arg).toDataTypeNotVoid()?;
                            genExpression(r.constructCtx(arg))?;
                        }

                        let f = r.symbols.getFunctionPartsArgs(v, &args
                            .iter()
                            .map(|it| r.constructCtx(it).toDataType())
                            .collect::<Result<Vec<_>, _>>()?)?;

                        r.push(LCall {
                            namespace: f.2 as u32,
                            id: f.3 as u32,
                        });
                    }
                    RawExpression::FieldAccess(v, name) => {
                        let prevT = r.constructCtx(v).toDataTypeNotVoid()?;

                        if prevT.isFunction() {
                            let (argz, f) = prevT.getFunction()?;

                            for arg in args {
                                r.constructCtx(arg).toDataTypeNotVoid()?;
                                genExpression(r.constructCtx(arg))?;
                            }

                            genExpression(r.constructCtx(prev))?;

                            r.push(DynamicCall(!f.isVoid(), argz.len()));
                            return Ok(());
                        }

                        let mut argsB = vec![];

                        println!("fType: {:?}", prevT);
                        argsB.push(prevT);
                        argsB.extend(
                            args.iter()
                                .map(|it| r.constructCtx(it).toDataType()).collect::<Result<Vec<_>, _>>()?,
                        );

                        r.constructCtx(v).genExpression()?;

                        for arg in args {
                            r.constructCtx(arg).toDataTypeNotVoid()?;
                            genExpression(r.constructCtx(arg))?;
                        }

                        let n = r.currentNamespace.get_mut();
                        let nId = n.id;

                        let (_, fId) = n.lookupFunction(name, &argsB)?;

                        r.push(LCall {
                            namespace: nId as u32,
                            id: fId as u32,
                        })
                    }
                    _ => {
                        return Err(CodeGenError::ExpressionIsNotAssignable)
                    }
                }
            },
            RawExpression::StructInit(name, init) => {
                // FIXME doesnt support other namespaces
                let (structMeta, structID) = (&*r.currentNamespace.get()).findStruct(name)?;
                let namespaceID = (&*r.currentNamespace.get()).id;

                r.push(New {
                    namespaceID: namespaceID as u32,
                    structID: structID as u32,
                });

                for (fieldName, value) in init {
                    r.push(Dup);
                    let (_, fieldID) = structMeta.findField(fieldName)?;
                    let ctx = r.constructCtx(value);
                    genExpression(ctx)?;
                    r.push(SetField {
                        fieldID,
                    })
                }
            }
            RawExpression::FieldAccess(prev, fieldName) => {
                let ctx = r.constructCtx(prev);
                genExpression(ctx)?;

                let e = r.constructCtx(prev).toDataTypeNotVoid()?;


                if fieldName == "size" || fieldName == "length" {
                    if e.isArray() {
                        r.push(ArrayLength);
                        return Ok(());
                    } else if e.isString() {
                        r.push(StringLength);
                        return Ok(());
                    }
                }
                let o = e.getRef()?;

                let (_, _, _, fieldID) =
                    (*r.currentNamespace.get())
                        .findStructField(o.name.as_str(), fieldName)?;

                r.push(GetField {
                    fieldID,
                });
            }
            RawExpression::Null => ctx.push(OpCode::PushIntZero),
            RawExpression::TernaryOperator(cond, tr, fal) => {
                let a = r.constructCtx(&tr).toDataType()?;
                r.constructCtx(&fal).assertType(a.clone())?;
                r.constructCtx(&cond).assertType(Bool)?;

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
            RawExpression::TypeCast(exp, target) => {
                let src = r.constructCtx(exp).toDataTypeNotVoid()?;

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
