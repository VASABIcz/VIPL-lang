use core::slice::sort::quicksort;
use std::cell::UnsafeCell;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Deref;
use std::ptr::null;

use libc::open;

use crate::ast::{ArithmeticOp, ASTNode, BinaryOp, Expression, FunctionDef, ModType, RawExpression, RawNode, RawStatement, Statement, StructDef};
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::codeGenCtx::{Body, ExpressionCtx, SimpleCtx, StatementCtx};
use crate::errors::{
    CodeGenError, InvalidTypeException, NoValue, SymbolNotFoundE, SymbolType, TypeError,
    TypeNotFound,
};
use crate::errors::CodeGenError::{LiteralParseError, UnexpectedVoid, UntypedEmptyArray};
use crate::lexer::*;
use crate::lexingUnits::TokenType::In;
use crate::parser::*;
use crate::symbolManager::{FunctionSymbol, SymbolManager};
use crate::utils::{genFunName, microsSinceEpoch};
use crate::vm::dataType::{DataType, Generic, ObjectMeta, RawDataType};
use crate::vm::dataType::DataType::{Bool, Char, Float, Int, Null, Reference, Value, Void};
use crate::vm::dataType::Generic::Any;
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta, Namespace};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::{JmpType, OpCode, VirtualMachine};
use crate::vm::vm::JmpType::{False, True};
use crate::vm::vm::OpCode::{Add, And, ArrayLength, ArrayLoad, ArrayNew, ArrayStore, Div, Dup, DynamicCall, F2I, GetChar, GetField, GetLocal, Greater, I2F, IsStruct, Jmp, LCall, Less, Mul, New, Not, Pop, PushBool, PushChar, PushFloat, PushFunction, PushInt, PushIntOne, PushIntZero, Return, SCall, SetField, SetGlobal, SetLocal, StringLength, StrNew, Sub, Swap};

const DEBUG: bool = false;

impl Body {
    pub fn generate(
        &self,
        mut ctx: SimpleCtx<SymbolicOpcode>,
    ) -> Result<(), CodeGenError> {
        ctx.enterScope();
        for statement in &self.statements {
            let y = ctx.inflate(statement);
            genStatement(y)?;
        }
        ctx.exitScope();

        Ok(())
    }

    pub fn validate(&self, mut ctx: SimpleCtx<SymbolicOpcode>, fun: &FunctionMeta) -> Result<(), CodeGenError> {
        let mut s = HashSet::new();

        let isCovered = self.coverageCheck(ctx.transfer(), &mut s)?;

        if fun.returnType.isVoid() && s.is_empty() {
            return Ok(())
        }

        if !isCovered || s.len() == 0 {
            return Err(CodeGenError::InvalidReturns)
        }

        if s.len() == 2 && s.contains(&Null) {
            let t = s.iter().find(|it| !it.isNull()).unwrap().clone();
            if t.isReference() && fun.returnType.isNullable() {
                return Ok(())
            }
        }

        if s.len() > 1 {
            return Err(CodeGenError::InvalidReturns)
        }

        let tt = s.iter().next().unwrap();

        if fun.returnType.isNullable() && tt.isNull() {
            return Ok(())
        }

        if let Ok(v) = tt.clone().getRef() && let Ok(v1) = fun.returnType.clone().getRef() && v.name == v1.name {
            if v.nullable && !v1.nullable {
                return Err(CodeGenError::InvalidReturns)
            }
            else {
                return Ok(())
            }
        }

        if *tt != fun.returnType {
            return Err(CodeGenError::TypeError(TypeError::newNone(fun.returnType.clone(), tt.clone())))
        }


        Ok(())
    }

    pub fn coverageCheck(&self, mut ctx: SimpleCtx<SymbolicOpcode>, r: &mut HashSet<DataType>) -> Result<bool, CodeGenError> {
        for s1 in &self.statements {
            let s = &s1.exp;
            match s {
                RawStatement::If(a) => {
                    let mut covered = a.body.coverageCheck(ctx.transfer(), r)?;

                    if let Some(v) = &a.elseBody {
                        covered &= v.coverageCheck(ctx.transfer(), r)?;
                    }

                    for (_, b) in &a.elseIfs {
                        covered &= b.coverageCheck(ctx.transfer(), r)?;
                    }

                    if covered {
                        return Ok(true)
                    }
                }
                RawStatement::Return(e) => {
                    let t = ctx.inflate(s1).ctx.makeExpressionCtx(e, None).toDataType()?;
                    r.insert(t);

                    return Ok(true)
                }
                RawStatement::Loop(b) => {
                    b.returnType(ctx.transfer(), r)?
                }
                RawStatement::StatementExpression(_) => {}
                RawStatement::Assignable(a, b, c, typeHint) => {
                    if let RawExpression::Variable(v) = &a.exp {
                        let mut ctx1 = ctx.inflate(s1);
                        let t = ctx1.ctx.makeExpressionCtx(b, None).toDataTypeNotVoid()?;

                        if let Some(Reference(r)) = typeHint && r.nullable && t.isNull() {
                            ctx1.ctx.registerVariable(v, DataType::Reference(r.clone()));
                        }
                        else {
                            ctx1.ctx.registerVariable(v, t);
                        }
                    }
                }
                RawStatement::ForLoop(name, e, b) => {
                    let mut ctx1 = ctx.inflate(s1);

                    ctx1.ctx.enterScope();

                    let d = ctx1.ctx.makeExpressionCtx(e, None).toDataTypeNotVoid()?.getArrayType()?;
                    ctx1.ctx.registerVariable(name, d);

                    b.returnType(ctx1.deflate().transfer(), r)?;

                    ctx1.ctx.exitScope()
                }
                RawStatement::Repeat(name, _, b) => {
                    let mut ctx1 = ctx.inflate(s1);

                    ctx1.ctx.enterScope();

                    ctx1.ctx.registerVariable(name, Int);

                    b.returnType(ctx1.deflate().transfer(), r)?;

                    ctx1.ctx.exitScope()
                }
                RawStatement::While(w) => {
                    w.body.returnType(ctx.transfer(), r)?
                }
                RawStatement::Continue => {}
                RawStatement::Break => {}
            }
        }

/*
FIXME this is commented bcs it breaks tests on simple for
if let Some(v) = self.statements.last() {
            if let RawStatement::StatementExpression(e) = &v.exp {
                let t = ctx.makeExpressionCtx(e, None).toDataType()?;

                r.insert(t);

                return Ok(true)
            }
        }*/

        Ok(false)
    }

    pub fn returnType(&self, mut ctx: SimpleCtx<SymbolicOpcode>, r: &mut HashSet<DataType>) -> Result<(), CodeGenError> {
        for s1 in &self.statements {
            let s = &s1.exp;
            match s {
                RawStatement::While(w) => {
                    w.body.returnType(ctx.transfer(), r)?
                }
                RawStatement::If(a) => {
                    a.body.returnType(ctx.transfer(), r)?;

                    if let Some(v) = &a.elseBody {
                        v.returnType(ctx.transfer(), r)?;
                    }

                    for (_, b) in &a.elseIfs {
                        b.returnType(ctx.transfer(), r)?;
                    }
                }
                RawStatement::Return(e) => {
                    let t = ctx.inflate(s1).ctx.makeExpressionCtx(e, None).toDataType()?;
                    r.insert(t);

                    break;
                }
                RawStatement::Loop(b) => {
                    b.returnType(ctx.transfer(), r)?
                }
                RawStatement::StatementExpression(_) => {}
                RawStatement::Assignable(a, b, c, typeHint) => {
                    if let RawExpression::Variable(v) = &a.exp {
                        let mut ctx1 = ctx.inflate(s1);
                        let t = ctx1.ctx.makeExpressionCtx(b, None).toDataTypeNotVoid()?;

                        if let Some(Reference(r)) = typeHint && r.nullable && t.isNull() {
                            ctx1.ctx.registerVariable(v, DataType::Reference(r.clone()));
                        }
                        else {
                            ctx1.ctx.registerVariable(v, t);
                        }
                    }
                }
                RawStatement::ForLoop(name, e, b) => {
                    let mut ctx1 = ctx.inflate(s1);

                    ctx1.ctx.enterScope();

                    let d = ctx1.ctx.makeExpressionCtx(e, None).toDataTypeNotVoid()?.getArrayType()?;
                    ctx1.ctx.registerVariable(name, d);

                    b.returnType(ctx1.deflate().transfer(), r)?;

                    ctx1.ctx.exitScope()
                }
                RawStatement::Repeat(name, _, b) => {
                    let mut ctx1 = ctx.inflate(s1);
                    ctx1.ctx.enterScope();
                    ctx1.ctx.registerVariable(name, Int);

                    b.returnType(ctx1.deflate().transfer(), r)?;

                    ctx1.ctx.exitScope()
                }
                RawStatement::Continue => {}
                RawStatement::Break => {}
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum SymbolicOpcode {
    Op(OpCode),
    Jmp(usize, JmpType),
    LoopLabel(usize),
}

impl SymbolicOpcode {
    pub fn isOp(&self, op: OpCode) -> bool {
        match self {
            Op(v) => v == &op,
            _ => false,
        }
    }
}

impl From<OpCode> for SymbolicOpcode {
    fn from(value: OpCode) -> Self {
        Op(value)
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

impl SimpleCtx<'_, SymbolicOpcode> {
    pub fn decrementLocal(&mut self, localId: usize) {
        self.push(GetLocal { index: localId });
        self.push(PushInt(1));
        self.push(Sub(RawDataType::Int));
        self.push(SetLocal { index: localId });
    }

    pub fn incrementLocal(&mut self, localId: usize) {
        self.push(GetLocal { index: localId });
        self.push(PushInt(1));
        self.push(Add(RawDataType::Int));
        self.push(SetLocal { index: localId });
    }

    pub fn makeLabel(&mut self, id: usize) {
        self.pushInstruction(SymbolicOpcode::LoopLabel(id));
    }

    pub fn opJmp(&mut self, label: usize, jmpType: JmpType) {
        self.pushInstruction(SymbolicOpcode::Jmp(label, jmpType))
    }

    pub fn beginLoop(&mut self) -> (usize, usize) {
        let (start, end) = self.enterLoop();
        self.makeLabel(start);

        (start, end)
    }

    pub fn endLoop(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.getContext().ok_or_else(|| CodeGenError::VeryBadState)?;

        self.makeLabel(ctx.1);
        self.exitLoop();

        Ok(())
    }

    #[inline]
    pub fn push(&mut self, op: OpCode) {
        if let LCall { namespace, id } = op {
            if namespace as usize == self.getNamespace().id {
                self.pushInstruction(Op(SCall { id: id as usize }));
                return;
            }
        }

        self.pushInstruction(Op(op))
    }
}

impl StatementCtx<'_, SymbolicOpcode> {
    pub fn opContinue(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.ctx.getContext().ok_or_else(|| CodeGenError::ContinueOutsideLoop(self.statement.clone()))?;

        self.ctx.opJmp(ctx.0, JmpType::Jmp);

        Ok(())
    }

    pub fn opBreak(&mut self) -> Result<(), CodeGenError> {
        let ctx = self.ctx.getContext().ok_or_else(|| CodeGenError::BreakOutsideLoop(self.statement.clone()))?;

        self.ctx.opJmp(ctx.1, JmpType::Jmp);

        Ok(())
    }

    pub fn push(&mut self, op: OpCode) {
        self.ctx.push(op)
    }
}

impl ExpressionCtx<'_, SymbolicOpcode> {
    pub fn genExpression(self) -> Result<(), CodeGenError> {
        genExpression(self)
    }

    pub fn push(&mut self, c: OpCode) {
        self.ctx.push(c)
    }
}

pub fn genFunctionDef(
    fun: &FunctionMeta,
    ctx: &mut SimpleCtx<SymbolicOpcode>,
) -> Result<(), Vec<CodeGenError>> {
    let mut errors = vec![];

    if let FunctionTypeMeta::Runtime(body) = &fun.functionType {
        // FIXME looks bad it works
        let b = ctx.getSymbols().clone();

        if let Err(e) = body.validate(ctx.transfer(), fun) {
            return Err(vec![e])
        }

        ctx.setSymbols(b);

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
        match ctx.getOps().last() {
            None => ctx.push(Return),
            Some(op) => {
                if !op.isOp(Return) {
                    ctx.push(Return);
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

pub fn genStatement(mut ctx: StatementCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    match &ctx.statement.exp {
        RawStatement::While(w) => {
            ctx.ctx.makeExpressionCtx(&w.exp, None)
                .assertType(Bool)?;

            let (loopStart, loopEnd) = ctx.ctx.beginLoop();

            genExpression(ctx.ctx.makeExpressionCtx(&w.exp, None))?;

            ctx.ctx.opJmp(loopEnd, False);

            w.body.generate(ctx.deflate())?;

            ctx.opContinue()?;

            ctx.ctx.endLoop()?;
        }
        RawStatement::If(flow) => {
            ctx.ctx.makeExpressionCtx(&flow.condition, None)
                .assertType(Bool)?;

            let endLabel = ctx.ctx.nextLabel();

            let mut ifLabels = vec![];

            for _ in 0..flow.elseIfs.len() {
                ifLabels.push(ctx.ctx.nextLabel());
            }

            if flow.elseBody.is_some() {
                ifLabels.push(ctx.ctx.nextLabel());
            }

            genExpression(ctx.ctx.makeExpressionCtx(&flow.condition, None))?;
            if let Some(v) = ifLabels.first() {
                ctx.ctx.opJmp(*v, False);
            } else {
                ctx.ctx.opJmp(endLabel, False)
            }

            flow.body.generate(ctx.deflate())?;

            ctx.ctx.opJmp(endLabel, JmpType::Jmp);

            for (i, (elsExp, elsBody)) in flow.elseIfs.iter().enumerate() {
                ctx.ctx.makeLabel(*ifLabels.get(i).ok_or_else(||CodeGenError::VeryBadState)?);

                ctx.ctx.makeExpressionCtx(&elsExp, None)
                    .assertType(Bool)?;
                ctx.ctx.makeExpressionCtx(&elsExp, None).genExpression()?;

                if let Some(v) = ifLabels.get(i + 1) {
                    ctx.ctx.opJmp(*v, False);
                } else {
                    ctx.ctx.opJmp(endLabel, False);
                }

                elsBody.generate(ctx.deflate())?;
                ctx.ctx.opJmp(endLabel, JmpType::Jmp);
            }

            if let Some(els) = &flow.elseBody {
                ctx.ctx.makeLabel(*ifLabels.last().ok_or_else(||CodeGenError::VeryBadState)?);
                els.generate(ctx.deflate())?;
            }
            ctx.ctx.makeLabel(endLabel);
        }
        RawStatement::Return(ret) => {
            genExpression(ctx.ctx.makeExpressionCtx(&ret, None))?;
            ctx.ctx.push(Return)
        }
        RawStatement::Continue => ctx.opContinue()?,
        RawStatement::Break => ctx.opBreak()?,
        RawStatement::Loop(body) => {
            ctx.ctx.beginLoop();

            body.generate(ctx.deflate())?;

            ctx.opContinue()?;
            ctx.ctx.endLoop()?;
        }
        RawStatement::StatementExpression(v) => {
            let mut eCtx = ctx.ctx.makeExpressionCtx(v, None);

            let ret = eCtx.toDataType()?;

            genExpression(eCtx)?;

            if ret != Void {
                (ctx.ctx.getHandle())(&mut ctx, ret);
            }
        }
        RawStatement::Assignable(dest, value, t, typeHint) => {
            let a = ctx.ctx.makeExpressionCtx(dest, None).toDataType();

            let b = ctx.ctx.makeExpressionCtx(value, None).toDataType()?;

            if let Ok(t) = a {
                if !t.canAssign(&b) || t.isVoid() || b.isVoid() {
                    println!("SUS {:?} {:?}", t, b);
                    return Err(CodeGenError::AssignableTypeError{
                        var: dest.clone(),
                        varType: t,
                        exp: value.clone(),
                        expType: b,
                    })
                }
            }

            ctx.ctx.makeExpressionCtx(value, None).toDataTypeNotVoid()?;

            match &dest.exp {
                RawExpression::Variable(v) => {
                    match ctx.ctx.getVariable(v) {
                        Ok(v) => {
                            let d = v.0.clone();
                            ctx.ctx.makeExpressionCtx(value, None).assertType(d)?;
                        }
                        Err(_) => {
                            let t = ctx.ctx.makeExpressionCtx(value, None).toDataType()?;

                            if let Some(Reference(r)) = typeHint && r.nullable && t.isNull() {
                                ctx.ctx.registerVariable(v, DataType::Reference(r.clone()));
                            }
                            else {
                                ctx.ctx.registerVariable(v, t);
                            }
                        }
                    }
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.ctx.makeExpressionCtx(&v.expr, None).genExpression()?;
                }
                RawExpression::NamespaceAccess(_) => todo!(),
                RawExpression::FieldAccess(obj, _) => {
                    ctx.ctx.makeExpressionCtx(obj, None).genExpression()?;
                }
                _ => {
                    return Err(CodeGenError::ExpressionIsNotAssignable)
                },
            }

            match t {
                None => {
                    ctx.ctx.makeExpressionCtx(value, None).genExpression()?;
                }
                Some(v) => {
                    let mut c1 = ctx.ctx.makeExpressionCtx(dest, None);
                    let t = c1.toDataType()?;
                    c1.genExpression()?;
                    ctx.ctx.makeExpressionCtx(value, None).genExpression()?;
                    let o = match v {
                        ArithmeticOp::Add => OpCode::Add(t.toRawType()?),
                        ArithmeticOp::Sub => OpCode::Sub(t.toRawType()?),
                        ArithmeticOp::Mul => OpCode::Mul(t.toRawType()?),
                        ArithmeticOp::Div => OpCode::Div(t.toRawType()?),
                    };
                    ctx.ctx.push(o);
                }
            }

            match &dest.exp {
                RawExpression::Variable(v) => {
                    let var = ctx.ctx.getVariable(v)?.1;

                    ctx.ctx.push(SetLocal { index: var })
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.ctx.makeExpressionCtx(&v.index, None).genExpression()?;

                    ctx.ctx.push(ArrayStore)
                }
                RawExpression::NamespaceAccess(v) => unsafe {
                    let global = ctx.ctx.findGlobalParts(v)?;

                    ctx.ctx.push(SetGlobal {
                        namespaceID: global.nId as u32,
                        globalID: global.gId as u32,
                    })
                },
                RawExpression::FieldAccess(obj, field) => {
                    let mut cd = ctx.ctx.makeExpressionCtx(obj, None);

                    let class = cd.toDataType()?.getRef()?;

                    let fiel = ctx.ctx.getStruct(&class.name)?.meta.findField(field)?;

                    ctx.ctx.push(SetField {
                        fieldID: fiel.1
                    })
                }
                _ => {
                    return Err(CodeGenError::ExpressionIsNotAssignable)
                },
            }
        }
        RawStatement::ForLoop(var, arr, body) => {
            let startLabel = ctx.ctx.nextLabel();

            let t = ctx.ctx.makeExpressionCtx(arr, None).toDataTypeNotVoid()?.getArrayType()?;
            ctx.ctx.enterScope();

            let lenId = ctx.ctx.registerVariable(&format!("__for-len{}", microsSinceEpoch()), Int);
            let arrId = ctx.ctx.registerVariable(&format!("__for-array{}", microsSinceEpoch()), Int);
            let counterId = ctx.ctx.registerVariable(&format!("__for-counter{}", microsSinceEpoch()), Int);
            let varId  = ctx.ctx.registerVariable(var, t);

            ctx.push(PushInt(0));
            ctx.push(SetLocal { index: counterId });

            ctx.ctx.makeExpressionCtx(arr, None).genExpression()?;
            ctx.push(Dup);
            ctx.push(ArrayLength);
            ctx.push(SetLocal { index: lenId });
            ctx.push(SetLocal { index: arrId });

            ctx.ctx.opJmp(startLabel, JmpType::Jmp);

            let (_, endLoop) = ctx.ctx.beginLoop();

            ctx.ctx.incrementLocal(counterId);

            ctx.ctx.makeLabel(startLabel);

            ctx.push(GetLocal { index: lenId });
            ctx.push(GetLocal { index: counterId });
            ctx.push(OpCode::Equals(RawDataType::Int));
            ctx.ctx.opJmp(endLoop, True);

            ctx.push(GetLocal { index: arrId });
            ctx.push(GetLocal { index: counterId  });
            ctx.push(ArrayLoad);
            ctx.push(SetLocal { index: varId });

            body.generate(ctx.deflate())?;

            ctx.opContinue()?;

            ctx.ctx.endLoop()?;

            ctx.ctx.exitScope();
        }
        RawStatement::Repeat(var, count, body) => {
            let loopStart = ctx.ctx.nextLabel();

            ctx.ctx.opJmp(loopStart, JmpType::Jmp);
            let (_, endLoop) = ctx.ctx.beginLoop();
            let (_, varId) = ctx.ctx.registerVariableIfNotExists(var, DataType::Int);

            ctx.push(OpCode::Inc {
                typ: Int.toRawType()?,
                index: varId as u32,
            });
            ctx.ctx.makeLabel(loopStart);

            ctx.push(GetLocal { index: varId });
            ctx.push(PushInt(*count as isize));
            ctx.push(Greater(Int.toRawType()?));
            ctx.ctx.opJmp(endLoop, False);

            body.generate(ctx.deflate())?;

            ctx.opContinue()?;

            ctx.ctx.endLoop()?;
            ctx.ctx.makeLabel(endLoop);
        }
    }
    Ok(())
}

pub fn genExpression(ctx: ExpressionCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    let mut r = ctx;

    unsafe {
        match &r.exp.exp {
            RawExpression::BinaryOperation { left, right, op: BinaryOp::Or } => {
                r.transfer(right).assertType(Bool)?;
                r.transfer(left).assertType(Bool)?;

                let endLabel = r.ctx.nextLabel();

                r.transfer(left).genExpression()?;

                r.ctx.push(Dup);
                r.ctx.opJmp(endLabel, True);
                r.transfer(right).genExpression()?;
                r.ctx.push(OpCode::Or);

                r.ctx.makeLabel(endLabel);
            }

            RawExpression::BinaryOperation { left, right, op: BinaryOp::And } => {
                r.transfer(right).assertType(Bool)?;
                r.transfer(left).assertType(Bool)?;

                let endLabel = r.ctx.nextLabel();

                r.transfer(left).genExpression()?;

                r.push(Dup);
                r.ctx.opJmp(endLabel, False);
                r.transfer(right).genExpression()?;
                r.push(And);

                r.ctx.makeLabel(endLabel);
            }

            RawExpression::BinaryOperation { left, right, op } => {
                let dat = r.transfer(left).toDataTypeNotVoid()?;
                let dat1 = r.transfer(right).toDataTypeNotVoid()?;

                r.transfer(left).genExpression()?;
                r.transfer(right).genExpression()?;

                if dat.isString() {
                    let f = r.ctx.findFunction("concat", &[DataType::str(), DataType::str()])?;
                    r.push(f.callInstruction());
                    return Ok(())
                }

                let rawT = dat.toRawType()?;

                let ts = match op {
                    BinaryOp::Add => vec![OpCode::Add(rawT)],
                    BinaryOp::Sub => vec![OpCode::Sub(rawT)],
                    BinaryOp::Mul => vec![OpCode::Mul(rawT)],
                    BinaryOp::Div => vec![OpCode::Div(rawT)],
                    BinaryOp::Gt => vec![OpCode::Greater(rawT)],
                    BinaryOp::Less => vec![OpCode::Less(rawT)],
                    BinaryOp::Eq => vec![OpCode::Equals(rawT)],
                    BinaryOp::And => vec![OpCode::And],
                    BinaryOp::Or => vec![OpCode::Or],
                    BinaryOp::Modulo => vec![OpCode::Modulo(rawT)],
                    BinaryOp::NotEq => vec![OpCode::Equals(rawT), Not],
                    BinaryOp::ShiftLeft => vec![OpCode::ShiftLeft],
                    BinaryOp::ShiftRight => vec![OpCode::ShiftRight],
                    BinaryOp::BitwiseOr => vec![OpCode::BitwiseOr],
                    BinaryOp::BitwiseAnd => vec![OpCode::BitwiseAnd],
                    BinaryOp::Xor => vec![OpCode::Xor]
                };
                for t in ts {
                    r.push(t)
                }
            }
            RawExpression::IntLiteral(i) => r.push(PushInt(i.parse::<isize>().map_err(|_| LiteralParseError)?)),
            RawExpression::FloatLiteral(i) => r.push(OpCode::PushFloat(i.parse::<f64>().map_err(|_| LiteralParseError)?)),
            RawExpression::StringLiteral(i) => {
                let sId = r.ctx.allocateOrGetString(i);
                r.push(StrNew(sId));
            },
            RawExpression::BoolLiteral(i) => r.push(OpCode::PushBool(*i)),
            RawExpression::Variable(v) => {
                match r.ctx.getVariable(v) {
                    Ok(v) => {
                        r.push(OpCode::GetLocal {
                            index: v.1,
                        });
                    }
                    Err(_) => {
                        let (_, f) = r.ctx.lookupFunctionByBaseName(v)?;

                        r.push(f.pushInstruction())
                    }
                }
            },
            RawExpression::CharLiteral(c) => r.push(PushChar(*c)),
            RawExpression::ArrayLiteral(i) => {
                let d = match r.typeHint {
                    None => Some(
                        r.transfer(i.first().ok_or_else(||UntypedEmptyArray)?)
                            .toDataTypeNotVoid()?,
                    ),
                    Some(ref v) => match v {
                        DataType::Reference(v) => match v.generics.first() {
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
                    r.transfer(exp).genExpression()?;
                    r.push(PushInt(ind as isize));
                    r.push(ArrayStore);
                }
            }
            RawExpression::ArrayIndexing(i) => {
                let d = r.transfer(&i.expr).toDataTypeNotVoid()?;

                if d.isString() {
                    r.transfer(&i.expr).genExpression()?;
                    r.transfer(&i.index).genExpression()?;
                    r.push(GetChar);
                    return Ok(())
                }
                else if d.isArray() {
                    genExpression(r.transfer(&i.expr))?;
                    genExpression(r.transfer(&i.index))?;

                    r.push(ArrayLoad);

                    return Ok(())
                }

                return Err(CodeGenError::ExpectedReference)
            }
            RawExpression::NotExpression(e, _) => {
                genExpression(r.transfer(e))?;
                r.push(Not)
            }
            RawExpression::NamespaceAccess(parts) => {
                let g = r.ctx.findGlobalParts(parts)?;

                r.push(OpCode::GetGlobal {
                    namespaceID: g.nId as u32,
                    globalID: g.gId as u32,
                })
            }
            RawExpression::Lambda(args, body, ret) => {
                todo!();
            }
            RawExpression::Callable(prev, args) => unsafe {
                match &prev.exp {
                    RawExpression::Variable(v) => {
                        for arg in args {
                            r.transfer(arg).toDataTypeNotVoid()?;
                            genExpression(r.transfer(arg))?;
                        }

                        if r.ctx.getVariable(v).is_err() {
                            let gz = args
                                .iter()
                                .map(|it| r.transfer(it).toDataType())
                                .collect::<Result<Vec<_>, _>>()?;
                            let func = r.ctx.findFunction(v, &gz)?;


                            r.push(func.callInstruction());
                        } else {
                            let (args, prevT) = r.transfer(prev).toDataType()?.getFunction()?;

                            genExpression(r.transfer(prev))?;
                            r.push(DynamicCall { returns: !prevT.isVoid(), argsCount: args.len() });
                        }
                    }
                    RawExpression::NamespaceAccess(v) => {
                        for arg in args {
                            r.transfer(arg).toDataTypeNotVoid()?;
                            genExpression(r.transfer(arg))?;
                        }

                        let gz = args
                            .iter()
                            .map(|it| r.transfer(it).toDataType())
                            .collect::<Result<Vec<_>, _>>()?;
                        let f = r.ctx.findFunctionParts(v, &gz)?;

                        r.push(f.callInstruction());
                    }
                    RawExpression::FieldAccess(v, name) => {
                        if let Ok(t) = r.transfer(prev).toDataType() {
                            // TODO type checking
                            if let Ok(v) = t.getFunction() {
                                for arg in args {
                                    r.transfer(arg).toDataTypeNotVoid()?;
                                    genExpression(r.transfer(arg))?;
                                }

                                r.transfer(prev).genExpression()?;
                                r.push(DynamicCall { returns: !v.1.isVoid(), argsCount: v.0.len() });

                                return Ok(())
                            }
                        }

                        let prevT = r.transfer(v).toDataTypeNotVoid()?;

                        let mut argsB = vec![];

                        argsB.push(prevT);
                        argsB.extend(
                            args.iter()
                                .map(|it| r.transfer(it).toDataType()).collect::<Result<Vec<_>, _>>()?,
                        );

                        r.transfer(v).genExpression()?;

                        for arg in args {
                            r.transfer(arg).toDataTypeNotVoid()?;
                            genExpression(r.transfer(arg))?;
                        }

                        let func = r.ctx.findFunction(name, &argsB)?;

                        r.push(func.callInstruction())
                    }
                    _ => {
                        return Err(CodeGenError::ExpressionIsNotAssignable)
                    }
                }
            },
            RawExpression::StructInit(name, init) => {
                // FIXME doesnt support other namespaces
                let s = r.ctx.getStruct(name.last().unwrap())?;

                let namespaceID = s.nId as u32;
                let structID = s.sId as u32;

                r.push(New {
                    namespaceID,
                    structID,
                });


                for (fieldName, value) in init {
                    r.push(Dup);
                    let s1 = r.ctx.getStruct(name.last().unwrap())?;
                    let (_, fieldID) = s1.meta.findField(fieldName)?;
                    let ctx = r.transfer(value);
                    genExpression(ctx)?;
                    r.push(SetField {
                        fieldID,
                    })
                }
            }
            RawExpression::FieldAccess(prev, fieldName) => {
                let ctx = r.transfer(prev);
                genExpression(ctx)?;

                let e = r.transfer(prev).toDataTypeNotVoid()?;


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


                let fiel = r.ctx.getStruct(&o.name)?.meta.findField(fieldName)?;
                r.push(GetField {
                    fieldID: fiel.1
                });
            }
            RawExpression::Null => r.push(OpCode::PushIntZero),
            RawExpression::TernaryOperator(cond, tr, fal) => {
                let falseLabel = r.ctx.nextLabel();
                let endLabel = r.ctx.nextLabel();

                r.transfer(cond).genExpression()?;

                r.ctx.opJmp(falseLabel, False);

                r.transfer(tr).genExpression()?;
                r.ctx.opJmp(endLabel, JmpType::Jmp);

                r.ctx.makeLabel(falseLabel);
                r.transfer(fal).genExpression()?;

                r.ctx.makeLabel(endLabel);
            }
            RawExpression::TypeCast(exp, target) => {
                let src = r.transfer(exp).toDataTypeNotVoid()?;

                if &src == target || target.isValue() || src.isValue() {
                    r.transfer(exp).genExpression()?;
                }

                else if src.isInt() && target.isFloat() {
                    r.transfer(exp).genExpression()?;
                    r.push(I2F);
                }

                else if target.isInt() && src.isFloat() {
                    r.transfer(exp).genExpression()?;
                    r.push(F2I);
                }

                else if src.isReference() && target.isObject() {
                    r.transfer(exp).genExpression()?;
                }

                else {
                    todo!("src: {:?} tgt: {:?}", src, target);
                }
            }
            RawExpression::FormatStringLiteral(_) => todo!(),
            RawExpression::TypeCheck(source, target) => {
                let src = r.transfer(source).toDataType()?;

                if src.isObject() && let DataType::Reference(v) = target {
                    r.transfer(source).genExpression()?;

                    let t = r.ctx.getStruct(&target.toString())?;

                    r.push(IsStruct { namespaceId: t.nId, structId: t.sId })
                }
                else {
                    r.push(PushBool(&src == target))
                }
            }
            RawExpression::Negate(e) => {
                let t = r.transfer(e).toDataType()?;

                if t.isFloat() {
                    r.push(OpCode::PushFloat(0.0));
                    r.transfer(e).genExpression()?;
                    r.push(Sub(RawDataType::Float));
                }
                else if t.isInt() {
                    r.push(OpCode::PushInt(0));
                    r.transfer(e).genExpression()?;
                    r.push(Sub(RawDataType::Int));
                }
                else {
                    unreachable!("invalid type, type should be float or int")
                }
            }
            RawExpression::BitwiseNot(e) => {
                r.transfer(e).genExpression()?;

                r.push(OpCode::BitwiseNot)
            }
            RawExpression::NullAssert(e) => {
                r.transfer(e).genExpression()?;

                r.push(Dup);
                r.push(PushInt(0));
                r.push(Swap);
                r.push(Div(RawDataType::Int));
                r.push(Pop);
            }
            RawExpression::Elvis(a, b) => {
                let endLabel = r.ctx.nextLabel();

                r.transfer(a).genExpression()?;

                r.push(Dup);
                r.push(PushInt(0));
                r.push(OpCode::Equals(RawDataType::Int));
                r.ctx.opJmp(endLabel, False);
                r.push(Pop);
                r.transfer(b).genExpression()?;
                r.ctx.makeLabel(endLabel);
            }
        }
    }
    Ok(())
}
