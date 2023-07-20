use std::collections::{HashMap, HashSet};

use crate::ast::{ArithmeticOp, BinaryOp, Expression, RawExpression, RawStatement};
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::codeGenCtx::{Body, ExpressionCtx, SimpleCtx, StatementCtx};
use crate::errors::{CodeGenError, SymbolNotFoundE, TypeError};
use crate::errors::CodeGenError::LiteralParseError;
use crate::lexer::*;
use crate::parser::*;
use crate::symbolManager::FunctionSymbol;
use crate::utils::microsSinceEpoch;
use crate::vm::dataType::{DataType, RawDataType};
use crate::vm::dataType::DataType::{Bool, Char, Float, Int, Null, Reference, Void};
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta};
use crate::vm::vm::{JmpType, OpCode};
use crate::vm::vm::JmpType::{False, True};
use crate::vm::vm::OpCode::{Add, And, ArrayLength, ArrayLoad, ArrayNew, ArrayStore, AssertNotNull, Div, Dup, DynamicCall, F2I, GetChar, GetField, GetLocal, Greater, I2F, IsStruct, LCall, Mul, New, Not, Pop, PushBool, PushChar, PushInt, PushNull, Return, SCall, SetField, SetGlobal, SetLocal, StringLength, StrNew, Sub};

const DEBUG: bool = true;

impl Body {
    pub fn generate(
        &self,
        mut ctx: SimpleCtx<SymbolicOpcode>,
    ) -> Result<(), CodeGenError> {
        ctx.enterScope();
        for (i, statement) in self.statements.iter().enumerate() {
            let mut y = ctx.inflate(statement);
            y.isLast = i == self.statements.len()-1;
            println!("izLast {:?}", y.isLast);
            genStatement(y)?;
        }
        ctx.exitScope();

        Ok(())
    }

    pub fn validate(&self, mut ctx: SimpleCtx<SymbolicOpcode>, fun: &FunctionMeta) -> Result<(), CodeGenError> {
        let mut s = HashSet::new();

        let isCovered = self.coverageCheck(ctx.transfer(), &mut s)?;

        if fun.returnType.isVoid() && s.is_empty() {
            return Ok(());
        }

        if (!isCovered || s.is_empty()) && (s.len() == 1 && !s.contains(&Void)) {
            eprintln!("!isCovered || s.is_empty() {} {} {:?}", !isCovered, s.is_empty(), s);
            return Err(CodeGenError::InvalidReturns);
        }

        if s.len() == 2 && s.contains(&Null) {
            let t = s.iter().find(|it| !it.isNull()).unwrap().clone();
            if t.isReference() && fun.returnType.isReferenceNullable() {
                return Ok(());
            }
        }

        if s.len() > 1 {
            if DEBUG {
                println!("more than 1 return ${:?}", s)
            }
            return Err(CodeGenError::InvalidReturns);
        }

        let tt = s.iter().next().unwrap();

        if fun.returnType.isReferenceNullable() && tt.isNull() {
            return Ok(());
        }

        if let Ok(v) = tt.clone().getRef() && let Ok(v1) = fun.returnType.clone().getRef() && v.name == v1.name {
            return if v.nullable && !v1.nullable {
                Err(CodeGenError::InvalidReturns)
            } else {
                Ok(())
            };
        }

        if fun.returnType != Void && !fun.returnType.canAssign(tt) {
            return Err(CodeGenError::TypeError(TypeError::newNone(fun.returnType.clone(), tt.clone())));
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
                    } else {
                        covered = false;
                    }

                    for (_, b) in &a.elseIfs {
                        covered &= b.coverageCheck(ctx.transfer(), r)?;
                    }

                    if covered {
                        return Ok(true);
                    }
                }
                RawStatement::Return(e) => {
                    if let Some(e) = e {
                        let t = ctx.inflate(s1).ctx.makeExpressionCtx(e).toDataType()?;
                        r.insert(t);
                    } else {
                        r.insert(Void);
                    }

                    return Ok(true);
                }
                RawStatement::Loop(b) => {
                    b.returnType(ctx.transfer(), r)?
                }
                RawStatement::StatementExpression(_) => {}
                RawStatement::Assignable(a, b, c, typeHint) => {
                    if let RawExpression::Variable(v) = &a.exp {
                        let mut ctx1 = ctx.inflate(s1);
                        let t = ctx1.ctx.makeExpressionCtx(b).toDataTypeNotVoid()?;

                        if let Some(Reference(r)) = typeHint && r.nullable && t.isNull() {
                            ctx1.ctx.registerVariable(v, DataType::Reference(r.clone()));
                        } else {
                            ctx1.ctx.registerVariable(v, t);
                        }
                    }
                }
                RawStatement::ForLoop(name, e, b) => {
                    let mut ctx1 = ctx.inflate(s1);

                    ctx1.ctx.enterScope();

                    let d = ctx1.ctx.makeExpressionCtx(e).toDataTypeNotVoid()?.getArrayType()?;
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
        if let Some(v) = self.statements.last() && let RawStatement::StatementExpression(e) = &v.exp && !self.isLoop {
            let t = ctx.makeExpressionCtx(e).toDataType()?;

            r.insert(t);

            return Ok(true);
        }

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
                    if let Some(e) = e {
                        let t = ctx.inflate(s1).ctx.makeExpressionCtx(e).toDataType()?;
                        r.insert(t);
                    } else {
                        r.insert(Void);
                    }
                    break;
                }
                RawStatement::Loop(b) => {
                    b.returnType(ctx.transfer(), r)?
                }
                RawStatement::StatementExpression(_) => {}
                RawStatement::Assignable(a, b, c, typeHint) => {
                    if let RawExpression::Variable(v) = &a.exp {
                        let mut ctx1 = ctx.inflate(s1);
                        let t = ctx1.ctx.makeExpressionCtx(b).toDataTypeNotVoid()?;

                        if let Some(Reference(r)) = typeHint && r.nullable && t.isNull() {
                            ctx1.ctx.registerVariable(v, Reference(r.clone()));
                        } else {
                            ctx1.ctx.registerVariable(v, t);
                        }
                    }
                }
                RawStatement::ForLoop(name, e, b) => {
                    let mut ctx1 = ctx.inflate(s1);

                    ctx1.ctx.enterScope();

                    let d = ctx1.ctx.makeExpressionCtx(e).toDataTypeNotVoid()?.getArrayType()?;
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
                let i = *labelLookup.get(&id).ok_or_else(|| CodeGenError::VeryBadState)?;
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
    pub fn boxValue(&mut self, t: DataType, exp: &Expression) -> Result<(), CodeGenError> {
        match t {
            Int => {
                let s = self.getStruct("Float")?;
                self.push(New { namespaceID: s.nId as u32, structID: s.sId as u32 });
            }
            Float => {
                let s = self.getStruct("Float")?;
                self.push(New { namespaceID: s.nId as u32, structID: s.sId as u32 });
            }
            Bool => {
                let s = self.getStruct("Float")?;
                self.push(New { namespaceID: s.nId as u32, structID: s.sId as u32 });
            }
            Char => {
                let s = self.getStruct("Float")?;
                self.push(New { namespaceID: s.nId as u32, structID: s.sId as u32 });
            }
            _ => unreachable!()
        }

        self.push(Dup);
        self.makeExpressionCtx(exp).genExpression()?;
        self.push(SetField { fieldID: 0 });

        Ok(())
    }

    pub fn implicitConversion(&mut self, src: DataType, tgt: DataType, exp: &Expression) -> Result<(), CodeGenError> {
        if src.isPrimitiveType() && tgt.isBoxed() {
            self.makeExpressionCtx(exp).genExpressionBox()
        } else if src.isBoxed() && tgt.isPrimitiveType() {
            self.makeExpressionCtx(exp).genExpressionUnbox()
        } else {
            self.makeExpressionCtx(exp).genExpression()
        }
    }

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

    pub fn genExpressionUnbox(mut self) -> Result<(), CodeGenError> {
        let t = self.toDataType()?;
        self.transfer(self.exp).genExpression()?;

        if t.isBoxed() {
            self.push(GetField { fieldID: 0 });
        }

        Ok(())
    }

    pub fn genExpressionBox(mut self) -> Result<(), CodeGenError> {
        let t = self.toDataType()?;

        if t.isPrimitiveType() {
            self.ctx.boxValue(t, self.exp)
        } else {
            self.genExpression()
        }
    }

    pub fn genExpressionExpecting(mut self, t: DataType) -> Result<(), CodeGenError> {
        genExpressionExpecting(self, Some(t))
    }

    pub fn genCondition(mut self) -> Result<(), CodeGenError> {
        self.genExpressionExpecting(Bool)
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
            return Err(vec![e]);
        }

        ctx.setSymbols(b);


        for (i, statement) in body.statements.iter().enumerate() {
            let mut y = ctx.inflate(statement);
            y.isLast = i == body.statements.len()-1;
            y.expectedReturn = fun.returnType.clone();

            if DEBUG {
                println!("gening {:?}", statement)
            }
            if let Err(e) = genStatement(y) {
                errors.push(e)
            }
        }

        if !errors.is_empty() {
            return Err(errors);
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
    } else {
        println!("this should never get called :3");
        Err(vec![CodeGenError::VeryBadState])
    }
}

pub fn genStatement(mut ctx: StatementCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    match &ctx.statement.exp {
        RawStatement::While(w) => {
            ctx.ctx.makeExpressionCtx(&w.exp).assertType(Bool)?;

            let (_, loopEnd) = ctx.ctx.beginLoop();

            ctx.ctx.makeExpressionCtx(&w.exp).genExpression()?;

            ctx.ctx.opJmp(loopEnd, False);

            w.body.generate(ctx.deflate())?;

            ctx.opContinue()?;

            ctx.ctx.endLoop()?;
        }
        RawStatement::If(flow) => {
            ctx.ctx.makeExpressionCtx(&flow.condition)
                .assertType(Bool)?;

            let endLabel = ctx.ctx.nextLabel();

            let mut ifLabels = vec![];

            for _ in 0..flow.elseIfs.len() {
                ifLabels.push(ctx.ctx.nextLabel());
            }

            if flow.elseBody.is_some() {
                ifLabels.push(ctx.ctx.nextLabel());
            }

            ctx.ctx.makeExpressionCtx(&flow.condition).genExpression()?;
            if let Some(v) = ifLabels.first() {
                ctx.ctx.opJmp(*v, False);
            } else {
                ctx.ctx.opJmp(endLabel, False)
            }

            flow.body.generate(ctx.deflate())?;

            ctx.ctx.opJmp(endLabel, JmpType::Jmp);

            for (i, (elsExp, elsBody)) in flow.elseIfs.iter().enumerate() {
                ctx.ctx.makeLabel(*ifLabels.get(i).ok_or_else(|| CodeGenError::VeryBadState)?);

                ctx.ctx.makeExpressionCtx(&elsExp)
                    .assertType(Bool)?;
                ctx.ctx.makeExpressionCtx(&elsExp).genExpression()?;

                if let Some(v) = ifLabels.get(i + 1) {
                    ctx.ctx.opJmp(*v, False);
                } else {
                    ctx.ctx.opJmp(endLabel, False);
                }

                elsBody.generate(ctx.deflate())?;
                ctx.ctx.opJmp(endLabel, JmpType::Jmp);
            }

            if let Some(els) = &flow.elseBody {
                ctx.ctx.makeLabel(*ifLabels.last().ok_or_else(|| CodeGenError::VeryBadState)?);
                els.generate(ctx.deflate())?;
            }
            ctx.ctx.makeLabel(endLabel);
        }
        RawStatement::Return(ret) => {
            if let Some(ret) = ret {
                ctx.ctx.makeExpressionCtx(&ret).genExpression()?;
            }
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
            let mut eCtx = ctx.ctx.makeExpressionCtx(v);

            let ret = eCtx.toDataType()?;

            eCtx.genExpression()?;

            if (ret != Void && ctx.isLast && ctx.expectedReturn == Void) || (!ctx.isLast && ret != Void) {
                (ctx.ctx.getHandle())(&mut ctx, ret);
            }
        }
        RawStatement::Assignable(dest, value, operator, typeHint) => {
            let a = ctx.ctx.makeExpressionCtx(dest).toDataType();

            let b = ctx.ctx.makeExpressionCtx(value).toDataType()?;

            if let Ok(t) = a {
                if !t.canAssign(&b) || t.isVoid() || b.isVoid() {
                    return Err(CodeGenError::AssignableTypeError {
                        var: dest.clone(),
                        varType: t,
                        exp: value.clone(),
                        expType: b,
                    });
                }
            }

            ctx.ctx.makeExpressionCtx(value).toDataTypeNotVoid()?;

            match &dest.exp {
                RawExpression::Variable(v) => {
                    match ctx.ctx.getLocal(v) {
                        Ok(v) => {
                            let d = v.0.clone();
                            ctx.ctx.makeExpressionCtx(value).assertType(d)?;
                        }
                        Err(_) => {
                            let t = ctx.ctx.makeExpressionCtx(value).toDataType()?;

                            if operator.is_some() {
                                return Err(CodeGenError::SymbolNotFound(SymbolNotFoundE::var(v)));
                            }

                            if let Some(Reference(r)) = typeHint && r.nullable && t.isNull() {
                                ctx.ctx.registerVariable(v, DataType::Reference(r.clone()));
                            } else {
                                ctx.ctx.registerVariable(v, t);
                            }
                        }
                    }
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.ctx.makeExpressionCtx(&v.expr).genExpression()?;
                }
                RawExpression::NamespaceAccess(_) => todo!(),
                RawExpression::FieldAccess(obj, _) => {
                    ctx.ctx.makeExpressionCtx(obj).genExpression()?;
                }
                _ => {
                    return Err(CodeGenError::ExpressionIsNotAssignable);
                }
            }

            match operator {
                None => {
                    ctx.ctx.makeExpressionCtx(value).genExpression()?;
                }
                Some(v) => {
                    let mut c1 = ctx.ctx.makeExpressionCtx(dest);
                    let t = c1.toDataType()?;
                    c1.genExpression()?;
                    ctx.ctx.makeExpressionCtx(value).genExpression()?;
                    let o = match v {
                        ArithmeticOp::Add => Add(t.toRawType()?),
                        ArithmeticOp::Sub => Sub(t.toRawType()?),
                        ArithmeticOp::Mul => Mul(t.toRawType()?),
                        ArithmeticOp::Div => Div(t.toRawType()?),
                        ArithmeticOp::Modulo => OpCode::Modulo(t.toRawType()?),
                        ArithmeticOp::ShiftLeft => OpCode::ShiftLeft,
                        ArithmeticOp::ShiftRight => OpCode::ShiftRight,
                        ArithmeticOp::BitwiseOr => OpCode::BitwiseOr,
                        ArithmeticOp::BitwiseAnd => OpCode::BitwiseAnd,
                        ArithmeticOp::Xor => OpCode::Xor,
                    };
                    ctx.ctx.push(o);
                }
            }

            match &dest.exp {
                RawExpression::Variable(v) => {
                    let var = ctx.ctx.getLocal(v)?.1;

                    ctx.ctx.push(SetLocal { index: var })
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.ctx.makeExpressionCtx(&v.index).genExpression()?;

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
                    let mut cd = ctx.ctx.makeExpressionCtx(obj);

                    let class = cd.toDataType()?.getRef()?;

                    let fiel = ctx.ctx.getStruct(&class.name)?.meta.findField(field)?;

                    ctx.ctx.push(SetField {
                        fieldID: fiel.1
                    })
                }
                _ => {
                    return Err(CodeGenError::ExpressionIsNotAssignable);
                }
            }
        }
        RawStatement::ForLoop(var, arr, body) => {
            let startLabel = ctx.ctx.nextLabel();

            let t = ctx.ctx.makeExpressionCtx(arr).toDataTypeNotVoid()?.getArrayType()?;
            ctx.ctx.enterScope();

            let lenId = ctx.ctx.registerVariable(&format!("__for-len{}", microsSinceEpoch()), Int);
            let arrId = ctx.ctx.registerVariable(&format!("__for-array{}", microsSinceEpoch()), Int);
            let counterId = ctx.ctx.registerVariable(&format!("__for-counter{}", microsSinceEpoch()), Int);
            let varId = ctx.ctx.registerVariable(var, t);

            ctx.push(PushInt(0));
            ctx.push(SetLocal { index: counterId });

            ctx.ctx.makeExpressionCtx(arr).genExpression()?;
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
            ctx.push(GetLocal { index: counterId });
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

pub fn generateImplicitConversion(src: DataType, tgt: DataType, exp: ExpressionCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    if src.isReferenceNonNullable() && tgt.isReference() {
        return Ok(());
    }
    if src.isReferenceNonNullable() && tgt.isObject() {
        return Ok(());
    }
    if src.isReferenceNullable() && tgt.isObjectNullable() {
        return Ok(());
    }
    if src.isNull() && tgt.isReferenceNullable() {
        return Ok(());
    }
    if src.isNull() && tgt.isObjectNullable() {
        return Ok(());
    }
    if src.isObjectNonNullable() && tgt.isObject() {
        return Ok(());
    }

    if src.clone().isBoxedNonNull() {
        let unboxed = src.toUnboxedType();
        assert_eq!(unboxed, tgt);

        return exp.genExpressionUnbox();
    }

    if src.clone().isPrimitiveType() {
        let unboxed = tgt.clone().toUnboxedType();
        assert_eq!(unboxed, src);

        return exp.genExpressionBox();
    }

    todo!("src: {src:?}, tgt: {tgt:?}")
}

pub fn genExpressionExpecting(mut ctx: ExpressionCtx<SymbolicOpcode>, expect: Option<DataType>) -> Result<(), CodeGenError> {
    if let Some(tgt) = expect {
        let src = ctx.clone().toDataType()?;
        if src != tgt {
            return generateImplicitConversion(src, tgt, ctx.clone());
        }
    }

    genExpression(ctx)
}

pub fn genExpression(ctx: ExpressionCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    let mut r = ctx;

    unsafe {
        match &r.exp.exp {
            RawExpression::BinaryOperation { left, right, op } if matches!(op, BinaryOp::And | BinaryOp::Or) => {
                r.transfer(right).assertType(Bool)?;
                r.transfer(left).assertType(Bool)?;
                let isAnd = op == &BinaryOp::And;

                let endLabel = r.ctx.nextLabel();

                r.transfer(left).genExpressionExpecting(Bool)?;

                r.push(Dup);
                r.ctx.opJmp(endLabel, if isAnd { False } else { True });
                r.transfer(right).genExpressionExpecting(Bool)?;
                if isAnd {
                    r.push(And);
                } else {
                    r.push(OpCode::Or);
                }

                r.ctx.makeLabel(endLabel);
            }

            RawExpression::BinaryOperation { left, right, op } if matches!(op, BinaryOp::Xor | BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::ShiftRight | BinaryOp::ShiftLeft) => {
                r.transfer(left).genExpressionExpecting(Int)?;
                r.transfer(right).genExpressionExpecting(Int)?;

                let op = match op {
                    BinaryOp::ShiftLeft => OpCode::ShiftLeft,
                    BinaryOp::ShiftRight => OpCode::ShiftRight,
                    BinaryOp::BitwiseOr => OpCode::BitwiseOr,
                    BinaryOp::BitwiseAnd => OpCode::BitwiseAnd,
                    BinaryOp::Xor => OpCode::Xor,
                    _ => unreachable!()
                };

                r.push(op)
            }

            RawExpression::BinaryOperation { left, right, op } => {
                let dat = r.transfer(left).toDataTypeNotVoid()?;
                let dat1 = r.transfer(right).toDataTypeNotVoid()?;


                if dat1.isNull() || dat.isNull() {
                    r.transfer(left).genExpression()?;
                    r.transfer(right).genExpression()?;
                } else {
                    r.transfer(left).genExpressionUnbox()?;
                    r.transfer(right).genExpressionUnbox()?;
                }

                if dat.isString() {
                    let f = r.ctx.findFunction("concat", &[DataType::str(), DataType::str()])?;
                    r.push(f.callInstruction());
                    return Ok(());
                }

                let rawT = dat.clone().toRawType()?;

                let ts = match op {
                    BinaryOp::Add => vec![OpCode::Add(rawT)],
                    BinaryOp::Sub => vec![OpCode::Sub(rawT)],
                    BinaryOp::Mul => vec![OpCode::Mul(rawT)],
                    BinaryOp::Div => vec![OpCode::Div(rawT)],
                    BinaryOp::Gt => vec![OpCode::Greater(rawT)],
                    BinaryOp::Less => vec![OpCode::Less(rawT)],
                    BinaryOp::Eq => {
                        println!("doing eq {:?} {:?} {:?}", rawT, dat, dat1);

                        vec![OpCode::Equals(rawT)]
                    }
                    BinaryOp::Modulo => vec![OpCode::Modulo(rawT)],
                    BinaryOp::NotEq => vec![OpCode::Equals(rawT), Not],
                    _ => unreachable!()
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
            }
            RawExpression::BoolLiteral(i) => r.push(PushBool(*i)),
            RawExpression::Variable(v) => {
                match r.ctx.getLocal(v) {
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
            }
            RawExpression::CharLiteral(c) => r.push(PushChar(*c)),
            RawExpression::ArrayLiteral(i) => {
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
                    return Ok(());
                } else if d.isArray() {
                    r.transfer(&i.expr).genExpression()?;
                    r.transfer(&i.index).genExpression()?;

                    r.push(ArrayLoad);

                    return Ok(());
                }

                return Err(CodeGenError::ExpectedReference);
            }
            RawExpression::NotExpression(e, _) => {
                r.transfer(e).genExpression()?;
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
                let mut argExpressions = args.clone();

                let mut argTypes = args
                    .iter()
                    .map(|it| r.transfer(it).toDataType())
                    .collect::<Result<Vec<_>, _>>()?;

                let mut funcMeta: Option<FunctionSymbol> = None;

                let expectedTypes = match &prev.exp {
                    RawExpression::Variable(v) => {
                        if r.ctx.isVariableParts(&[v.clone()]) {
                            let (args, prevT) = r.transfer(prev).toDataType()?.getFunction()?;

                            args
                        } else {
                            let func = r.ctx.findFunction(v, &argTypes)?;

                            funcMeta = Some(func.clone());

                            func.args.clone()
                        }
                    }
                    RawExpression::NamespaceAccess(n) => {
                        if r.ctx.isVariableParts(n) {
                            let (args, prevT) = r.transfer(prev).toDataType()?.getFunction()?;

                            args
                        } else {
                            let func = r.ctx.findFunctionParts(n, &argTypes)?;

                            funcMeta = Some(func.clone());

                            func.args.clone()
                        }
                    }

                    RawExpression::FieldAccess(obj, fieldName) => {
                        if let Ok(t) = r.transfer(prev).toDataType() && let Ok(v) = t.getFunction() {
                            v.0
                        } else {
                            let prevT = r.transfer(obj).toDataTypeNotVoid()?;

                            argExpressions.insert(0, *obj.clone());

                            argTypes.insert(0, prevT);

                            let func = r.ctx.findFunction(fieldName, &argTypes)?;

                            funcMeta = Some(func.clone());

                            func.args.clone()
                        }
                    }
                    _ => panic!()
                };

                for ((src, dest), exp) in argTypes.into_iter().zip(expectedTypes).zip(argExpressions) {
                    // FIXME
                    r.ctx.implicitConversion(src, dest, &exp)?;
                }

                match funcMeta {
                    None => {
                        // FIXME not sure if this is working
                        let f = r.transfer(prev).toDataType()?.getFunction()?;
                        r.transfer(prev).genExpression()?;

                        r.push(DynamicCall { returns: !f.1.isVoid(), argsCount: f.0.len() })
                    }
                    Some(v) => {
                        r.push(v.callInstruction())
                    }
                }
            },
            RawExpression::StructInit(name, init) => {
                let s = r.ctx.getStructParts(name)?;

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
                    r.transfer(value).genExpression()?;
                    r.push(SetField {
                        fieldID,
                    })
                }
            }
            RawExpression::FieldAccess(prev, fieldName) => {
                r.transfer(prev).genExpression()?;

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
            RawExpression::Null => r.push(PushNull),
            RawExpression::TernaryOperator(cond, tr, fal) => {
                let t = r.toDataType()?;

                let falseLabel = r.ctx.nextLabel();
                let endLabel = r.ctx.nextLabel();

                r.transfer(cond).genCondition()?;

                r.ctx.opJmp(falseLabel, False);

                // FIXME this should propably be gen expecting but it causes segfault investigate
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
                } else if src.isInt() && target.isFloat() {
                    r.transfer(exp).genExpression()?;
                    r.push(I2F);
                } else if target.isInt() && src.isFloat() {
                    r.transfer(exp).genExpression()?;
                    r.push(F2I);
                } else if src.isReferenceNonNullable() && target.isObjectNonNullable() {
                    r.transfer(exp).genExpression()?;
                } else {
                    todo!("src: {:?} tgt: {:?}", src, target);
                }
            }
            RawExpression::FormatStringLiteral(_) => todo!(),
            RawExpression::TypeCheck(source, target) => {
                let src = r.transfer(source).toDataType()?;

                if src.isObject() && let DataType::Reference(_) = target {
                    r.transfer(source).genExpression()?;

                    let t = r.ctx.getStruct(&target.toString())?;

                    r.push(IsStruct { namespaceId: t.nId, structId: t.sId })
                } else {
                    r.push(PushBool(&src == target))
                }
            }
            RawExpression::Negate(e) => {
                let t = r.transfer(e).toDataType()?;

                if t.isFloat() {
                    r.push(OpCode::PushFloat(0.0));
                    r.transfer(e).genExpression()?;
                    r.push(Sub(RawDataType::Float));
                } else if t.isInt() {
                    r.push(OpCode::PushInt(0));
                    r.transfer(e).genExpression()?;
                    r.push(Sub(RawDataType::Int));
                } else {
                    unreachable!("invalid type, type should be float or int")
                }
            }
            RawExpression::BitwiseNot(e) => {
                r.transfer(e).genExpressionExpecting(Int)?;

                r.push(OpCode::BitwiseNot)
            }
            RawExpression::NullAssert(e) => {
                r.transfer(e).genExpression()?;

                r.push(Dup);
                r.push(AssertNotNull);
            }
            RawExpression::Elvis(a, b) => {
                let endLabel = r.ctx.nextLabel();

                r.transfer(a).genExpression()?;

                r.push(Dup);
                r.push(PushInt(0));
                r.push(OpCode::Equals(RawDataType::Int));
                r.ctx.opJmp(endLabel, False);
                r.push(Pop);
                r.transfer(b).genExpressionBox()?;
                r.ctx.makeLabel(endLabel);
            }
        }
    }
    Ok(())
}
