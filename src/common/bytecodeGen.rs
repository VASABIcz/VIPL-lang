use std::collections::HashMap;

use crate::ast::{ArithmeticOp, BinaryOp, RawExpression, RawStatement};
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::codeGenCtx::{ExpressionCtx, SimpleCtx, StatementCtx};
use crate::errors::{CodeGenError, SymbolNotFoundE};
use crate::errors::CodeGenError::LiteralParseError;
use crate::implicitConverter::getImplicitConverter;
use crate::lexer::*;
use crate::parser::*;
use crate::symbolManager::FunctionSymbol;
use crate::utils::microsSinceEpoch;
use crate::vm::dataType::{DataType, RawDataType};
use crate::vm::dataType::DataType::{Bool, Float, Int, Reference, Void};
use crate::vm::namespace::{FunctionMeta, FunctionTypeMeta};
use crate::vm::vm::{JmpType, OpCode};
use crate::vm::vm::JmpType::{False, True};
use crate::vm::vm::OpCode::{Add, And, ArrayLength, ArrayLoad, ArrayNew, ArrayStore, AssertNotNull, Div, Dup, DynamicCall, F2I, GetChar, GetField, GetLocal, Greater, I2F, IsStruct, LCall, Mul, New, Not, Pop, PushBool, PushChar, PushFloat, PushInt, PushNull, Return, SCall, SetField, SetGlobal, SetLocal, StringLength, StrNew, Sub, Swap};

const DEBUG: bool = true;

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

    for sym in syms.iter() {
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

    pub fn genExpressionExpecting(mut self, tgt: DataType) -> Result<(), CodeGenError> {
        let src = self.toDataType()?;
        self.clone().genExpression()?;
        self.ctx.genConversion(src, tgt, getImplicitConverter())
    }

    pub fn genExpressionExpectingLike<F: Fn(&DataType) -> bool>(mut self, tgt: F) -> Result<DataType, CodeGenError> {
        let src = self.toDataType()?;
        self.clone().genExpression()?;
        self.ctx.genConversionAbstract(src, tgt, getImplicitConverter())
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
            y.isLast = i == body.statements.len() - 1;
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
            let (_, loopEnd) = ctx.ctx.beginLoop();

            ctx.ctx.makeExpressionCtx(&w.exp).genCondition()?;

            ctx.ctx.opJmp(loopEnd, False);

            w.body.generateWithStatement(&mut ctx, genStatement)?;

            ctx.opContinue()?;

            ctx.ctx.endLoop()?;
        }
        RawStatement::If(flow) => {
            let endLabel = ctx.ctx.nextLabel();

            let mut ifLabels = vec![];

            for _ in 0..flow.elseIfs.len() {
                ifLabels.push(ctx.ctx.nextLabel());
            }

            if flow.elseBody.is_some() {
                ifLabels.push(ctx.ctx.nextLabel());
            }

            ctx.ctx.makeExpressionCtx(&flow.condition).genCondition()?;
            if let Some(v) = ifLabels.first() {
                ctx.ctx.opJmp(*v, False);
            } else {
                ctx.ctx.opJmp(endLabel, False)
            }

            flow.body.generateWithStatement(&mut ctx, genStatement)?;

            ctx.ctx.opJmp(endLabel, JmpType::Jmp);

            for (i, (elsExp, elsBody)) in flow.elseIfs.iter().enumerate() {
                ctx.ctx.makeLabel(*ifLabels.get(i).ok_or_else(|| CodeGenError::VeryBadState)?);

                ctx.ctx.makeExpressionCtx(&elsExp).genCondition()?;

                if let Some(v) = ifLabels.get(i + 1) {
                    ctx.ctx.opJmp(*v, False);
                } else {
                    ctx.ctx.opJmp(endLabel, False);
                }

                elsBody.generateWithStatement(&mut ctx, genStatement)?;
                ctx.ctx.opJmp(endLabel, JmpType::Jmp);
            }

            if let Some(els) = &flow.elseBody {
                ctx.ctx.makeLabel(*ifLabels.last().ok_or_else(|| CodeGenError::VeryBadState)?);
                els.generateWithStatement(&mut ctx, genStatement)?;
            }
            ctx.ctx.makeLabel(endLabel);
        }
        RawStatement::Return(ret) => {
            if let Some(ret) = ret {
                ctx.ctx.makeExpressionCtx(&ret).genExpressionExpecting(ctx.expectedReturn.clone())?;
            }
            ctx.ctx.push(Return)
        }
        RawStatement::Continue => ctx.opContinue()?,
        RawStatement::Break => ctx.opBreak()?,
        RawStatement::Loop(body) => {
            ctx.ctx.beginLoop();

            body.generateWithStatement(&mut ctx, genStatement)?;

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
        RawStatement::Assignable(destExp, valueExp, operator, typeHint) => {
            let dest = ctx.ctx.makeExpressionCtx(destExp).toDataType();

            let srcType = ctx.ctx.makeExpressionCtx(valueExp).toDataType()?;

            if let Ok(destType) = dest {
                if !ctx.deflate().canConvert(&srcType, &destType) || destType.isVoid() || srcType.isVoid() {
                    return Err(CodeGenError::AssignableTypeError {
                        var: destExp.clone(),
                        varType: destType,
                        exp: valueExp.clone(),
                        expType: srcType,
                    });
                }
            }

            // FIXME this looks kinda sus
            match &destExp.exp {
                RawExpression::Variable(v) => {
                    match ctx.ctx.getLocal(v) {
                        Ok(v) => {
                            let d = v.0.clone();
                            ctx.ctx.makeExpressionCtx(valueExp).assertType(d)?;
                        }
                        Err(_) => {
                            let t = ctx.ctx.makeExpressionCtx(valueExp).toDataType()?;

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
                    ctx.ctx.makeExpressionCtx(valueExp).genExpression()?;
                }
                Some(v) => {
                    let mut c1 = ctx.ctx.makeExpressionCtx(destExp);
                    let t = c1.toDataType()?;
                    // FIXME make generic function that generates arithmetics THIS IS BAD!
                    c1.genExpression()?;
                    ctx.ctx.makeExpressionCtx(valueExp).genExpression()?;
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

            match &destExp.exp {
                RawExpression::Variable(v) => {
                    let var = ctx.ctx.getLocal(v)?.1;

                    ctx.ctx.push(SetLocal { index: var })
                }
                RawExpression::ArrayIndexing(v) => {
                    ctx.ctx.makeExpressionCtx(&v.index).genExpressionExpecting(Int)?;

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

            ctx.ctx.makeExpressionCtx(arr).genExpressionExpectingLike(|it| it.isArray())?;
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

            body.generateWithStatement(&mut ctx, genStatement)?;

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

            body.generateWithStatement(&mut ctx, genStatement)?;

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
            RawExpression::BinaryOperation { left, right, op } if matches!(op, BinaryOp::And | BinaryOp::Or) => {
                let isAnd = op == &BinaryOp::And;

                let endLabel = r.ctx.nextLabel();

                r.transfer(left).genCondition()?;

                r.push(Dup);
                r.ctx.opJmp(endLabel, if isAnd { False } else { True });
                r.transfer(right).genCondition()?;
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
                // FIXME !!!!! this is huge mess it would be best to refactor it to smaller parts
                // FIXME it looks like we dont even support string comparison :)

                let dat = r.transfer(left).toDataTypeNotVoid()?;
                let dat1 = r.transfer(right).toDataTypeNotVoid()?;

                if dat.isString() || dat1.isString() {
                    r.transfer(left).genExpressionExpecting(DataType::str())?;
                    r.transfer(right).genExpressionExpecting(DataType::str())?;

                    match op {
                        BinaryOp::Add => {
                            let f = r.ctx.findFunction("concat", &[DataType::str(), DataType::str()])?;
                            r.push(f.callInstruction());
                        }
                        BinaryOp::Eq => {
                            todo!("planed implementation of string eq")
                        }
                        BinaryOp::NotEq => {
                            todo!("planed implementation string neq")
                        }
                        _ => panic!()
                    }
                    return Ok(());
                }


                if dat1.isReferenceLike() || dat.isReferenceLike() {
                    r.transfer(left).genExpressionExpectingLike(|it| it.isReferenceLike())?;
                    r.transfer(right).genExpressionExpectingLike(|it| it.isReferenceLike())?;

                    match op {
                        BinaryOp::Eq => {
                            r.push(OpCode::Equals(RawDataType::Int))
                        }
                        BinaryOp::NotEq => {
                            r.push(OpCode::Equals(RawDataType::Int));
                            r.push(Not)
                        }
                        _ => panic!()
                    }

                    return Ok(());
                }

                let finalTypA = r.transfer(left).genExpressionExpectingLike(|it| it.isPrimitiveType())?;
                let finalTypB = r.transfer(right).genExpressionExpectingLike(|it| it.isPrimitiveType())?;
                let isOneFloat = finalTypA.isFloat() || finalTypB.isFloat();
                let mut rawT = finalTypA.toRawType()?;

                if isOneFloat {
                    // FIXME this is pretty / slow
                    if finalTypB.isFloat() {
                        r.push(Swap);
                        r.push(I2F);
                        r.push(Swap);
                    } else {
                        r.push(I2F);
                    }
                    rawT = RawDataType::Float;
                }

                let ts = match op {
                    BinaryOp::Add => vec![OpCode::Add(rawT)],
                    BinaryOp::Sub => vec![OpCode::Sub(rawT)],
                    BinaryOp::Mul => vec![OpCode::Mul(rawT)],
                    BinaryOp::Div => vec![OpCode::Div(rawT)],
                    BinaryOp::Gt => vec![OpCode::Greater(rawT)],
                    BinaryOp::Less => vec![OpCode::Less(rawT)],
                    BinaryOp::Eq => vec![OpCode::Equals(rawT)],
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
                let t = if let Some(v) = i.get(0) {
                    Some(r.transfer(v).toDataType()?)
                } else {
                    None
                };

                r.push(PushInt(i.len() as isize));
                r.push(ArrayNew);
                for (ind, exp) in i.iter().enumerate() {
                    r.push(Dup);
                    if let Some(t1) = t.clone() {
                        r.transfer(exp).genExpressionExpecting(t1)?;
                    } else {
                        r.transfer(exp).genExpression()?;
                    }
                    r.push(PushInt(ind as isize));
                    r.push(ArrayStore);
                }
            }
            RawExpression::ArrayIndexing(i) => {
                let d = r.transfer(&i.expr).toDataTypeNotVoid()?;

                r.transfer(&i.expr).genExpressionExpectingLike(|it| it.isArray())?;
                r.transfer(&i.index).genExpressionExpecting(Int)?;
                if d.isString() {
                    r.push(GetChar);
                    return Ok(());
                } else if d.isArray() {
                    r.push(ArrayLoad);

                    return Ok(());
                }

                return Err(CodeGenError::ExpectedReference);
            }
            RawExpression::NotExpression(e, _) => {
                r.transfer(e).genCondition()?;
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
            RawExpression::Callable(prev, args) => {
                // TODO create better function detection using implicit conversions

                let mut argExpressions = args.clone();

                let mut argTypes = r.ctx.argsToDataTypes(args)?;

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
                    r.transfer(&exp).genExpressionExpecting(dest)?;
                }

                match funcMeta {
                    None => {
                        // FIXME not sure if this is working
                        let f = r.transfer(prev).toDataType()?.getFunction()?;
                        r.transfer(prev).genExpressionExpectingLike(|it| it.isFunction())?;

                        r.push(DynamicCall { returns: !f.1.isVoid(), argsCount: f.0.len() })
                    }
                    Some(v) => {
                        r.push(v.callInstruction())
                    }
                }
            }
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
                    let (var, fieldID) = s1.meta.findField(fieldName)?;
                    let fieldType = var.typ.clone();
                    r.transfer(value).genExpressionExpecting(fieldType)?;
                    r.push(SetField {
                        fieldID,
                    })
                }
            }
            RawExpression::FieldAccess(prev, fieldName) => {
                r.transfer(prev).genExpressionExpectingLike(|it| it.isReferenceNonNullable())?;

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

                r.transfer(tr).genExpressionExpecting(t.clone())?;
                r.ctx.opJmp(endLabel, JmpType::Jmp);

                r.ctx.makeLabel(falseLabel);
                r.transfer(fal).genExpressionExpecting(t)?;

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

                if r.ctx.canConvert(&t, &Int) {
                    r.push(PushInt(0));
                    r.transfer(e).genExpressionExpecting(Int)?;
                    r.push(Sub(RawDataType::Int));
                } else {
                    r.push(PushFloat(0.0));
                    r.transfer(e).genExpressionExpecting(Float)?;
                    r.push(Sub(RawDataType::Float));
                }
                println!("FINISH")
            }
            RawExpression::BitwiseNot(e) => {
                r.transfer(e).genExpressionExpecting(Int)?;

                r.push(OpCode::BitwiseNot)
            }
            RawExpression::NullAssert(e) => {
                r.transfer(e).genExpressionExpectingLike(|it| it.isReference() || it.isObject())?;

                r.push(Dup);
                r.push(AssertNotNull);
            }
            RawExpression::Elvis(a, b) => {
                let endLabel = r.ctx.nextLabel();

                let t = r.transfer(a).genExpressionExpectingLike(|it| it.isReference() || it.isObject())?;

                r.push(Dup);
                r.push(PushInt(0));
                r.push(OpCode::Equals(RawDataType::Int));
                r.ctx.opJmp(endLabel, False);
                r.push(Pop);
                r.transfer(b).genExpressionExpecting(t)?;
                r.ctx.makeLabel(endLabel);
            }
        }
    }
    Ok(())
}
