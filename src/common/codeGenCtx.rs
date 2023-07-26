use std::cell::UnsafeCell;
use std::collections::HashSet;
use std::fmt::Debug;

use crate::ast::{ASTNode, BinaryOp, Expression, RawExpression, RawStatement, Statement};
use crate::errors::{CodeGenError, SymbolNotFoundE, TypeError};
use crate::errors::CodeGenError::UnexpectedVoid;
use crate::implicitConverter::{getImplicitConverter, ImplicitConverter};
use crate::symbolManager::{FunctionSymbol, GlobalSymbol, StructSymbol, SymbolManager};
use crate::utils::{genNamespaceName, swapChain};
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::dataType::DataType::{Bool, Char, Float, Int, Null, Reference, Void};
use crate::vm::namespace::{FunctionMeta, Namespace};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

const DEBUG: bool = false;

#[derive(Debug, Clone)]
pub struct Body {
    pub statements: Vec<Statement>,
    pub isLoop: bool,
}

impl Body {
    pub fn generate<T: Debug, FN: Fn(StatementCtx<T>) -> Result<(), CodeGenError>>(
        &self,
        mut ctx: SimpleCtx<T>,
        expectedReturn: DataType,
        f: FN
    ) -> Result<(), CodeGenError> {
        ctx.enterScope();
        for (i, statement) in self.statements.iter().enumerate() {
            let mut y = ctx.inflate(statement);
            y.expectedReturn = expectedReturn.clone();
            y.isLast = i == self.statements.len() - 1;
            f(y)?;
        }
        ctx.exitScope();

        Ok(())
    }

    pub fn generateWithStatement<T: Debug, FN: Fn(StatementCtx<T>) -> Result<(), CodeGenError>>(
        &self,
        ctx: &mut StatementCtx<T>,
        f: FN
    ) -> Result<(), CodeGenError> {
        let ret = ctx.expectedReturn.clone();
        self.generate(ctx.deflate(), ret, f)
    }

    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }

    pub fn new(b: Vec<Statement>, isLoop: bool) -> Self {
        Self { statements: b, isLoop }
    }

    pub fn validate<T: Debug>(&self, mut ctx: SimpleCtx<T>, fun: &FunctionMeta) -> Result<(), CodeGenError> {
        let mut s = HashSet::new();

        let isCovered = self.coverageCheck(ctx.transfer(), &mut s)?;

        if fun.returnType.isVoid() && s.is_empty() {
            return Ok(());
        }

        if (!isCovered || s.is_empty()) && (s.len() == 1 && !s.contains(&Void)) {
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
                eprintln!("more than 1 return ${:?}", s)
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

        if fun.returnType != Void && !ctx.canConvert(tt, &fun.returnType) {
            return Err(CodeGenError::TypeError(TypeError::newNone(fun.returnType.clone(), tt.clone())));
        }

        Ok(())
    }

    pub fn coverageCheck<T: Debug>(&self, mut ctx: SimpleCtx<T>, r: &mut HashSet<DataType>) -> Result<bool, CodeGenError> {
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

    pub fn returnType<T: Debug>(&self, mut ctx: SimpleCtx<T>, r: &mut HashSet<DataType>) -> Result<(), CodeGenError> {
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

#[derive(Debug, Default, Clone)]
pub struct LabelManager {
    labelCounter: usize,
    loopContext: Vec<(usize, usize)>,
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

#[derive(Debug)]
pub struct SimpleCtx<'a, T> {
    ops: &'a mut Vec<T>,
    currentNamespace: &'a mut UnsafeCell<Namespace>,
    vm: &'a mut UnsafeCell<VirtualMachine>,
    handle: fn(&mut StatementCtx<T>, DataType) -> (),
    labels: &'a mut LabelManager,
    symbols: &'a mut SymbolManager,
}

impl<T: Debug> SimpleCtx<'_, T>  {
    pub fn genConversion(&mut self, src: DataType, tgt: DataType, converter: &ImplicitConverter<T>) -> Result<(), CodeGenError> {
        // FIXME [gen] converting Int to Void using [BoxConverter { ac: 604 }, ObjectConverter { ac: 600 }, ObjNullifyConverter { ac: 601 }]
        if tgt.isVoid() {
            return Err(CodeGenError::VeryBadState);
        }
        // FIXME proper errors
        let converters = converter.findConversionChain(&src, &tgt).expect(&format!("failed to convert {:?} to {:?}", src, tgt));

        println!("[gen] converting {:?} to {:?} using {:?}", src, tgt, converters);

        for converter in converters {
            // FIXME this doesnt work bcs we dont provide the actual data type, it could be deduced tho
            converter.genConvert(src.clone(), tgt.clone(), self.transfer())?;
        }

        Ok(())
    }
}

impl<T: Debug> SimpleCtx<'_, T> {
    pub fn genConversionAbstract<F: Fn(&DataType) -> bool>(&mut self, src: DataType, tgt: F, converter: &ImplicitConverter<T>) -> Result<DataType, CodeGenError> {
        // FIXME proper errors
        let (converters, finalType) = converter.findAbstractConversion(src.clone(), tgt).expect(&format!("failed to convert {:?} to specified rule", src));

        for converter in converters {
            // FIXME this doesnt work bcs we dont provide the actual data type, it could be deduced tho
            converter.genBlindConvert(src.clone(), self.transfer())?;
        }

        Ok(finalType)
    }

    pub fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        getImplicitConverter().findConversionChain(src, tgt).is_some()
    }

    pub fn argsToDataTypes(&mut self, args: &[Expression]) -> Result<Vec<DataType>, CodeGenError> {
        args
            .iter()
            .map(|it| self.makeExpressionCtx(it).toDataType())
            .collect::<Result<Vec<_>, _>>()
    }

    pub fn metaToDataTypes(&mut self, args: &[VariableMetadata]) -> Vec<DataType> {
        args
            .iter()
            .map(|it| it.typ.clone())
            .collect::<Vec<_>>()
    }

    pub fn isVariableParts(&mut self, name: &[String]) -> bool {
        if name.is_empty() {
            return false;
        }

        if name.len() > 1 {
            return self.findGlobalParts(name).is_ok();
        }

        return self.getLocal(&name[0]).is_ok() || self.findGlobalParts(name).is_ok();
    }

    pub fn getVMMut(&mut self) -> &mut VirtualMachine {
        self.vm.get_mut()
    }

    pub fn getVM(&self) -> &VirtualMachine {
        unsafe { &*self.vm.get() }
    }

    pub fn enterLoop(&mut self) -> (usize, usize) {
        self.labels.enterLoop()
    }

    pub fn exitLoop(&mut self) {
        self.labels.exitLoop()
    }

    pub fn getNamespace(&mut self) -> &mut Namespace {
        self.currentNamespace.get_mut()
    }

    pub fn pushInstruction(&mut self, i: T) {
        self.ops.push(i)
    }

    pub fn getContext(&self) -> Option<(usize, usize)> {
        self.labels.getContext()
    }

    pub fn allocateOrGetString(&mut self, i: &str) -> usize {
        self.currentNamespace.get_mut().allocateOrGetString(i)
    }

    pub fn getSymbols(&self) -> &SymbolManager {
        self.symbols
    }

    pub fn setSymbols(&mut self, symbols: SymbolManager) {
        *self.symbols = symbols
    }

    pub fn getHandle(&self) -> fn(&mut StatementCtx<T>, DataType) -> () {
        self.handle
    }

    pub fn findGlobalParts(&self, name: &[String]) -> Result<&GlobalSymbol, CodeGenError> {
        self.symbols.getGlobal(&genNamespaceName(name))
    }

    pub fn new<'b>(
        currentNamespace: &'b mut UnsafeCell<Namespace>,
        vm: &'b mut UnsafeCell<VirtualMachine>,
        handle: fn(&mut StatementCtx<T>, DataType) -> (),
        symbols: &'b mut SymbolManager,
        labels: &'b mut LabelManager,
        ops: &'b mut Vec<T>,
        implicitConverter: &'b ImplicitConverter<T>,
    ) -> SimpleCtx<'b, T> {
        SimpleCtx {
            ops,
            currentNamespace,
            vm,
            handle,
            labels,
            symbols,
        }
    }

    pub fn inflate<'a>(
        &'a mut self,
        statement: &'a Statement,
    ) -> StatementCtx<'a, T> {
        StatementCtx {
            statement: &statement,
            ctx: self.transfer(),
            isLast: false,
            expectedReturn: Void,
        }
    }

    pub fn transfer<'a>(&mut self) -> SimpleCtx<T> {
        SimpleCtx {
            ops: self.ops,
            currentNamespace: self.currentNamespace,
            vm: self.vm,
            handle: self.handle,
            labels: self.labels,
            symbols: self.symbols,
        }
    }

    pub fn isCurrentNamespace(&self, nId: usize) -> bool {
        unsafe { (*self.currentNamespace.get()).id == nId }
    }

    pub fn nextLabel(&mut self) -> usize {
        self.labels.nextLabel()
    }

    pub fn findFunction(&self, name: &str, args: &[DataType]) -> Result<&FunctionSymbol, CodeGenError> {
        self.symbols.getFunctionArgs(name, args)
    }

    pub fn findFunctionParts(&self, name: &[String], args: &[DataType]) -> Result<&FunctionSymbol, CodeGenError> {
        self.symbols.getFunctionPartsArgs(name, args)
    }

    pub fn getLocal(&self, varName: &str) -> Result<&(DataType, usize), CodeGenError> {
        self.symbols.getLocal(varName)
    }

    pub fn hasField(&self, t: DataType) -> bool {
        todo!()
    }

    pub fn lookupFunctionByBaseName(&self, name: &str) -> Result<(&String, &FunctionSymbol), CodeGenError> {
        Ok(*self.symbols.getFunctionsByBaseName(name).first().ok_or_else(|| CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)))?)
    }

    pub fn registerVariable(&mut self, name: &str, t: DataType) -> usize {
        self.symbols.registerLocal(name, t).1
    }

    pub fn registerVariableIfNotExists(&mut self, name: &str, t: DataType) -> (DataType, usize) {
        if let Ok(v) = self.symbols.getLocal(name) {
            v.clone()
        } else {
            self.symbols.registerLocal(name, t)
        }
    }

    pub fn makeExpressionCtx<'a>(
        &'a mut self,
        exp: &'a Expression,
    ) -> ExpressionCtx<T> {
        ExpressionCtx {
            exp,
            ctx: self.transfer(),
        }
    }

    pub fn getLocals(&self) -> &[VariableMetadata] {
        &self.symbols.locals
    }

    pub fn getStruct(&self, name: &str) -> Result<&StructSymbol, CodeGenError> {
        self.symbols.getStruct(name)
    }

    pub fn getStructParts(&self, parts: &[String]) -> Result<&StructSymbol, CodeGenError> {
        self.symbols.getStruct(&genNamespaceName(parts))
    }

    pub fn getOps(&self) -> &[T] {
        self.ops
    }

    pub fn enterScope(&mut self) {
        self.symbols.enterScope();
    }

    pub fn exitScope(&mut self) {
        self.symbols.exitScope();
    }
}

#[derive(Debug)]
pub struct ExpressionCtx<'a, T> {
    pub exp: &'a Expression,
    pub ctx: SimpleCtx<'a, T>,
}

impl<T: Debug> ExpressionCtx<'_, T> {
    pub fn clone(&mut self) -> ExpressionCtx<T> {
        ExpressionCtx {
            exp: self.exp,
            ctx: self.ctx.transfer(),
        }
    }

    pub fn assertType(&mut self, t: DataType) -> Result<(), CodeGenError> {
        let t1 = self.toDataType()?;

        // FIXME not sure if this is the right thing to do
        /*        self.ctx.makeExpressionCtx(self).
                if !t.canAssign(&t1) {
                    return Err(CodeGenError::TypeError(TypeError::new(t, t1, self.exp.clone())));
                }*/

        Ok(())
    }

    pub fn transfer<'a>(&'a mut self, exp: &'a Expression /*typeHint: Option<DataType>*/) -> ExpressionCtx<T> {
        ExpressionCtx {
            exp,
            ctx: self.ctx.transfer(),
        }
    }

    pub fn toDataTypeNotVoid(&mut self) -> Result<DataType, CodeGenError> {
        let t = self.toDataType()?;

        if t.isVoid() {
            return Err(UnexpectedVoid(ASTNode::Expr(self.exp.clone())));
        }

        Ok(t)
    }

    pub fn toDataTypeAssertEq(&mut self, f: fn(&DataType) -> bool) -> Result<DataType, CodeGenError> {
        let t = self.toDataType()?;
        if !f(&t) {
            panic!()
        }
        Ok(t)
    }

    pub fn toDataTypeAssert(&mut self, t1: DataType) -> Result<DataType, CodeGenError> {
        let t = self.toDataType()?;

        if t != t1 {
            return Err(CodeGenError::TypeError(TypeError::new(t1, t, self.exp.clone())));
        }

        Ok(t)
    }

    pub fn toDataType(&mut self) -> Result<DataType, CodeGenError> {
        match &(self.exp).exp {
            RawExpression::BinaryOperation {
                left,
                right,
                op: o,
            } => {
                let leftT = self.transfer(left).toDataType()?;
                let rightT = self.transfer(right).toDataType()?;

                if !left.isNull() && !right.isNull() && leftT != rightT {
                    return Err(CodeGenError::TypeError(TypeError::new(leftT, rightT, self.exp.clone())));
                }

                let t = match o {
                    BinaryOp::Gt | BinaryOp::Less => {
                        assert!(rightT.supportsComparisson());

                        Bool
                    }
                    BinaryOp::Eq | BinaryOp::NotEq => {
                        assert!(rightT.supportsEquals());

                        Bool
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        assert!(rightT.isBoolLike());

                        Bool
                    }
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Modulo => {
                        if o == &BinaryOp::Add && rightT.isString() {
                            return Ok(DataType::str());
                        }

                        assert!(rightT.supportsArithmetics());

                        leftT.toUnboxedType()
                    }
                    BinaryOp::Div => {
                        assert!(rightT.supportsArithmetics());

                        Float
                    }
                    BinaryOp::BitwiseAnd | BinaryOp::Xor | BinaryOp::BitwiseOr | BinaryOp::ShiftRight | BinaryOp::ShiftLeft => {
                        assert!(rightT.isBoolLike());

                        Int
                    }
                };

                Ok(t)
            }
            RawExpression::IntLiteral(_) => Ok(Int),
            RawExpression::FloatLiteral(_) => Ok(Float),
            RawExpression::StringLiteral(_) => Ok(DataType::str()),
            RawExpression::Variable(name) => match self.ctx.getLocal(name) {
                Err(_) => {
                    let (_, fun) = self.ctx.lookupFunctionByBaseName(name)?;

                    Ok(DataType::Function { args: fun.args.clone(), ret: Box::new(fun.returnType.clone()) })
                }
                Ok(v) => {
                    Ok(v.0.clone())
                }
            },
            RawExpression::BoolLiteral(_) => Ok(Bool),
            RawExpression::CharLiteral(_) => Ok(Char),
            RawExpression::ArrayLiteral(e) => {
                let t = self
                    .transfer(e.get(0).ok_or_else(|| CodeGenError::UntypedEmptyArray)?)
                    .toDataTypeNotVoid()?;

                Ok(DataType::arr(Generic::Type(t)))
            }
            RawExpression::ArrayIndexing(i) => {
                let e = self.transfer(&i.expr).toDataTypeNotVoid()?;

                match e {
                    Reference(o) => {
                        if o.name.as_str() == "String" {
                            return Ok(Char);
                        }
                        Ok(o.generics
                            .first()
                            .ok_or_else(|| CodeGenError::UntypedEmptyArray)?
                            .clone()
                            .ok_or_else(|| CodeGenError::UntypedEmptyArray)?)
                    }
                    _ => {
                        Err(CodeGenError::ExpectedReference)
                    }
                }
            }
            RawExpression::NotExpression(i, _) => {
                self.transfer(i).assertType(Bool)?;

                Ok(Bool)
            }
            RawExpression::NamespaceAccess(n) => {
                let g = self.ctx.symbols.getGlobal(&genNamespaceName(n))?;

                Ok(g.typ.clone())
            }
            RawExpression::Lambda(l, _, ret) => {
                Ok(DataType::Function {
                    args: self.ctx.metaToDataTypes(l),
                    ret: Box::new(ret.clone().unwrap_or(Void)),
                })
            }
            RawExpression::Callable(prev, args) => unsafe {
                match &prev.exp {
                    RawExpression::Variable(v) => {
                        if self.ctx.getLocal(v).is_err() {
                            let gz = self.ctx.argsToDataTypes(args)?;

                            let funcId = self.ctx.symbols.getFunctionArgs(&v, &gz)?;
                            return Ok(funcId.returnType.clone());
                        }

                        let var = self.ctx.getLocal(v)?;
                        match &var.0 {
                            DataType::Function { ret, .. } => Ok(*ret.clone()),
                            _ => Err(CodeGenError::ExpectedLambda),
                        }
                    }
                    RawExpression::NamespaceAccess(v) => {
                        let argz = self.ctx.argsToDataTypes(args)?;

                        Ok(self.ctx.symbols.getFunctionPartsArgs(&v, &argz)?.returnType.clone())
                    }
                    RawExpression::FieldAccess(a, b) => {
                        let t = self.transfer(a).toDataType()?;

                        match t {
                            DataType::Function { ret, .. } => Ok(*ret),
                            t => {
                                let mut fArgs = vec![];
                                fArgs.push(t);
                                fArgs.extend(self.ctx.argsToDataTypes(args)?);

                                let f = self.ctx.findFunction(b, &fArgs)?;

                                Ok(f.returnType.clone())
                            }
                        }
                    }
                    _ => {
                        Err(CodeGenError::ExpectedCallable)
                    }
                }
            },
            RawExpression::StructInit(name, _) => Ok(Reference(ObjectMeta::nunNull(name.last().unwrap()))),
            RawExpression::FieldAccess(prev, fieldName) => {
                let e = self.transfer(prev).toDataType()?;
                match e {
                    Reference(o) => {
                        if matches!(o.name.as_str(), "Array" | "String") {
                            return Ok(Int);
                        }

                        let (structMeta, _) = self
                            .ctx
                            .currentNamespace
                            .get_mut()
                            .findStruct(o.name.as_str())?;
                        let (t, _) = structMeta.findField(fieldName)?;
                        Ok(t.typ.clone())
                    }
                    _ => {
                        Err(CodeGenError::ExpectedReference)
                    }
                }
            }
            RawExpression::Null => Ok(Null),
            RawExpression::TernaryOperator(cond, tr, fal) => {
                self.transfer(&cond).assertType(Bool)?;

                let a = self.transfer(&tr).toDataType()?;
                let b = self.transfer(&fal).toDataType()?;

                if let Some(v) = swapChain(&a, &b, |a, b| { if self.ctx.canConvert(b, a) { Some(a.clone()) } else { None } }) {
                    return Ok(v);
                }
                if let Some(v) = swapChain(&a, &b, |a, b| { if b.isReferenceNonNullable() && a.isNull() { Some(b.clone().toNullable()) } else { None } }) {
                    return Ok(v);
                }
                panic!("{:?} {:?}", a, b)
            }
            // FIXME check if cast is possible
            RawExpression::TypeCast(_, t) => Ok(t.clone()),
            RawExpression::FormatStringLiteral(_) => todo!(),
            // FIXME
            RawExpression::TypeCheck(_, _) => Ok(DataType::Bool),
            RawExpression::Negate(v) => {
                let t = self.transfer(v).toDataTypeAssertEq(|it| it.isNumeric())?;

                Ok(t)
            }
            RawExpression::BitwiseNot(v) => {
                self.transfer(v).toDataTypeAssert(Int)?;

                Ok(Int)
            }
            RawExpression::NullAssert(v) => {
                let t = self.transfer(v).toDataTypeNotVoid()?;

                let mut r = t.getRef()?;

                r.nullable = false;

                Ok(Reference(r))
            }
            RawExpression::Elvis(nullable, otherwise) => {
                let mut a = self.transfer(nullable).toDataTypeNotVoid()?;
                let b = self.transfer(otherwise).toDataTypeNotVoid()?;

                if self.ctx.canConvert(&b, &a) && let Ok(v) = a.getReffM() {
                    v.nullable = false;

                    return Ok(a);
                }

                Err(CodeGenError::TypeError(TypeError::new(a, b, self.exp.clone())))
            }
        }
    }
}

#[derive(Debug)]
pub struct StatementCtx<'a, T> {
    pub statement: &'a Statement,
    pub ctx: SimpleCtx<'a, T>,
    pub isLast: bool,
    pub expectedReturn: DataType,
}

impl<T: Debug> StatementCtx<'_, T> {
    pub fn deflate(&mut self) -> SimpleCtx<T> {
        SimpleCtx {
            ops: self.ctx.ops,
            currentNamespace: self.ctx.currentNamespace,
            vm: self.ctx.vm,
            handle: self.ctx.handle,
            labels: self.ctx.labels,
            symbols: self.ctx.symbols,
        }
    }

    pub fn transfer<'a>(&'a mut self, statement: &'a Statement) -> StatementCtx<T> {
        StatementCtx {
            statement,
            expectedReturn: self.expectedReturn.clone(),
            isLast: self.isLast,
            ctx: self.deflate(),
        }
    }
}