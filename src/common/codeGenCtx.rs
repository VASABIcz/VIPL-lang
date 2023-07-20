use std::cell::UnsafeCell;
use std::mem::swap;

use crate::ast::{ASTNode, BinaryOp, Expression, RawExpression, Statement};
use crate::errors::{CodeGenError, SymbolNotFoundE, TypeError};
use crate::errors::CodeGenError::UnexpectedVoid;
use crate::symbolManager::{FunctionSymbol, GlobalSymbol, StructSymbol, SymbolManager};
use crate::utils::{genNamespaceName, swapChain};
use crate::vm::dataType::{DataType, Generic, ObjectMeta};
use crate::vm::dataType::DataType::{Bool, Char, Float, Int, Null, Reference, Void};
use crate::vm::namespace::Namespace;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

#[derive(Debug, Clone)]
pub struct Body {
    pub statements: Vec<Statement>,
    pub isLoop: bool,
}

impl Body {
    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }

    pub fn new(b: Vec<Statement>, isLoop: bool) -> Self {
        Self { statements: b, isLoop }
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

impl<T> SimpleCtx<'_, T> {
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

impl<T> ExpressionCtx<'_, T> {
    pub fn clone(&mut self) -> ExpressionCtx<T> {
        ExpressionCtx {
            exp: self.exp,
            ctx: self.ctx.transfer(),
        }
    }

    pub fn assertType(&mut self, t: DataType) -> Result<(), CodeGenError> {
        let t1 = self.toDataType()?;

        // FIXME not sure if this is the right thing to do
        if !t.canAssign(&t1) {
            return Err(CodeGenError::TypeError(TypeError::new(t, t1, self.exp.clone())));
        }

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
                            return Ok(Int)
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

                if let Some(v) = swapChain(&a, &b, |a, b| { if (a.canAssign(b)) { Some(a.clone()) } else { None } }) {
                    return Ok(v)
                }
                if let Some(v) = swapChain(&a, &b, |a, b| { if  b.isReferenceNonNullable() && a.isNull() { Some(b.clone().toNullable()) } else { None } }) {
                    return Ok(v)
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

                if a.canAssign(&b) && let Ok(v) = a.getReffM() {
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
    pub expectedReturn: DataType
}

impl<T> StatementCtx<'_, T> {
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