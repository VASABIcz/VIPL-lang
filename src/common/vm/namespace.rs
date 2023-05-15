use std::alloc::Global;
use std::arch::asm;
use std::collections::HashMap;
use std::error::Error;
use std::hint::black_box;
use std::mem::transmute;
use libc::printf;

use crate::ast::{Expression, FunctionDef, Node, BinaryOp, Statement, VariableModd};
use crate::bytecodeGen::{ExpressionCtx, genFunctionDef};
use crate::errors::{CodeGenError, Errorable, LoadFileError, SymbolNotFound, SymbolType};
use crate::fastAcess::FastAcess;
// use crate::codegen::complexBytecodeGen;
use crate::lexer::{tokenizeSource, TokenType};
use crate::parser::{Operation, parseTokens};
use crate::utils::{genFunName, genFunNameMeta, genFunNameMetaTypes, restoreRegisters, saveRegisters};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::Void;
use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::objects::Str;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::{OpCode, VirtualMachine};

#[derive(Debug, PartialEq)]
pub enum NamespaceState {
    Loaded,
    PartiallyLoaded,
}

#[derive(Debug, Clone)]
pub enum FunctionTypeMeta {
    Runtime(Vec<Statement>),
    Builtin,
    Native
}

#[derive(Debug, Clone)]
pub struct FunctionMeta {
    pub name: String,
    pub argsCount: usize,
    pub functionType: FunctionTypeMeta,
    pub localsMeta: Box<[VariableMetadata]>,
    pub returnType: DataType,
    pub isPure: bool,
    pub returns: bool
}

impl FunctionMeta {
    #[inline(always)]
    pub fn returns(&self) -> bool {
        self.returns
    }
}

#[derive(Clone, Debug)]
pub struct GlobalMeta {
    pub name: String,
    pub default: Expression,
    pub typ: DataType
}

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub name: String,
    pub fieldsLookup: HashMap<String, usize>,
    pub fields: Vec<VariableMetadata>
    // TODO default values
}

impl StructMeta {
    pub fn findField(&self, name: &str) -> Result<(&VariableMetadata, usize), CodeGenError> {
        let fieldId = self.fieldsLookup.get(name)
            .ok_or(CodeGenError::SymbolNotFound(SymbolNotFound{ name: name.to_string(), typ: SymbolType::Struct }))?;

        let field = self.fields.get(*fieldId)
            .ok_or(CodeGenError::SymbolNotFound(SymbolNotFound{ name: name.to_string(), typ: SymbolType::Struct }))?;

        Ok((field, *fieldId))
    }
}

impl FunctionMeta {
    pub fn toFunctionType(&self) -> DataType {
        DataType::Function { args: self.localsMeta.iter().map(|it|{it.typ.clone()}).collect::<Vec<_>>(), ret: Box::new(self.returnType.clone()) }
    }
}

impl Into<FunctionMeta> for FunctionDef {
    fn into(self) -> FunctionMeta {
        FunctionMeta::makeRuntime(self.name, self.localsMeta.into_boxed_slice(), self.argsCount, self.returnType.unwrap_or(Void), self.body)
    }
}

impl FunctionMeta {
    pub fn genName(&self) -> String {
        genFunName(&self.name, &self.localsMeta.iter().map(|it| {
            it.typ.clone()
        }).collect::<Vec<_>>())
    }

    pub fn makeNative(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: DataType, pure: bool) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Native,
            localsMeta: locals,
            returns: ret != Void,
            returnType: ret,
            isPure: pure,
        }
    }

    pub fn makeBuiltin(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: DataType, pure: bool) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Native,
            localsMeta: locals,
            returns: ret != Void,
            returnType: ret,
            isPure: pure,
        }
    }

    pub fn makeRuntime(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: DataType, body: Vec<Statement>) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Runtime(body),
            localsMeta: locals,
            returns: ret != Void,
            returnType: ret,
            isPure: false,
        }
    }
}

#[derive(Debug)]
pub enum LoadedFunction {
    BuiltIn(fn (&mut VirtualMachine, &mut StackFrame) -> Value),
    Native(extern fn (&mut VirtualMachine, &mut StackFrame) -> Value),
    Virtual(Vec<OpCode>)
}

#[inline(always)]
pub fn printStack() {
    let mut ptr = 0usize;
    unsafe {
        asm!(
        "mov {ptr}, rsp",
        ptr = out(reg) ptr
        );
    }
    println!("rsp is {}", ptr)
}

pub fn callNative(f: &extern fn (&mut VirtualMachine, &mut StackFrame) -> Value, vm: &mut VirtualMachine, frame: &mut StackFrame) -> Value {
    saveRegisters();
    let res = f(vm, frame);
    restoreRegisters();
    res
}

impl LoadedFunction {
    #[inline]
    pub fn call(&self, vm: &mut VirtualMachine, frame: StackFrame, returns: bool) -> Value {
        let vm2 = unsafe { &mut *vm.rawPtr() };

        vm.pushFrame(frame);

        let a = vm.getMutFrame();

        let x = match self {
            LoadedFunction::BuiltIn(b) => b(vm2, a),
            LoadedFunction::Native(n) => callNative(n, vm2, a),
            LoadedFunction::Virtual(v) => vm.execute(v, returns),
        };

        vm.popFrame();

        x
    }
}

#[derive(Debug)]
pub struct Namespace {
    pub id: usize,
    pub name: String,
    pub state: NamespaceState,

    pub functions: FastAcess<String, (FunctionMeta, Option<LoadedFunction>)>,

    pub globals: FastAcess<String, (GlobalMeta, Value)>,

    pub structs: FastAcess<String, StructMeta>,

    pub strings: FastAcess<String, *mut Str>,

    pub types: FastAcess<DataType, DataType>
}

impl Allocation for Namespace {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        for s in &self.strings.actual {
            allocations.visit(*s as usize)
        }

        for s in &self.globals.actual {
            allocations.visit((s.1).into())
        }
    }
}

impl Namespace {
    pub fn asPtr(&self) -> *mut Namespace {
        self as *const Namespace as *mut Namespace
    }

    pub fn findFunction(&self, name: &str) -> Result<(&FunctionMeta, usize), CodeGenError> {
        let id = self.functions.getSlowStr(name.strip_prefix(&format!("{}::", self.name)).unwrap_or(name)).ok_or(CodeGenError::SymbolNotFound(SymbolNotFound{ name: name.to_string(), typ: SymbolType::Namespace }))?;

        Ok((&id.0.0, id.1))
    }

    pub fn findGlobal(&self, name: &str) -> Result<(&GlobalMeta, usize), CodeGenError> {
        let gId = self.globals.getSlowStr(name)
            .ok_or(CodeGenError::SymbolNotFound(SymbolNotFound{ name: name.to_string(), typ: SymbolType::Global }))?;

        Ok((&gId.0.0, gId.1))
    }

    pub fn findStruct(&self, name: &str) -> Result<(&StructMeta, usize), CodeGenError> {
        let strc = self.structs.getSlowStr(name)
            .ok_or(CodeGenError::SymbolNotFound(SymbolNotFound{ name: name.to_string(), typ: SymbolType::Struct }))?;

        Ok(strc)
    }

    pub fn findStructField(&self, structName: &str, fieldName: &str) -> Result<(&StructMeta, usize, &VariableMetadata, usize), CodeGenError> {
        let (struc, structId) = self.findStruct(structName)?;

        let (field, fieldId) = struc.findField(fieldName)?;

        Ok((struc, structId, field, fieldId))
    }

    pub fn getFunction(&self, id: usize) -> &(FunctionMeta, Option<LoadedFunction>) {
        self.functions.getFast(id).unwrap()
    }

    pub fn getFunctionMeta(&self, id: usize) -> Option<&FunctionMeta> {
        match self.functions.getFast(id) {
            None => None,
            Some(v) => Some(&v.0)
        }
    }

    pub fn makeNative(
        &mut self,
        name: &str,
        args: &[DataType],
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> Value,
        ret: DataType,
        pure: bool
    ) {
        let genName = genFunNameMetaTypes(&name, &args, args.len());
        let argsCount = args.len();

        let meta = FunctionMeta::makeBuiltin(name.to_string(), args.iter().map(|it| { it.clone().into() }).collect::<Vec<VariableMetadata>>().into_boxed_slice(), argsCount, ret, pure);

        self.functions.insert(genName, (meta, Some(LoadedFunction::BuiltIn(fun))));
    }


    // 🐀
    // this is pretty hacky ngl
    pub fn makeNativeNoRat(
        &mut self,
        name: &str,
        args: &[DataType],
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> (),
        pure: bool
    ) {
        let uwuFn: fn(&mut VirtualMachine, &mut StackFrame) -> Value = unsafe { transmute(fun) };

        let genName = genFunNameMetaTypes(&name, &args, args.len());
        let argsCount = args.len();

        let meta = FunctionMeta::makeBuiltin(name.to_string(), args.iter().map(|it| { it.clone().into() }).collect::<Vec<VariableMetadata>>().into_boxed_slice(), argsCount, DataType::Void, pure);

        self.functions.insert(genName, (meta, Some(LoadedFunction::BuiltIn(uwuFn))));
    }

    pub fn registerFunctionDef(&mut self, d: FunctionMeta) -> usize {
        self.functions.insert(d.genName(), (d, None)).unwrap()
    }

    pub fn registerStruct(&mut self, d: StructMeta) -> usize {
        let index = self.structs.insert(d.name.clone(), d).unwrap();

        index
    }

    pub fn registerGlobal(&mut self, global: GlobalMeta) -> usize {
        let res = self.globals.insert(global.name.as_str().to_string(), (global, Value::null())).unwrap();

        res
    }

    pub fn new(name: &str) -> Self {
        Self {
            id: 0,
            name: name.to_string(),
            state: NamespaceState::PartiallyLoaded,
            functions: Default::default(),
            globals: Default::default(),
            structs: Default::default(),
            strings: Default::default(),
            types: Default::default(),
        }
    }

    pub fn constructNamespace(src: Vec<Operation>, name: &str, vm: &mut VirtualMachine, mainLocals: Vec<VariableMetadata>) -> Namespace {
        let mut n = Namespace::new(name);
        let mut initFunction = FunctionDef{
            name: String::from("__init"),
            localsMeta: mainLocals,
            argsCount: 0,
            body: vec![],
            returnType: None,
            isNative: false,
        };

        for s in src {
            match s {
                Operation::Global(g) => {
                    match g {
                        Node::FunctionDef(d) => {
                            n.registerFunctionDef(d.into());
                        }
                        Node::StructDef(v) => {
                            n.registerStruct(v.into());
                        }
                        Node::Import(_) => todo!(),
                        // TODO default value
                        Node::GlobalVarDef(name, default) => {
                            n.registerGlobal(GlobalMeta{
                                name,
                                default,
                                typ: DataType::Void,
                            });
                        }
                    }
                }
                Operation::Statement(v) => {
                    initFunction.body.push(v);
                }
                Operation::Expr(e) => {
                    match e {
                        // Expression::NamespaceAccess(c) => todo!(),
                        c => initFunction.body.push(Statement::StatementExpression(c))
                    }
                }
            }
        }
        n.registerFunctionDef(initFunction.into());
        n
    }
}

pub fn loadSourceFile(src: String, vm: &mut VirtualMachine) -> Result<Vec<Operation>, LoadFileError<TokenType>> {
    let tokens = match tokenizeSource(&src) {
        Ok(v) => v,
        Err(e) => {
            return Err(e.into())
        }
    };

    println!("tokens {:?}", tokens);

    if tokens.is_empty() {
        return Ok(vec![])
    }

    Ok(parseTokens(tokens)?)
}