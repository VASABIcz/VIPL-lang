use std::arch::asm;
use std::collections::HashMap;
use std::mem::transmute;

use crate::ast::{ASTNode, BinaryOp, RawExpression, FunctionDef, RawNode, RawStatement, VariableModd, Statement, Expression};
use crate::bytecodeGen::{genFunctionDef, Body, ExpressionCtx};
use crate::errors::{CodeGenError, LoadFileError, SymbolNotFoundE, SymbolType};
use crate::fastAccess::FastAcess;
use crate::lexer::{LexingUnit, tokenizeSource};
use crate::lexingUnits::TokenType;
// use crate::codegen::complexBytecodeGen;
use crate::naughtyBox::Naughty;
use crate::parser::ParsingUnit;
use crate::utils::{
    genFunName, genFunNameMeta, genFunNameMetaTypes, restoreRegisters, saveRegisters,
};
use crate::viplParser::{parseTokens, VIPLParsingState};
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::{Function, Void};
use crate::vm::heap::{Allocation, Hay, HayCollector};
use crate::vm::nativeObjects::ViplObject;
use crate::vm::objects::Str;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::{ImportHints, OpCode, VirtualMachine};
use crate::wss::WhySoSlow;

#[derive(Debug, PartialEq)]
pub enum NamespaceState {
    Loaded,
    PartiallyLoaded,
    FailedToLoad
}

#[derive(Debug, Clone)]
pub enum FunctionTypeMeta {
    Runtime(Body),
    Builtin,
    Native,
}

#[derive(Debug, Clone)]
pub struct FunctionMeta {
    pub name: String,
    pub argsCount: usize,
    pub functionType: FunctionTypeMeta,
    pub localsMeta: Box<[VariableMetadata]>,
    pub returnType: DataType,
    pub isPure: bool,
    pub returns: bool,
}

impl FunctionMeta {
    #[inline(always)]
    pub fn returns(&self) -> bool {
        self.returns
    }

    pub fn getArgs(&self) -> Vec<DataType> {
        self.localsMeta[0..self.argsCount].iter().map(|it| it.typ.clone()).collect()
    }
}

#[derive(Clone, Debug)]
pub struct GlobalMeta {
    pub name: String,
    pub default: Expression,
    pub typ: DataType,
}

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub name: String,
    fields: FastAcess<String, VariableMetadata>, // TODO default values
}

#[derive(Debug, Clone)]
pub struct EnumArm {
    name: String,
    valueType: TokenType,
}

#[derive(Debug, Clone)]
pub struct EnumMeta {
    pub name: String,
    arms: FastAcess<String, EnumArm>,
}

impl StructMeta {
    pub fn n(name: &str, fields: FastAcess<String, VariableMetadata>) -> Self {
        Self {
            name: name.to_string(),
            fields,
        }
    }

    pub fn new(name: String, fields: FastAcess<String, VariableMetadata>) -> Self {
        Self { name, fields }
    }

    pub fn findField(&self, name: &str) -> Result<(&VariableMetadata, usize), CodeGenError> {
        self.fields
            .getSlowStr(name)
            .ok_or_else(||CodeGenError::SymbolNotFound(SymbolNotFoundE::obj(name)))
    }

    #[inline]
    pub fn fieldCount(&self) -> usize {
        self.fields.len()
    }
}

impl FunctionMeta {
    pub fn toFunctionType(&self) -> DataType {
        Function {
            ret: Box::new(self.returnType.clone()),
            args: self
                .localsMeta
                .iter()
                .take(self.argsCount)
                .map(|it| it.typ.clone())
                .collect::<Vec<_>>(),
        }
    }
}

impl Into<FunctionMeta> for FunctionDef {
    fn into(self) -> FunctionMeta {
        FunctionMeta::makeRuntime(
            self.name,
            self.localsMeta.into_boxed_slice(),
            self.argsCount,
            self.returnType.unwrap_or(Void),
            self.body,
        )
    }
}

impl FunctionMeta {
    pub fn genName(&self) -> String {
        genFunName(
            &self.name,
            &self
                .localsMeta
                .iter()
                .take(self.argsCount)
                .map(|it| it.typ.clone())
                .collect::<Vec<_>>(),
        )
    }

    pub fn makeNative(
        name: String,
        locals: Box<[VariableMetadata]>,
        argsCount: usize,
        ret: DataType,
        pure: bool,
    ) -> Self {
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

    pub fn makeBuiltin(
        name: String,
        locals: Box<[VariableMetadata]>,
        argsCount: usize,
        ret: DataType,
        pure: bool,
    ) -> Self {
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

    pub fn makeRuntime(
        name: String,
        locals: Box<[VariableMetadata]>,
        argsCount: usize,
        ret: DataType,
        body: Body,
    ) -> Self {
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
    BuiltIn(fn(&mut VirtualMachine, &mut StackFrame) -> Value),
    Native(extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> Value),
    Virtual(Vec<OpCode>),
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

// this is stupid
#[inline(never)]
pub fn callNative(
    f: &extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> Value,
    vm: &mut VirtualMachine,
    frame: &mut StackFrame,
) -> Value {
    saveRegisters();
    let res = f(vm, frame);
    restoreRegisters();
    res
}

impl LoadedFunction {
    #[inline]
    pub fn call(&self, vm: &mut VirtualMachine, frame: StackFrame, returns: bool) -> Value {
        let mut vm2 = vm.getNaughty();

        vm.pushFrame(frame);

        let a = vm.getMutFrame();

        let x = match self {
            LoadedFunction::BuiltIn(b) => b(vm2.getMut(), a),
            LoadedFunction::Native(n) => callNative(n, vm2.getMut(), a),
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
    pub vm: Naughty<VirtualMachine>,

    functions: FastAcess<String, (FunctionMeta, Option<LoadedFunction>)>,

    globals: FastAcess<String, (GlobalMeta, Value)>,

    structs: FastAcess<String, StructMeta>,

    strings: FastAcess<String, *mut ViplObject<Str>>,

    types: FastAcess<TokenType, TokenType>,

    enums: FastAcess<String, EnumMeta>,

    importHints: Vec<ImportHints>
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
    pub fn getImportHints(&self) -> &Vec<ImportHints> {
        &self.importHints
    }

    pub fn getString(&self, id: usize) -> Value {
        Value::from(*self.strings.getFast(id).unwrap() as usize)
    }

    pub fn allocateOrGetString(&mut self, s: &str) -> usize {
        match self.strings.getSlowStr(s) {
            None => {
                let al = self.vm.getMut().allocateString(s);
                self.strings.insert(s.to_string(), al.inner).unwrap()
            }
            Some(v) => v.1,
        }
    }

    pub fn asPtr(&self) -> *mut Namespace {
        self as *const Namespace as *mut Namespace
    }

    pub fn findFunction(&self, name: &str) -> Result<(&FunctionMeta, usize), CodeGenError> {
        let lookName = name.strip_prefix(&format!("{}::", self.name)).unwrap_or(name);

        let id = self
            .functions
            .getSlowStr(lookName)
            .ok_or_else(|| CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)))?;

        Ok((&id.0 .0, id.1))
    }

    pub fn findFunctionsByName(&self, name: &str) -> Vec<(FunctionMeta, usize)> {
        self.functions.actual
            .iter()
            .filter(|it| it.0.name == name)
            .enumerate()
            .map(|(id, it)| (it.0.clone(), id))
            .collect::<Vec<_>>()
    }

    pub fn findFunctionMut(&mut self, name: &str) -> Result<(&mut FunctionMeta, usize), CodeGenError> {
        println!("{} {:?}", name, self.functions.lookupTable.keys());

        let id = self
            .functions
            .getSlowStrMut(name);

        let d = match id {
            None => {
                Err(CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(name)))?
            }
            Some(v) => v
        };

        Ok((&mut d.0.0, d.1))
    }

    pub fn lookupFunction(
        &self,
        name: &str,
        args: &[DataType],
    ) -> Result<(&FunctionMeta, usize), CodeGenError> {
        let genName = genFunName(name, args);

        let id = self
            .functions
            .getSlowStr(
                genName
                    .strip_prefix(&format!("{}::", self.name))
                    .unwrap_or(&genName),
            )
            .ok_or(CodeGenError::SymbolNotFound(SymbolNotFoundE::fun(&genName)))?;

        Ok((&id.0 .0, id.1))
    }

    pub fn findGlobal(&self, name: &str) -> Result<(&GlobalMeta, usize), CodeGenError> {
        let gId = self
            .globals
            .getSlowStr(name)
            .ok_or(CodeGenError::SymbolNotFound(SymbolNotFoundE::global(name)))?;

        Ok((&gId.0 .0, gId.1))
    }

    pub fn getGlobal(&self, id: usize) -> &(GlobalMeta, Value) {
        self.globals.getFast(id).unwrap()
    }

    pub fn getGlobalMut(&mut self, id: usize) -> &mut (GlobalMeta, Value) {
        self.globals.getFastMut(id).unwrap()
    }

    pub fn getStruct(&self, id: usize) -> &StructMeta {
        self.structs.getFast(id).unwrap()
    }

    pub fn getStructMut(&mut self, id: usize) -> &mut StructMeta {
        self.structs.getFastMut(id).unwrap()
    }

    pub fn getFunctions(&self) -> &Vec<(FunctionMeta, Option<LoadedFunction>)> {
        &self.functions.actual
    }

    pub fn getStructs(&self) -> &Vec<StructMeta> {
        &self.structs.actual
    }

    pub fn getFunctionsMut(&mut self) -> &mut Vec<(FunctionMeta, Option<LoadedFunction>)> {
        &mut self.functions.actual
    }

    pub fn getGlobals(&self) -> &Vec<(GlobalMeta, Value)> {
        &self.globals.actual
    }

    pub fn getGlobalsMut(&mut self) -> &mut Vec<(GlobalMeta, Value)> {
        &mut self.globals.actual
    }

    pub fn findStruct(&self, name: &str) -> Result<(&StructMeta, usize), CodeGenError> {
        let strc = self
            .structs
            .getSlowStr(name)
            .ok_or_else(||CodeGenError::SymbolNotFound(SymbolNotFoundE::obj(name)))?;

        Ok(strc)
    }

    pub fn findStructField(
        &self,
        structName: &str,
        fieldName: &str,
    ) -> Result<(&StructMeta, usize, &VariableMetadata, usize), CodeGenError> {
        let (struc, structId) = self.findStruct(structName)?;

        let (field, fieldId) = struc.findField(fieldName)?;

        Ok((struc, structId, field, fieldId))
    }

    pub fn getFunction(&self, id: usize) -> &(FunctionMeta, Option<LoadedFunction>) {
        self.functions.getFast(id).unwrap()
    }

    pub fn getFunctionMut(&mut self, id: usize) -> &mut (FunctionMeta, Option<LoadedFunction>) {
        self.functions.getFastMut(id).unwrap()
    }

    pub fn getFunctionMeta(&self, id: usize) -> Option<&FunctionMeta> {
        match self.functions.getFast(id) {
            None => None,
            Some(v) => Some(&v.0),
        }
    }

    pub fn makeNative(
        &mut self,
        name: &str,
        args: &[DataType],
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> Value,
        ret: DataType,
        pure: bool,
    ) {
        let genName = genFunNameMetaTypes(&name, &args, args.len());
        let argsCount = args.len();

        let meta = FunctionMeta::makeBuiltin(
            name.to_string(),
            args.iter()
                .map(|it| it.clone().into())
                .collect::<Vec<VariableMetadata>>()
                .into_boxed_slice(),
            argsCount,
            ret,
            pure,
        );

        self.functions
            .insert(genName, (meta, Some(LoadedFunction::BuiltIn(fun))));
    }

    // 🐀
    // this is pretty hacky ngl
    pub fn makeNativeNoRat(
        &mut self,
        name: &str,
        args: &[DataType],
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> (),
        pure: bool,
    ) {
        let uwuFn: fn(&mut VirtualMachine, &mut StackFrame) -> Value = unsafe { transmute(fun) };

        let genName = genFunNameMetaTypes(&name, &args, args.len());
        let argsCount = args.len();

        let meta = FunctionMeta::makeBuiltin(
            name.to_string(),
            args.iter()
                .map(|it| it.clone().into())
                .collect::<Vec<VariableMetadata>>()
                .into_boxed_slice(),
            argsCount,
            DataType::Void,
            pure,
        );

        self.functions
            .insert(genName, (meta, Some(LoadedFunction::BuiltIn(uwuFn))));
    }

    pub fn registerFunctionDef(&mut self, d: FunctionMeta) -> usize {
        self.functions.insert(d.genName(), (d, None)).unwrap()
    }

    pub fn registerStruct(&mut self, d: StructMeta) -> usize {
        let index = self.structs.insert(d.name.clone(), d).unwrap();

        index
    }

    pub fn registerGlobal(&mut self, global: GlobalMeta) -> usize {
        let res = self
            .globals
            .insert(global.name.as_str().to_string(), (global, Value::null()))
            .unwrap();

        res
    }

    pub fn new(name: &str, vm: *mut VirtualMachine) -> Self {
        Self {
            id: 0,
            name: name.to_string(),
            state: NamespaceState::PartiallyLoaded,
            vm: Naughty::new(vm),
            functions: Default::default(),
            globals: Default::default(),
            structs: Default::default(),
            strings: Default::default(),
            types: Default::default(),
            enums: Default::default(),
            importHints: vec![],
        }
    }

    pub fn extendFunctionality(&mut self, src: Vec<ASTNode>) {
        for s in src {
            match s {
                ASTNode::Global(g) => {
                    match g.exp {
                        RawNode::FunctionDef(d) => {
                            self.registerFunctionDef(d.into());
                        }
                        RawNode::StructDef(v) => {
                            self.registerStruct(v.into());
                        }
                        RawNode::Import(nam, syms) => {
                            self.importHints.push(ImportHints::Symbols(nam, syms))
                        },
                        RawNode::NamespaceImport(nam, ren) => {
                            self.importHints.push(ImportHints::Namespace(nam, ren))
                        },
                        RawNode::GlobalVarDef(name, default) => {
                            self.registerGlobal(GlobalMeta {
                                name,
                                default: default,
                                typ: Void,
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }

    pub fn constructNamespace(
        src: Vec<ASTNode>,
        name: &str,
        vm: &mut VirtualMachine,
        mainLocals: Vec<VariableMetadata>,
    ) -> Namespace {
        let mut n = Namespace::new(name, vm);
        let mut initFunction = FunctionDef {
            name: String::from("__init"),
            localsMeta: mainLocals,
            argsCount: 0,
            body: Body::new(vec![]),
            returnType: None,
            isNative: false,
            isOneLine: false,
        };

        for s in src {
            match s {
                ASTNode::Global(g) => {
                    match g.exp {
                        RawNode::FunctionDef(d) => {
                            n.registerFunctionDef(d.into());
                        }
                        RawNode::StructDef(v) => {
                            n.registerStruct(v.into());
                        }
                        RawNode::Import(na, s) => {
                            n.importHints.push(ImportHints::Symbols(na, s))
                        },
                        // TODO default value
                        RawNode::NamespaceImport(na, r) => {
                            n.importHints.push(ImportHints::Namespace(na, r))
                        },
                        RawNode::GlobalVarDef(name, default) => {
                            n.registerGlobal(GlobalMeta {
                                name,
                                default: default,
                                typ: DataType::Void,
                            });
                        }
                    }
                }
                ASTNode::Statement(v) => {
                    initFunction.body.push(v);
                }
                ASTNode::Expr(e) => {
                    match e {
                        // Expression::NamespaceAccess(c) => todo!(),
                        c => initFunction.body.push(Statement{ exp: RawStatement::StatementExpression(c), loc: vec![] }),
                    }
                }
            }
        }
        n.registerFunctionDef(initFunction.into());
        n
    }
}

pub fn loadSourceFile(
    src: &str,
    vm: &mut VirtualMachine,
    lexingUnits: &mut [Box<dyn LexingUnit<TokenType>>],
    parsingUnits: &mut [Box<dyn ParsingUnit<ASTNode, TokenType, VIPLParsingState>>]
) -> Result<Vec<ASTNode>, LoadFileError<TokenType>> {
    let tokens = match tokenizeSource(src, lexingUnits) {
        Ok(v) => v,
        Err(e) => return Err(e.into()),
    };

    if tokens.is_empty() {
        return Ok(vec![]);
    }

    let ast = parseTokens(tokens, parsingUnits)?;

    Ok(ast)
}
