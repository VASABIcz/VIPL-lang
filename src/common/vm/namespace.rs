use std::alloc::Global;
use std::arch::asm;
use std::collections::HashMap;
use std::error::Error;
use std::hint::black_box;
use std::mem::transmute;
use libc::printf;

use crate::ast::{Expression, FunctionDef, Node, BinaryOp, Statement, VariableModd};
use crate::bytecodeGen::{ExpressionCtx, genFunctionDef};
use crate::errors::Errorable;
use crate::fastAcess::FastAcess;
// use crate::codegen::complexBytecodeGen;
use crate::lexer::tokenizeSource;
use crate::parser::{Operation, parseTokens};
use crate::utils::{genFunName, genFunNameMeta, genFunNameMetaTypes};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::DataType;
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
    pub returnType: Option<DataType>,
    pub isPure: bool
}

impl FunctionMeta {
    pub fn returns(&self) -> bool {
        match &self.returnType {
            None => false,
            Some(v) => v != &DataType::Void
        }
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
    pub fn findField(&self, name: &str) -> Errorable<(&VariableMetadata, usize)> {
        let fieldId = self.fieldsLookup.get(name)
            .ok_or(format!("failed to find field {} on struct {}", name, self.name))?;

        let field = self.fields.get(*fieldId)
            .ok_or(format!("failed to find field with id {} on struct {}", fieldId, self.name))?;

        Ok((field, *fieldId))
    }
}

impl FunctionMeta {
    pub fn toFunctionType(&self) -> DataType {
        DataType::Function { args: self.localsMeta.iter().map(|it|{it.typ.clone()}).collect::<Vec<_>>(), ret: Box::new(self.returnType.clone().unwrap_or(DataType::Void)) }
    }
}

impl Into<FunctionMeta> for FunctionDef {
    fn into(self) -> FunctionMeta {
        FunctionMeta::makeRuntime(self.name, self.localsMeta.into_boxed_slice(), self.argsCount, self.returnType, self.body)
    }
}

impl FunctionMeta {
    pub fn genName(&self) -> String {
        genFunName(&self.name, &self.localsMeta.iter().map(|it| {
            it.typ.clone()
        }).collect::<Vec<_>>())
    }

    pub fn makeNative(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: Option<DataType>, pure: bool) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Native,
            localsMeta: locals,
            returnType: ret,
            isPure: pure,
        }
    }

    pub fn makeBuiltin(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: Option<DataType>, pure: bool) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Native,
            localsMeta: locals,
            returnType: ret,
            isPure: pure,
        }
    }

    pub fn makeRuntime(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: Option<DataType>, body: Vec<Statement>) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Runtime(body),
            localsMeta: locals,
            returnType: ret,
            isPure: false,
        }
    }
}

#[derive(Debug)]
pub enum LoadedFunction {
    BuiltIn(fn (&mut VirtualMachine, &mut StackFrame)),
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

#[inline(never)]
pub fn callNative(f: &extern fn (&mut VirtualMachine, &mut StackFrame) -> Value, vm: &mut VirtualMachine, frame: &mut StackFrame) -> Value {
    saveRegisters();
    let res = f(vm, frame);
    restoreRegisters();
    res
}

#[inline(always)]
pub fn saveRegisters() {
    unsafe {
        asm!(
        "push rbx",
        "push rsi",
        "push rdi",
        // "push rbp",
        // "push rsp",
        "push r8",
        "push r9",
        "push r10",
        "push r11",
        "push r12",
        "push r13",
        "push r14",
        "push r15",
        "push 0"
        )
    }
}

#[inline(always)]
pub fn restoreRegisters() {
    unsafe {
        asm!(
        "pop r15",
        "pop r15",
        "pop r14",
        "pop r13",
        "pop r12",
        "pop r11",
        "pop r10",
        "pop r9",
        "pop r8",
        // "pop rsp",
        // "pop rbp",
        "pop rdi",
        "pop rsi",
        "pop rbx",
        )
    }
}

#[inline(always)]
pub fn printRegisters() {
    let mut rax = 0usize;
    let mut rbx = 0usize;
    let mut rcx = 0usize;
    let mut rdx = 0usize;
    let mut rsi = 0usize;
    let mut rdi = 0usize;
    let mut rbp = 0usize;
    let mut rsp = 0usize;
    let mut r8 = 0usize;
    let mut r9 = 0usize;
    let mut r10 = 0usize;
    let mut r11 = 0usize;
    let mut r12 = 0usize;
    let mut r13 = 0usize;
    let mut r14 = 0usize;
    let mut r15 = 0usize;

    unsafe {
        asm!(
        "mov {rax}, rax",
        "mov {rbx}, rbx",
        "mov {rcx}, rcx",
        "mov {rdx}, rdx",
        "mov {rsi}, rsi",
        "mov {rdi}, rdi",
        "mov {rbp}, rbp",
        "mov {rsp}, rsp",
        "mov {r8}, r8",
        "mov {r9}, r9",
        "mov {r10}, r10",
        "mov {r11}, r11",
        "mov {r12}, r12",
        "mov {r13}, r13",
        "mov {r14}, r14",
        "mov {r15}, r15",
        rax = out(reg) rax,
        rbx = out(reg) rbx,
        rcx = out(reg) rcx,
        rdx = out(reg) rdx,
        rsi = out(reg) rsi,
        rdi = out(reg) rdi,
        rbp = out(reg) rbp,
        rsp = out(reg) rsp,
        r8 = out(reg) r8,
        r9 = out(reg) r9,
        r10 = out(reg) r10,
        r11 = out(reg) r11,
        r12 = out(reg) r12,
        r13 = out(reg) r13,
        r14 = out(reg) r14,
        r15 = out(reg) r15,
        )
    }
    println!("rax: {}", rax);
    println!("rbx: {}", rbx);
    println!("rcx: {}", rcx);
    println!("rdx: {}", rdx);
    println!("rsi: {}", rsi);
    println!("rdi: {}", rdi);
    println!("rbp: {}", rbp);
    println!("rsp: {}", rsp);
    println!("r8: {}", r8);
    println!("r9: {}", r9);
    println!("r10: {}", r10);
    println!("r11: {}", r11);
    println!("r12: {}", r12);
    println!("r13: {}", r13);
    println!("r14: {}", r14);
    println!("r15: {}", r15);
}

impl LoadedFunction {
    pub fn call(&self, vm: &mut VirtualMachine, frame: StackFrame, returns: bool) {
        // println!("vm: {:?} {}", vm.rawPtr(), vm.rawPtr() as usize);

        vm.pushFrame(frame);

        let vm2 = unsafe { &mut *vm.rawPtr() };

        let a = vm.getMutFrame();

        match self {
            LoadedFunction::BuiltIn(b) => {
                b(vm2, a);
            }
            LoadedFunction::Native(n) => {
                // FIXME in release problem with return value
                let v = callNative(n, vm2, a);
                // println!("UwU");

                if returns {
                    vm.push(v.into())
                }
            }
            LoadedFunction::Virtual(v) => {
                vm.execute(v);
            }
        }
        vm.popFrame()
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

    pub fn findFunction(&self, name: &str) -> Errorable<(&FunctionMeta, usize)> {
        let id = self.functions.getSlowStr(name.strip_prefix(&format!("{}::", self.name)).unwrap_or(name)).ok_or(format!("could not find function with name {} in namespace {}", name, self.name))?;

        Ok((&id.0.0, id.1))
    }

    pub fn findGlobal(&self, name: &str) -> Errorable<(&GlobalMeta, usize)> {
        let gId = self.globals.getSlowStr(name)
            .ok_or(format!("failed to find global varibale with name {}, in namespace {}", name, self.name))?;

        Ok((&gId.0.0, gId.1))
    }

    pub fn findStruct(&self, name: &str) -> Errorable<(&StructMeta, usize)> {
        let strc = self.structs.getSlowStr(name)
            .ok_or(format!("failed to find struct with name {}, in namespace {}", name, self.name))?;

        Ok(strc)
    }

    pub fn findStructField(&self, structName: &str, fieldName: &str) -> Errorable<(&StructMeta, usize, &VariableMetadata, usize)> {
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
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> (),
        ret: DataType,
        pure: bool
    ) {
        let genName = genFunNameMetaTypes(&name, &args, args.len());
        let argsCount = args.len();

        let meta = FunctionMeta::makeBuiltin(name.to_string(), args.iter().map(|it| { it.clone().into() }).collect::<Vec<VariableMetadata>>().into_boxed_slice(), argsCount, Some(ret), pure);

        self.functions.insert(genName, (meta, Some(LoadedFunction::BuiltIn(fun))));
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
        }
    }

    pub fn lookupNamespace(vm: &mut VirtualMachine, name: Vec<String>) {
        todo!()
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

pub fn loadSourceFile(src: String, vm: &mut VirtualMachine) -> Result<Vec<Operation>, Box<dyn Error>> {
    let tokens = match tokenizeSource(&src) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("tokenizer");
            todo!("{}", e)
        }
    };

    if tokens.is_empty() {
        return Ok(vec![])
    }

    parseTokens(tokens)
}