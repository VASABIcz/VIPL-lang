use std::collections::HashMap;
use std::error::Error;
use std::mem::transmute;

use crate::ast::{Expression, FunctionDef, Node, BinaryOp, Statement, VariableModd};
use crate::bytecodeGen::{ExpressionCtx, genFunctionDef};
use crate::errors::Errorable;
// use crate::codegen::complexBytecodeGen;
use crate::lexer::tokenizeSource;
use crate::parser::{Operation, parseTokens};
use crate::utils::{genFunName, genFunNameMeta};
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

    pub fn makeNative(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: Option<DataType>) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Native,
            localsMeta: locals,
            returnType: ret,
        }
    }

    pub fn makeBuiltin(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: Option<DataType>) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Native,
            localsMeta: locals,
            returnType: ret,
        }
    }

    pub fn makeRuntime(name: String, locals: Box<[VariableMetadata]>, argsCount: usize, ret: Option<DataType>, body: Vec<Statement>) -> Self {
        Self {
            name,
            argsCount,
            functionType: FunctionTypeMeta::Runtime(body),
            localsMeta: locals,
            returnType: ret,
        }
    }
}

#[derive(Debug)]
pub enum LoadedFunction {
    BuiltIn(fn (&mut VirtualMachine, &mut StackFrame)),
    Native(extern fn (&mut VirtualMachine, &mut StackFrame)),
    Virtual(Vec<OpCode>)
}

impl LoadedFunction {
    #[inline(never)]
    pub fn call(&self, vm: &mut VirtualMachine, mut frame: StackFrame) {
        match self {
            LoadedFunction::BuiltIn(b) => {
                b(vm, &mut frame);
            }
            LoadedFunction::Native(n) => unsafe {
                let x: *const () = transmute(n.clone());
                let d: usize = transmute(vm.nativeWrapper.stringNew);
                println!("before call");
                println!("vm: {:?} {}", vm as *const VirtualMachine, vm as *const VirtualMachine as usize);
                println!("proc: {:?} {}", x, x as usize);
                n(vm, &mut frame);
            }
            LoadedFunction::Virtual(v) => {
                vm.pushFrame(frame);
                vm.execute(v);
            }
        }
    }
}

#[derive(Debug)]
pub struct Namespace {
    pub id: usize,
    pub name: String,
    pub state: NamespaceState,

    pub functionsLookup: HashMap<String, usize>,
    pub functionsMeta: Vec<FunctionMeta>,
    pub functions: Vec<Option<LoadedFunction>>,

    pub globalsLookup: HashMap<String, usize>,
    pub globalsMeta: Vec<GlobalMeta>,
    pub globals: Vec<Value>,

    pub structLookup: HashMap<String, usize>,
    pub structs: Vec<StructMeta>,

    pub stringsLookup: HashMap<String, usize>,
    pub strings: Vec<*mut Str>
}

impl Allocation for Namespace {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        for s in &self.strings {
            allocations.visit(*s as usize)
        }

        for s in &self.globals {
            allocations.visit((*s).into())
        }
    }
}

impl Namespace {
    pub fn findFunction(&self, name: &str) -> Option<(&FunctionMeta, usize)> {
        println!("i am here");
        let id = self.functionsLookup.get(name.strip_prefix(&format!("{}::", self.name)).unwrap_or(name))?;
        match self.functionsMeta.get(*id) {
            None => None,
            Some(v) => {
                Some((v, *id))
            }
        }
    }

    pub fn findGlobal(&self, name: &str) -> Errorable<(&GlobalMeta, usize)> {
        let gId = self.globalsLookup.get(name)
            .ok_or(format!("failed to find global varibale with name {}, in namespace {}", name, self.name))?;

        let g = self.globalsMeta.get(*gId).ok_or(format!("failed to find global varibale with id {}", gId))?;

        Ok((g, *gId))
    }

    pub fn findStruct(&self, name: &str) -> Errorable<(&StructMeta, usize)> {
        let structId = self.structLookup.get(name)
            .ok_or(format!("failed to find struct with name {}, in namespace {}", name, self.name))?;

        let struc = self.structs.get(*structId).ok_or(format!("failed to find struct with id {}", structId))?;

        Ok((struc, *structId))
    }

    pub fn findStructField(&self, structName: &str, fieldName: &str) -> Errorable<(&StructMeta, usize, &VariableMetadata, usize)> {
        let (struc, structId) = self.findStruct(structName)?;

        let (field, fieldId) = struc.findField(fieldName)?;

        Ok((struc, structId, field, fieldId))
    }

    pub fn makeNative(
        &mut self,
        name: String,
        args: Box<[VariableMetadata]>,
        fun: fn(&mut VirtualMachine, &mut StackFrame) -> (),
        ret: Option<DataType>,
    ) {
        let genName = genFunNameMeta(&name, &args, args.len());
        let argsCount = args.len();

        self.functionsLookup.insert(genName, self.functionsMeta.len());
        self.functionsMeta.push(FunctionMeta::makeBuiltin(name, args, argsCount, ret));
        self.functions.push(Some(LoadedFunction::BuiltIn(fun)));
    }

    pub fn registerFunctionDef(&self, d: FunctionMeta) -> usize {
        let mS = unsafe { &mut *(self as *const Namespace as *mut Namespace) };

        let index = self.functionsMeta.len();
        mS.functionsLookup.insert(d.genName(), index);
        mS.functionsMeta.push(d);
        mS.functions.push(None);

        index
    }

    pub fn registerStruct(&mut self, d: StructMeta) -> usize {
        let index = self.structs.len();
        self.structLookup.insert(d.name.clone(), index);
        self.structs.push(d);

        index
    }

    pub fn registerGlobal(&mut self, global: GlobalMeta) -> usize {
        let index = self.globals.len();
        self.globalsLookup.insert(global.name.as_str().to_string(), index);
        self.globalsMeta.push(global);
        self.globals.push(Value::from(0));

        index
    }

    pub fn new(name: String) -> Self {
        Self {
            id: 0,
            name,
            state: NamespaceState::PartiallyLoaded,
            functionsLookup: Default::default(),
            globalsLookup: Default::default(),
            structLookup: Default::default(),
            functionsMeta: vec![],
            functions: vec![],
            globalsMeta: vec![],
            globals: vec![],
            structs: vec![],
            stringsLookup: Default::default(),
            strings: vec![],
        }
    }

    pub fn lookupNamespace(vm: &mut VirtualMachine, name: Vec<String>) {
        todo!()
    }

    pub fn constructNamespace(src: Vec<Operation>, name: String, vm: &mut VirtualMachine, mainLocals: Vec<VariableMetadata>) -> Namespace {
        let mut n = Namespace::new(name);
        let mut initFunction = FunctionDef{
            name: String::from("__init"),
            localsMeta: mainLocals,
            argsCount: 0,
            body: vec![],
            returnType: None,
            isNative: false,
        };
        println!("{:?}", src);

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