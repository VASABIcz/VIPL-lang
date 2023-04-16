use std::collections::HashMap;
use std::error::Error;
use std::mem::transmute;
use crate::ast::{Expression, FunctionDef, Node, Op, Statement, VariableModd};
use crate::betterGen::genFunctionDef;
// use crate::codegen::complexBytecodeGen;
use crate::lexer::tokenizeSource;
use crate::objects::{Str, ViplObject};
use crate::parser::{Operation, parseTokens};
use crate::value::Value;
use crate::vm;
use crate::vm::{DataType, Func, genFunName, genFunNameMeta, MyStr, OpCode, StackFrame, VariableMetadata, VirtualMachine};
use crate::vm::FuncType::Builtin;

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

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub name: String,
    pub fieldsLookup: HashMap<String, usize>,
    pub fields: Vec<VariableMetadata>
    // TODO default values
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
                vm.execute2(v);
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
    pub globalsLookup: HashMap<String, usize>,
    pub structLookup: HashMap<String, usize>,

    pub functionsMeta: Vec<FunctionMeta>,
    pub functions: Vec<LoadedFunction>,

    pub globalsMeta: Vec<VariableMetadata>,
    pub globals: Vec<Value>,

    pub structs: Vec<StructMeta>,
}

impl Namespace {
    pub fn getFunctionByName(&self, name: &str) -> Option<(&FunctionMeta, usize)> {
        let id = self.functionsLookup.get(name.strip_prefix((&format!("{}::", self.name))).unwrap_or(name))?;
        match self.functionsMeta.get(*id) {
            None => None,
            Some(v) => {
                Some((v, *id))
            }
        }
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
        self.functions.push(LoadedFunction::BuiltIn(fun));
    }

    pub fn registerFunctionDef(&mut self, d: FunctionMeta) {
        let index = self.functionsMeta.len();
        self.functionsLookup.insert(d.genName(), index);
        self.functionsMeta.push(d);
    }

    pub fn registerStruct(&mut self, d: StructMeta) {
        let index = self.structs.len();
        self.structLookup.insert(d.name.clone(), index);
        self.structs.push(d);
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
        }
    }

    pub fn lookupNamespace(vm: &mut VirtualMachine, name: Vec<String>) {
        todo!()
    }

    pub fn constructNamespace(src: Vec<Operation>, name: String, vm: &mut VirtualMachine) -> Namespace {
        let mut n = Namespace::new(name);
        let mut initFunction = FunctionDef{
            name: String::from("__init"),
            localsMeta: vec![],
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
                        Node::StructDef(v) => n.registerStruct(v.into()),
                        Node::Import(_) => todo!()
                    }
                }
                Operation::Statement(v) => {
                    initFunction.body.push(v);
                }
                Operation::Expr(e) => {
                    match e {
                        Expression::FunctionCall(c) => {
                            initFunction.body.push(Statement::FunctionExpr(c));
                        }
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

pub fn loadSourceFile(src: &str, vm: &mut VirtualMachine) -> Result<Vec<Operation>, Box<dyn Error>> {
    let src = std::fs::read_to_string(src).expect("failed to read source");

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

    let ast = match parseTokens(tokens) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("parser");
            todo!("{}", e)
        }
    };

    Ok(ast)
}