use std::collections::HashMap;
use std::error::Error;
use crate::ast::{FunctionDef, Node, Op};
use crate::betterGen::genFunctionDef;
use crate::codegen::complexBytecodeGen;
use crate::lexer::tokenizeSource;
use crate::parser::{Operation, parseTokens};
use crate::std::bootStrapVM;
use crate::value::Value;
use crate::vm::{Func, OpCode, StackFrame, VariableMetadata, VirtualMachine};

#[derive(Debug, PartialEq)]
pub enum NamespaceState {
    Loaded,
    PartiallyLoaded,
}

#[derive(Debug)]
pub enum LoadedFunction {
    BuiltIn(fn (&mut VirtualMachine, &mut StackFrame)),
    Native(fn (&mut VirtualMachine, &mut StackFrame)),
    Virtual(Vec<OpCode>)
}

impl LoadedFunction {
    pub fn call(&self, vm: &mut VirtualMachine, mut frame: StackFrame) {
        match self {
            LoadedFunction::BuiltIn(b) => {
                b(vm, &mut frame)
            }
            LoadedFunction::Native(n) => {
                n(vm, &mut frame)
            }
            LoadedFunction::Virtual(v) => {
                vm.pushFrame(frame);
                vm.execute2(v)
            }
        }
    }
}

#[derive(Debug)]
pub struct Namespace {
    pub name: String,
    pub state: NamespaceState,

    pub functionsLookup: HashMap<String, usize>,
    pub globalsLookup: HashMap<String, usize>,

    pub functionsMeta: Vec<FunctionDef>,
    pub functions: Vec<LoadedFunction>,

    pub globalsMeta: Vec<VariableMetadata>,
    pub globals: Vec<Value>,
}

impl Namespace {
    pub fn registerFunctionDef(&mut self, d: FunctionDef) {
        let index = self.functionsMeta.len();
        self.functionsLookup.insert(d.name.clone(), index);
        self.functionsMeta.push(d);
    }

    pub fn constructNamespace(src: Vec<Operation>, name: String, vm: &mut VirtualMachine) -> Namespace {
        let mut n = Namespace {
            name,
            state: NamespaceState::PartiallyLoaded,
            functionsLookup: Default::default(),
            globalsLookup: Default::default(),
            functionsMeta: vec![],
            functions: vec![],
            globalsMeta: vec![],
            globals: vec![],
        };
        let mut initFunction = FunctionDef{
            name: String::from("__init"),
            args: vec![],
            argCount: 0,
            body: vec![],
            returnType: None,
            isNative: false,
        };

        for s in src {
            match s {
                Operation::Global(g) => {
                    match g {
                        Node::FunctionDef(d) => {
                            n.registerFunctionDef(d);
                        }
                        Node::StructDef(_) => todo!()
                    }
                }
                Operation::Statement(v) => {
                    initFunction.body.push(v)
                }
                Operation::Expr(_) => todo!()
            }
        }
        n.registerFunctionDef(initFunction);
        n
    }
}

pub fn link(vm: &mut VirtualMachine) {
    for n in &vm.namespaces {
        if n.state == NamespaceState::Loaded {
            continue
        }

        for f in &n.functionsMeta {
            let mut ops = vec![];
            genFunctionDef(f.clone(), &mut ops)
        }
    }
}

pub fn loadSourceFile(src: &str, vm: &mut VirtualMachine) -> Result<Vec<Operation>, Box<dyn Error>> {
    let src = std::fs::read_to_string(src).expect("failed to read source");

    let tokens = match tokenizeSource(&src) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("tokenizer");
            todo!()
        }
    };

    if tokens.is_empty() {
        return Ok(vec![])
    }

    let ast = match parseTokens(tokens) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("parser");
            todo!()
        }
    };

    Ok(ast)
}