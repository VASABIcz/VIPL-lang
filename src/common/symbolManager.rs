use std::collections::HashMap;

use crate::errors::{CodeGenError, SymbolNotFoundE};
use crate::errors::CodeGenError::SymbolNotFound;
use crate::utils::genFunName;
use crate::vm::dataType::DataType;
use crate::vm::namespace::{FunctionMeta, GlobalMeta, StructMeta};
use crate::vm::variableMetadata::VariableMetadata;

#[derive(Debug, Default)]
pub struct SymbolManager {
    pub functions: HashMap<String, (Vec<DataType>, DataType, usize, usize)>,
    pub structs: HashMap<String, (StructMeta, usize, usize)>,
    pub globals: HashMap<String, (DataType, usize, usize)>,
    pub locals: Vec<VariableMetadata>,
    pub frames: Vec<HashMap<String, (DataType, usize)>>,
    usedLocals: usize
}

impl SymbolManager {
    pub fn new() -> Self {
        Self {
            functions: Default::default(),
            structs: Default::default(),
            globals: Default::default(),
            locals: vec![],
            frames: vec![],
            usedLocals: 0,
        }
    }

    pub fn enterScope(&mut self) {
        self.frames.push(Default::default())
    }

    pub fn exitScope(&mut self) {
        let f = self.frames.pop().unwrap();

        self.usedLocals -= f.len();
    }
    
    pub fn getFunctionsByName(&self, name: &str) -> Vec<(&String, &(Vec<DataType>, DataType, usize, usize))> {
        let n = format!("{}(", name);
        
        self.functions.iter().filter(|it| it.0.starts_with(&n)).collect::<Vec<_>>()
    }

    pub fn getFunctionParts(&self, name: &[String]) -> Result<&(Vec<DataType>, DataType, usize, usize), CodeGenError> {
        let j = name.join("::");

        self.getFunction(&j)
    }

    pub fn getFunctionPartsArgs(&self, name: &[String], args: &[DataType]) -> Result<&(Vec<DataType>, DataType, usize, usize), CodeGenError> {
        let j = name.join("::");

        self.getFunction(&genFunName(&j, args))
    }

    pub fn getFunction(&self, name: &str) -> Result<&(Vec<DataType>, DataType, usize, usize), CodeGenError> {
        self.functions.get(name).ok_or_else(||SymbolNotFound(SymbolNotFoundE::fun(name)))
    }

    pub fn getGlobal(&self, name: &str) -> Result<&(DataType, usize, usize), CodeGenError> {
        self.globals.get(name).ok_or_else(||SymbolNotFound(SymbolNotFoundE::global(name)))
    }

    pub fn getStruct(&self, name: &str) -> Result<&(StructMeta, usize, usize), CodeGenError> {
        self.structs.get(name).ok_or_else(||SymbolNotFound(SymbolNotFoundE::obj(name)))
    }

    pub fn getLocal(&self, name: &str) -> Result<&(DataType, usize), CodeGenError> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.get(name) {
                return Ok(v)
            }
        }
        return Err(SymbolNotFound(SymbolNotFoundE::var(name)))
    }

    pub fn registerLocal(&mut self, name: &str, typ: DataType) {
        if self.usedLocals >= self.locals.len() {
            self.locals.push(VariableMetadata::n(name, typ.clone()));
        }

        self.frames.last_mut().unwrap().insert(name.to_string(), (typ, self.usedLocals));
        self.usedLocals += 1;
    }

    pub fn registerFunction(&mut self, name: String, namespaceId: usize, functionId: usize, args: Vec<DataType>, ret: DataType) {
        self.functions.insert(name, (args, ret, namespaceId, functionId));
    }

    pub fn registerStruct(&mut self, name: String, namespaceId: usize, structId: usize, meta: StructMeta) {
        self.structs.insert(name, (meta, namespaceId, structId));
    }

    pub fn registerGlobal(&mut self, name: String, namespaceId: usize, globalId: usize, typ: DataType) {
        self.globals.insert(name, (typ, namespaceId, globalId));
    }
}