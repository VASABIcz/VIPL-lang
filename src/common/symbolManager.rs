use std::collections::HashMap;

use crate::errors::{CodeGenError, SymbolNotFoundE};
use crate::errors::CodeGenError::SymbolNotFound;
use crate::utils::genFunName;
use crate::vm::dataType::DataType;
use crate::vm::namespace::{FunctionMeta, GlobalMeta, StructMeta};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::OpCode;

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub args: Vec<DataType>,
    pub returnType: DataType,
    pub nId: usize,
    pub fId: usize
}

impl FunctionSymbol {
    pub fn callInstruction(&self) -> OpCode {
        OpCode::LCall { namespace: self.nId as u32, id: self.fId as u32 }
    }

    pub fn pushInstruction(&self) -> OpCode {
        OpCode::PushFunction{ namespaceId: self.nId as u32, functionId: self.fId as u32 }
    }
}

#[derive(Debug, Clone)]
pub struct StructSymbol {
    pub meta: StructMeta,
    pub nId: usize,
    pub sId: usize
}

#[derive(Debug, Clone)]
pub struct GlobalSymbol {
    pub typ: DataType,
    pub nId: usize,
    pub gId: usize
}

#[derive(Debug, Default, Clone)]
pub struct SymbolManager {
    pub functions: HashMap<String, FunctionSymbol>,
    pub structs: HashMap<String, StructSymbol>,
    pub globals: HashMap<String, GlobalSymbol>,
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
    
    pub fn getFunctionsByName(&self, name: &str) -> Vec<(&String, &FunctionSymbol)> {
        let n = format!("{}(", name);
        
        self.functions.iter().filter(|it| it.0.starts_with(&n)).collect::<Vec<_>>()
    }

    pub fn getFunctionParts(&self, name: &[String]) -> Result<&FunctionSymbol, CodeGenError> {
        let j = name.join("::");

        self.getFunction(&j)
    }

    pub fn getFunctionPartsArgs(&self, name: &[String], args: &[DataType]) -> Result<&FunctionSymbol, CodeGenError> {
        let j = name.join("::");

        self.getFunction(&genFunName(&j, args))
    }

    pub fn getFunctionArgs(&self, name: &str, args: &[DataType]) -> Result<&FunctionSymbol, CodeGenError> {
        if args.len() == 0 {
            return self.getFunction(&genFunName(name, args))
        }

        // FIXME this is pretty bad
        for pattern in 0..(args.len() * args.len()) {
            let mut buf = vec![];

            for o in 0..args.len() {
                if (pattern & (1 << o)) != 0 {
                    buf.push(DataType::Value)
                } else {
                    buf.push(args[o].clone())
                }
            }

            match self.getFunction(&genFunName(name, &buf)) {
                Ok(v) => {
                    return Ok(v)
                }
                Err(e) => {
                    if pattern == (args.len() * args.len())-1 {
                        return Err(e)
                    }
                }
            }
        }
        unreachable!()
    }

    pub fn getFunction(&self, name: &str) -> Result<&FunctionSymbol, CodeGenError> {
        self.functions.get(name).ok_or_else(||SymbolNotFound(SymbolNotFoundE::fun(name)))
    }

    pub fn getGlobal(&self, name: &str) -> Result<&GlobalSymbol, CodeGenError> {
        self.globals.get(name).ok_or_else(||SymbolNotFound(SymbolNotFoundE::global(name)))
    }

    pub fn getStruct(&self, name: &str) -> Result<&StructSymbol, CodeGenError> {
        self.structs.get(name).ok_or_else(||SymbolNotFound(SymbolNotFoundE::obj(name)))
    }

    pub fn getLocal(&self, name: &str) -> Result<&(DataType, usize), CodeGenError> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.get(name) {
                return Ok(v)
            }
        }
        Err(SymbolNotFound(SymbolNotFoundE::var(name)))
    }

    pub fn registerLocal(&mut self, name: &str, typ: DataType) -> (DataType, usize) {
        if self.usedLocals >= self.locals.len() {
            self.locals.push(VariableMetadata::n(name, typ.clone()));
        }

        self.frames.last_mut().unwrap().insert(name.to_string(), (typ, self.usedLocals));
        self.usedLocals += 1;

        self.frames.last().unwrap().get(name).unwrap().clone()
    }

    pub fn registerFunction(&mut self, name: String, namespaceId: usize, functionId: usize, args: Vec<DataType>, ret: DataType) {
        self.functions.insert(name, FunctionSymbol{
            args,
            returnType: ret,
            nId: namespaceId,
            fId: functionId,
        });
    }

    pub fn registerStruct(&mut self, name: String, namespaceId: usize, structId: usize, meta: StructMeta) {
        self.structs.insert(name, StructSymbol{
            meta,
            nId: namespaceId,
            sId: structId,
        });
    }

    pub fn registerGlobal(&mut self, name: String, namespaceId: usize, globalId: usize, typ: DataType) {
        self.globals.insert(name, GlobalSymbol{
            typ,
            nId: namespaceId,
            gId: globalId,
        });
    }
}