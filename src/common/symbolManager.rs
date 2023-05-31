use crate::vm::dataType::DataType;
use crate::vm::namespace::{FunctionMeta, GlobalMeta, StructMeta};

pub struct SymbolManager {

}

impl SymbolManager {
    pub fn enterScope(&mut self) {
        todo!()
    }

    pub fn exitScope(&mut self) {
        todo!()
    }

    pub fn getFunction(&mut self) -> Result<FunctionMeta, ()> {
        todo!()
    }

    pub fn getGlobal(&mut self) -> Result<GlobalMeta, ()> {
        todo!()
    }

    pub fn getStruct(&mut self) -> Result<StructMeta, ()> {
        todo!()
    }

    pub fn getLocal(&mut self) -> Result<(usize, DataType), ()> {
        todo!()
    }

    pub fn registerLocal(&mut self, name: &str, typ: DataType) {
        todo!()
    }
}