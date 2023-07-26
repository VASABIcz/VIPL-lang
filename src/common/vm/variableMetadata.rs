use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct VariableMetadata {
    pub name: String,
    pub typ: DataType,
    pub mutable: bool
}

impl From<DataType> for VariableMetadata {
    fn from(value: DataType) -> Self {
        Self {
            name: "unknown".to_string(),
            typ: value,
            mutable: false,
        }
    }
}

impl VariableMetadata {
    pub fn n(name: &str, t: DataType) -> Self {
        Self::new(name.to_string(), t)
    }

    pub fn new(name: String, t: DataType) -> Self {
        Self { name, typ: t, mutable: false }
    }
}
