use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct VariableMetadata {
    pub name: String,
    pub typ: DataType,
}

impl From<DataType> for VariableMetadata {
    fn from(value: DataType) -> Self {
        Self {
            name: "unknown".to_string(),
            typ: value,
        }
    }
}

impl VariableMetadata {
    pub fn f(name: String) -> Self {
        Self { name, typ: Float }
    }

    pub fn i(name: String) -> Self {
        Self { name, typ: Int }
    }

    pub fn c(name: String) -> Self {
        Self { name, typ: Char }
    }

    pub fn b(name: String) -> Self {
        Self { name, typ: Bool }
    }

    pub fn n(name: &str, t: DataType) -> Self {
        Self {
            name: name.to_string(),
            typ: t,
        }
    }

    pub fn new(name: String, t: DataType) -> Self {
        Self { name, typ: t }
    }
}
