use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};
use crate::vm::myStr::MyStr;

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct VariableMetadata {
    pub name: MyStr,
    pub typ: DataType,
}

impl From<DataType> for VariableMetadata {
    fn from(value: DataType) -> Self {
        Self {
            name: "unknown".into(),
            typ: value,
        }
    }
}

impl VariableMetadata {
    pub fn f(name: MyStr) -> Self {
        Self { name, typ: Float }
    }

    pub fn i(name: MyStr) -> Self {
        Self { name, typ: Int }
    }

    pub fn c(name: MyStr) -> Self {
        Self { name, typ: Char }
    }

    pub fn b(name: MyStr) -> Self {
        Self { name, typ: Bool }
    }
}