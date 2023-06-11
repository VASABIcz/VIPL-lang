use crate::ast::{RawExpression};
use crate::errors::{CodeGenError, TypeError};
use crate::vm::dataType::DataType::*;
use crate::vm::objects::Str;
use crate::vm::value::Value;
use std::error::Error;

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum RawDataType {
    Int,
    Float,
    Bool,
    Char,
}

impl RawDataType {
    pub fn toType(&self) -> DataType {
        match self {
            RawDataType::Int => DataType::Int,
            RawDataType::Float => DataType::Float,
            RawDataType::Bool => DataType::Bool,
            RawDataType::Char => DataType::Char
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Reference(ObjectMeta),
    Function {
        args: Vec<DataType>,
        ret: Box<DataType>,
    },
    Void,
    Value,
    Object
}

impl DataType {
    pub fn isReference(&self) -> bool {
        matches!(self, DataType::Reference(_))
    }

    pub fn isObject(&self) -> bool {
        matches!(self, DataType::Object)
    }

    pub fn toRawType(self) -> Result<RawDataType, CodeGenError> {
        Ok(match self {
            Int => RawDataType::Int,
            Float => RawDataType::Float,
            Bool => RawDataType::Bool,
            Char => RawDataType::Char,
            // FIXME this is stupid check if ref is null workaround
            // maybe introduce isnull opcode? idk
            Reference(_) => RawDataType::Int,
            _ => Err(CodeGenError::ExpectedRawType)?
        })
    }

    // FIXME
    pub fn getArrayType(&self) -> Result<DataType, CodeGenError> {
        let a = self.asArray()?;
        a.generics.first().unwrap().clone().ok_or(CodeGenError::UntypedEmptyArray)
    }

    pub fn getRef(self) -> Result<ObjectMeta, CodeGenError> {
        match self {
            Reference(v) => Ok(v),
            _ => {
                panic!();
                Err(CodeGenError::ExpectedReference)
            }
        }
    }

    pub fn getFunction(self) -> Result<(Vec<DataType>, DataType), CodeGenError> {
        match self {
            Function { args, ret } => Ok((args, *ret)),
            _ => Err(CodeGenError::VeryBadState)
        }
    }

    pub fn isString(&self) -> bool {
        match self {
            Reference(o) => return o.name.as_str() == "String",
            _ => false,
        }
    }

    pub fn isArray(&self) -> bool {
        match self {
            Reference(o) => return o.name.as_str() == "Array",
            _ => false,
        }
    }

    pub fn isValue(&self) -> bool {
        matches!(self, Value)
    }

    pub fn isVoid(&self) -> bool {
        matches!(self, Void)
    }

    pub fn isInt(&self) -> bool {
        matches!(self, Int)
    }

    pub fn isFloat(&self) -> bool {
        matches!(self, Float)
    }

    pub fn isFunction(&self) -> bool {
        matches!(self, Function { .. })
    }

    pub fn asArray(&self) -> Result<&ObjectMeta, CodeGenError> {
        match self {
            Reference(o) => {
                if o.name.as_str() == "Array" {
                    Ok(o)
                } else {
                    Err(CodeGenError::TypeError(TypeError {
                        expected: DataType::arr(Generic::Any),
                        actual: self.clone(),
                        exp: None,
                    }))
                }
            }
            v => Err(CodeGenError::TypeError(TypeError {
                expected: DataType::arr(Generic::Any),
                actual: self.clone(),
                exp: None,
            })),
        }
    }

    pub fn str() -> Self {
        Reference(ObjectMeta {
            name: "String".to_string(),
            generics: Box::new([]),
        })
    }

    pub fn arr(inner: Generic) -> Self {
        Reference(ObjectMeta {
            name: "Array".to_string(),
            generics: Box::new([inner]),
        })
    }

    pub fn obj(name: &'static str) -> Self {
        Reference(ObjectMeta {
            name: name.to_string(),
            generics: Box::new([]),
        })
    }

    pub fn toString(&self) -> String {
        match self {
            Int => "int".to_string(),
            Float => "float".to_string(),
            Bool => "bool".to_string(),
            Value => "value".to_string(),
            Reference(x) => {
                if x.generics.len() == 0 {
                    return x.name.clone();
                } else {
                    format!(
                        "{}<{}>",
                        x.name,
                        x.generics
                            .iter()
                            .map(|it| it.toString())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Char => "char".to_string(),
            Void => "!".to_string(),
            Function { args, ret } => format!(
                "({}): {}",
                args.iter()
                    .map(|it| { it.toString() })
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.toString()
            ),
            Object => "object".to_string()
        }
    }

    #[inline]
    pub fn toDefaultValue(&self) -> Value {
        match self {
            Void => unreachable!(),
            _ => Value::null()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum Generic {
    Any,
    Type(DataType),
}

impl Generic {
    pub fn toString(&self) -> String {
        match self {
            Generic::Any => "*".to_string(),
            Generic::Type(t) => t.toString(),
        }
    }

    pub fn ok_or<E>(self, err: E) -> Result<DataType, E> {
        match self {
            Generic::Any => Err(err),
            Generic::Type(v) => Ok(v),
        }
    }

    pub fn ok_or_else<E, F>(self, err: F) -> Result<DataType, E>
        where
            F: FnOnce() -> E,
    {
        match self {
            Generic::Type(v) => Ok(v),
            Generic::Any => Err(err()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct ObjectMeta {
    pub name: String,
    pub generics: Box<[Generic]>,
}

impl ObjectMeta {
    pub fn new(name: &'static str) -> Self {
        Self {
            name: name.to_string(),
            generics: Box::new([]),
        }
    }
}
