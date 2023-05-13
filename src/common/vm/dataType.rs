use std::error::Error;
use crate::ast::Expression;
use crate::errors::{CodeGenError, TypeError};
use crate::vm::dataType::DataType::*;
use crate::vm::myStr::MyStr;
use crate::vm::value::Value;

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Object(ObjectMeta),
    Function{
        args: Vec<DataType>,
        ret: Box<DataType>
    },
    Void
}

impl DataType {
    pub fn isString(&self) -> bool {
        match self {
            Object(o) => {
                return o.name.as_str() == "String"
            }
            _ => false
        }
    }

    pub fn isArray(&self) -> bool {
        match self {
            Object(o) => {
                return o.name.as_str() == "Array"
            }
            _ => false
        }
    }

    pub fn isVoid(&self) -> bool {
        match self {
            Void => true,
            _ => false
        }
    }

    pub fn asArray(&self) -> Result<&ObjectMeta, CodeGenError>  {
        match self {
            Object(o) => {
                if o.name.as_str() == "Array" {
                    Ok(o)
                }
                else {
                    Err(CodeGenError::TypeError(TypeError{
                        expected: DataType::arr(Generic::Any),
                        actual: self.clone(),
                        exp: None,
                    }))
                }
            }
            v => Err(CodeGenError::TypeError(TypeError{
                expected: DataType::arr(Generic::Any),
                actual: self.clone(),
                exp: None,
            }))
        }
    }

    pub fn str() -> Self {
        Object(ObjectMeta {
            name: MyStr::Static("String"),
            generics: Box::new([]),
        })
    }

    pub fn arr(inner: Generic) -> Self {
        Object(ObjectMeta {
            name: MyStr::Static("Array"),
            generics: Box::new([inner]),
        })
    }

    pub fn obj(name: &'static str) -> Self {
        Object(ObjectMeta {
            name: MyStr::Static(name),
            generics: Box::new([]),
        })
    }

    pub fn toString(&self) -> String {
        match self {
            Int => "int".to_string(),
            Float => "float".to_string(),
            Bool => "bool".to_string(),
            Object(x) => x.name.clone().to_string(),
            Char => "char".to_string(),
            Void => "!".to_string(),
            Function { args, ret } =>
                format!("({}): {}", args.iter().map(|it| {
                    it.toString()
                }).collect::<Vec<_>>().join(", "), ret.toString())
        }
    }

    pub fn toCString(&self) -> &str {
        match self {
            Int => "long",
            Float => "float",
            Bool => "bool",
            Object(_) => "ViplObject*",
            Char => "char",
            Void => "void",
            Function { .. } => todo!()
        }
    }

    #[inline]
    pub fn toDefaultValue(&self) -> Value {
        match self {
            Int => 0.into(),
            Float => 0.0.into(),
            Bool => false.into(),
            Char => 0.into(),
            Object(_) => 0.into(),
            Function { .. } => 0.into(),
            Void => unreachable!(),
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
    pub fn ok_or<E>(self, err: E) -> Result<DataType, E> {
        match self {
            Generic::Any => Err(err),
            Generic::Type(v) => Ok(v),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct ObjectMeta {
    pub name: MyStr,
    pub generics: Box<[Generic]>,
}

impl ObjectMeta {
    pub fn new(name: &'static str) -> Self {
        Self {
            name: MyStr::Static(name),
            generics: Box::new([]),
        }
    }
}