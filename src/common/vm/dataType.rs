use crate::ast::Expression;
use crate::errors::{CodeGenError, TypeError};
use crate::vm::dataType::DataType::*;
use crate::vm::objects::Str;
use crate::vm::value::Value;
use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Object(ObjectMeta),
    Function {
        args: Vec<DataType>,
        ret: Box<DataType>,
    },
    Void,
    Value
}

impl DataType {
    // FIXME
    pub fn getArrayType(&self) -> Result<DataType, CodeGenError> {
        let a = self.asArray()?;
        a.generics.first().unwrap().clone().ok_or(CodeGenError::UntypedEmptyArray)
    }

    pub fn assertType(&self, t: DataType) -> Result<DataType, CodeGenError> {
        if self == &t {
            Ok(t)
        } else {
            Err(CodeGenError::TypeError(TypeError {
                expected: t,
                actual: self.clone(),
                exp: None,
            }))
        }
    }

    pub fn assertNotVoid(self) -> Result<DataType, CodeGenError> {
        if self != Void {
            Ok(self)
        } else {
            Err(CodeGenError::UnexpectedVoid)
        }
    }

    pub fn isString(&self) -> bool {
        match self {
            Object(o) => return o.name.as_str() == "String",
            _ => false,
        }
    }

    pub fn isArray(&self) -> bool {
        match self {
            Object(o) => return o.name.as_str() == "Array",
            _ => false,
        }
    }

    pub fn isValue(&self) -> bool {
        match self {
            Value => true,
            _ => false
        }
    }

    pub fn isVoid(&self) -> bool {
        match self {
            Void => true,
            _ => false,
        }
    }

    pub fn isInt(&self) -> bool {
        match self {
            Int => true,
            _ => false
        }
    }

    pub fn isFloat(&self) -> bool {
        match self {
            Float => true,
            _ => false
        }
    }

    pub fn isFunction(&self) -> bool {
        match self {
            Function { .. } => true,
            _ => false,
        }
    }

    pub fn asArray(&self) -> Result<&ObjectMeta, CodeGenError> {
        match self {
            Object(o) => {
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
        Object(ObjectMeta {
            name: "String".to_string(),
            generics: Box::new([]),
        })
    }

    pub fn arr(inner: Generic) -> Self {
        Object(ObjectMeta {
            name: "Array".to_string(),
            generics: Box::new([inner]),
        })
    }

    pub fn obj(name: &'static str) -> Self {
        Object(ObjectMeta {
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
            Object(x) => {
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
