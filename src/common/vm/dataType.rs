use crate::errors::{CodeGenError, TypeError};
use crate::vm::dataType::DataType::*;
use crate::vm::value::Value;

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

    pub fn isFloat(&self) -> bool {
        matches!(self, RawDataType::Float)
    }

    pub fn isBool(&self) -> bool {
        matches!(self, RawDataType::Bool)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    Object(bool),
    Null
}

impl DataType {
    pub fn getRefName(&self) -> &str {
        &self.getReff().unwrap().name
    }

    pub fn isReference(&self) -> bool {
        matches!(self, DataType::Reference(_))
    }
    
    pub fn isReferenceLike(&self) -> bool {
        self.isObject() || self.isReference() || self.isNull()
    }

    pub fn isRefNamed(&self, name: &str) -> bool {
        match self {
            Reference(r) => r.name == name,
            _ => false
        }
    }

    pub fn isReferenceNullable(&self) -> bool {
        matches!(self, DataType::Reference(ObjectMeta{name, generics, nullable: true}))
    }

    pub fn isReferenceNonNullable(&self) -> bool {
        match self {
            Reference(v) => !v.nullable,
            _ => false
        }
    }

    pub fn isObjectNullable(&self) -> bool {
        matches!(self, DataType::Object(true))
    }

    pub fn isObjectNonNullable(&self) -> bool {
        matches!(self, DataType::Object(false))
    }

    pub fn isObject(&self) -> bool {
        matches!(self, DataType::Object(_))
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
            Null => RawDataType::Int,
            _ => Err(CodeGenError::ExpectedRawType)?
        })
    }

    pub fn toRawTypeWeak(self) -> Result<RawDataType, CodeGenError> {
        Ok(match self {
            Int => RawDataType::Int,
            Float => RawDataType::Float,
            Bool => RawDataType::Bool,
            Char => RawDataType::Char,
            _ => todo!()
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
                Err(CodeGenError::ExpectedReference)
            }
        }
    }

    pub fn getReff(&self) -> Result<&ObjectMeta, CodeGenError> {
        match self {
            Reference(v) => Ok(v),
            _ => {
                Err(CodeGenError::ExpectedReference)
            }
        }
    }

    pub fn getReffM(&mut self) -> Result<&mut ObjectMeta, CodeGenError> {
        match self {
            Reference(v) => Ok(v),
            _ => {
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
        return self.isRefNamed("String")
    }

    pub fn isArray(&self) -> bool {
        return self.isRefNamed("Array")
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

    pub fn isNull(&self) -> bool {
        matches!(self, Null)
    }

    pub fn isFloat(&self) -> bool {
        matches!(self, Float)
    }

    pub fn isBool(&self) -> bool {
        matches!(self, Bool)
    }

    pub fn isBoolLike(&self) -> bool {
        self.isBool() || self.isRefNamed("Bool")
    }

    pub fn isIntLIke(&self) -> bool {
        self.isInt() || self.isRefNamed("Int")
    }

    pub fn isChar(&self) -> bool {
        matches!(self, Char)
    }

    pub fn isNumeric(&self) -> bool {
        matches!(self, Float | Int)
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
                    Err(CodeGenError::TypeError(TypeError::newNone(DataType::arr(Generic::Any), self.clone())))
                }
            }
            v => Err(CodeGenError::TypeError(TypeError::newNone(DataType::arr(Generic::Any),self.clone())))
        }
    }

    pub fn str() -> Self {
        Reference(ObjectMeta {
            name: "String".to_string(),
            generics: Box::new([]),
            nullable: false,
        })
    }

    pub fn arr(inner: Generic) -> Self {
        Reference(ObjectMeta {
            name: "Array".to_string(),
            generics: Box::new([inner]),
            nullable: false,
        })
    }

    pub fn obj(name: &str) -> Self {
        Reference(ObjectMeta::nunNull(name))
    }

    pub fn nullObj(name: &str) -> Self {
        Reference(ObjectMeta::nullable(name))
    }

    pub fn toString(&self) -> String {
        match self {
            Int => "int".to_string(),
            Float => "float".to_string(),
            Bool => "bool".to_string(),
            Value => "value".to_string(),
            Reference(x) => {
                if x.generics.len() == 0 {
                    if x.nullable {
                        return format!("{}?", x.name)
                    }
                    else {
                        x.name.clone()
                    }
                } else {
                    format!(
                        "{}<{}>{}",
                        x.name,
                        x.generics
                            .iter()
                            .map(|it| it.toString())
                            .collect::<Vec<_>>()
                            .join(", "),
                        if x.nullable { "?" } else { "" }
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
            Object(n) => if !n { "object".to_string() } else { "object?".to_string() },
            Null => "null".to_string()
        }
    }

    #[inline]
    pub fn toDefaultValue(&self) -> Value {
        match self {
            Void => unreachable!(),
            _ => Value::null()
        }
    }

    pub fn toCompatibleTypesIncluding(&self) -> Vec<DataType> {
        let mut buf = vec![];
        buf.push(self.clone());

        match self {
            Int => {
                buf.push(DataType::obj("Int"));
                buf.push(DataType::nullObj("Int"));
            }
            Float => {
                buf.push(DataType::obj("Float"));
                buf.push(DataType::nullObj("Float"));
            }
            Bool => {
                buf.push(DataType::obj("Bool"));
                buf.push(DataType::nullObj("Bool"));
            }
            Char => {
                buf.push(DataType::obj("Char"));
                buf.push(DataType::nullObj("Char"));
            }
            // FIXME missing case that we want to convert boxed value into primitive
            Reference(meta) => {
                if !meta.nullable {
                    let mut clon = meta.clone();
                    clon.nullable = true;
                    buf.push(Reference(clon))
                }
            }
            Void => {
                return vec![]
            }
            _ => {}
        }

        buf.push(DataType::Value);

        buf
    }

    pub fn supportsEquals(&self) -> bool {
        !matches!(self, Void | Value)
    }

    pub fn supportsComparisson(&self) -> bool {
        self.isPrimitiveType() || self.isBoxed()
    }

    pub fn isPrimitiveType(&self) -> bool {
        matches!(self, Int | Float | Bool | Char)
    }

    pub fn isBoxed(&self) -> bool {
        self.isRefNamed("Int") || self.isRefNamed("Float") || self.isRefNamed("Bool") || self.isRefNamed("Char")
    }

    pub fn isBoxedNonNull(&self) -> bool {
        self.isBoxed() && self.isReferenceNonNullable()
    }

    pub fn supportsArithmetics(&self) -> bool {
        self.isPrimitiveType() || self.isBoxed()
    }

    pub fn toBoxedType(self) -> DataType {
        match self {
            Int => DataType::obj("Int"),
            Float => DataType::obj("Float"),
            Bool => DataType::obj("Bool"),
            Char => DataType::obj("Char"),
            _ => panic!()
        }
    }

    pub fn toNullable(self) -> DataType {
        match self {
            Reference(mut o) => {
                o.nullable = true;

                Reference(o)
            }
            t => panic!("expected reference got {:?}", t)
        }
    }

    pub fn toUnboxedType(self) -> DataType {
        if self.isBoxed() {
            match self.getRef().unwrap().name.as_str() {
                "Int" => Int,
                "Float" => Float,
                "Bool" => Bool,
                "Char" => Char,
                _ => unreachable!()
            }
        }
        else if self.isPrimitiveType() {
            self
        }
        else {
            unreachable!()
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct ObjectMeta {
    pub name: String,
    pub generics: Box<[Generic]>,
    pub nullable: bool
}

impl ObjectMeta {
    pub fn new(name: &str, nullable: bool) -> Self {
        Self {
            name: name.to_string(),
            generics: Box::new([]),
            nullable,
        }
    }

    pub fn nullable(name: &str) -> Self {
        Self {
            name: name.to_string(),
            generics: Box::new([]),
            nullable: true,
        }
    }

    pub fn nunNull(name: &str) -> Self {
        Self {
            name: name.to_string(),
            generics: Box::new([]),
            nullable: false,
        }
    }
}
