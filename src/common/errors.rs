use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::vm::DataType;

#[derive(Debug)]
pub struct NoValue {
    pub(crate) msg: String,
}

impl Display for NoValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for NoValue {}

#[derive(Debug)]
pub struct SymbolNotFound {
    pub name: String,
}

impl Display for SymbolNotFound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "symbol {} not found", self.name)
    }
}

impl Error for SymbolNotFound {}

#[derive(Debug)]
pub(crate) struct TypeNotFound {
    pub(crate) typ: String,
}

impl Display for TypeNotFound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Error for TypeNotFound {}

#[derive(Debug)]
pub struct UnknownToken {
    pub(crate) source: String,
}

impl Display for UnknownToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "failed to parse remaining source: \"{}\"", self.source.replace("\n", "").escape_debug())
    }
}

impl Error for UnknownToken {}

#[derive(Debug)]
pub struct InvalidTypeException {
    pub(crate) expected: DataType,
    pub(crate) actual: Option<DataType>,
}

impl Display for InvalidTypeException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.actual {
            None => {
                write!(f, "expected {:?}, got None", self.expected)
            }
            Some(v) => {
                write!(f, "expected {:?}, got {:?}", self.expected, v)
            }
        }
    }
}

impl Error for InvalidTypeException {}

#[derive(Debug)]
pub struct InvalidOpcode {
    pub(crate) msg: String,
}

impl Display for InvalidOpcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for InvalidOpcode {}

#[derive(Debug)]
pub struct GenericException {
    pub(crate) msg: String,
}

impl Display for GenericException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for GenericException {}

#[derive(Debug)]
pub struct OutOfBoundsException {
    pub(crate) max: isize,
    pub(crate) index: isize,
    pub(crate) msg: String,
}

impl Display for OutOfBoundsException {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "tried to index {} out of bounds index {} bounds 0-{}",
            self.msg, self.index, self.index
        )
    }
}

impl Error for OutOfBoundsException {}