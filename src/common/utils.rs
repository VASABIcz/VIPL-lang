use std::error::Error;
use crate::lexer::tokenizeSource;
use crate::parser::{parseDataType, TokenProvider};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::DataType;

// same as Vec but can be unsafely modified and accessed
pub struct FastVec<T> {
    pub cap: usize,
    pub ptr: *mut T,
    pub size: usize
}

#[inline]
pub fn genFunName(name: &str, args: &[DataType]) -> String {
    format!("{}({})", name, argsToString(args))
}

#[inline]
pub fn genFunNameMeta(name: &str, args: &[VariableMetadata], argsLen: usize) -> String {
    format!("{}({})", name, argsToStringMeta(&args[0..argsLen]))
}

#[inline]
pub fn argsToStringMeta(args: &[VariableMetadata]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(&arg.typ.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn argsToString(args: &[DataType]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(&arg.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn parseDataTypeFromStr(s: &str) -> Result<DataType, Box<dyn Error>> {
    let p = tokenizeSource(s)?;
    parseDataType(&mut TokenProvider::new(p))
}