use std::mem;
use std::mem::size_of;

use crate::ast::Op;
use crate::lexer::{lexingUnits, SourceProvider, tokenize, TokenType};
use crate::parser::{ArithmeticParsingUnit, BoolParsingUnit, CallParsingUnit, FunctionParsingUnit, IfParsingUnit, NumericParsingUnit, parse, ParsingUnit, StatementVarCreateParsingUnit, TokenProvider, VariableParsingUnit, WhileParsingUnit};
use crate::parser::ParsingUnitSearchType::Ahead;
use crate::serialization::{deserialize, serialize};
use crate::vm::{DataType, JmpType, OpCode, SeekableOpcodes, StackFrame, Value};
use crate::vm::DataType::*;
use crate::vm::OpCode::*;
use crate::vm::Value::Flo;

pub fn test() {
    println!("fuck")
}

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod serialization;
pub mod vm;
pub mod tests;