use crate::vm::{DataType, ObjectMeta, VariableMetadata, VirtualMachine};

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod serialization;
pub mod vm;
pub mod objects;
pub mod bytecodeChecker;
mod tests;
pub mod fs;
pub mod optimalizer;
pub mod std;
