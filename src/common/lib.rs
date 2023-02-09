#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]

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
