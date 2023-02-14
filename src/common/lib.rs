#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]


pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod vm;
pub mod objects;
pub mod bytecodeChecker;
mod tests;
pub mod fs;
pub mod optimalizer;
pub mod std;
mod ffi;
// pub mod ffi;
