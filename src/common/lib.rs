#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]
#![feature(box_syntax)]
#![feature(const_box, const_heap)]
#![feature(stmt_expr_attributes)]
#![feature(let_chains)]

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod vm;
pub mod objects;
pub mod bytecodeChecker;
pub mod fs;
pub mod std;
mod ffi;
pub mod optimizer;
mod tests;
pub mod cGen;
// pub mod ffi;
