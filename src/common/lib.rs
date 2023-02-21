#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]
#![feature(box_syntax)]
#![feature(const_box, const_heap)]
#![feature(stmt_expr_attributes)]
#![feature(let_chains)]

pub mod ast;
pub mod bytecodeChecker;
pub mod cGen;
pub mod codegen;
mod ffi;
pub mod fs;
pub mod gccWrapper;
pub mod lexer;
pub mod objects;
pub mod optimizer;
pub mod parser;
pub mod std;
mod tests;
pub mod vm;
// pub mod cGen;
// pub mod ffi;
