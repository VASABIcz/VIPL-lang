#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]
#![feature(box_syntax)]
#![feature(const_box, const_heap)]
#![feature(stmt_expr_attributes)]
#![feature(let_chains)]
#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(pointer_byte_offsets)]

// FIXME this is just quick workaround
// #[global_allocator]
// static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

pub mod ast;
pub mod bytecodeChecker;
pub mod cGen;
pub mod codegen;
pub mod ffi;
pub mod fs;
pub mod gccWrapper;
pub mod lexer;
pub mod objects;
pub mod optimizer;
pub mod parser;
pub mod std;
pub mod vm;
pub mod strCaching;
mod tests;
mod rice;
