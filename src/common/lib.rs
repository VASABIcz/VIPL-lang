#![feature(get_mut_unchecked)]
#![feature(downcast_unchecked)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]
#![feature(const_box, const_heap)]
#![feature(stmt_expr_attributes)]
#![feature(let_chains)]
#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(pointer_byte_offsets)]
#![feature(dir_entry_ext2)]
#![feature(core_intrinsics)]
#![feature(adt_const_params)]
#![feature(generic_const_exprs)]

// FIXME this is just quick workaround
// #[global_allocator]
// static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

extern crate core;

pub mod ast;
// pub mod bytecodeChecker;
// pub mod cGen;
// pub mod codegen;
pub mod ffi;
pub mod gccWrapper;
pub mod lexer;
pub mod optimizer;
pub mod parser;
pub mod strCaching;
pub mod rice;
pub mod asm;
pub mod bytecodeGen;
pub mod std;
pub mod errors;
pub mod utils;
pub mod variableMetadata;
pub mod vm;
