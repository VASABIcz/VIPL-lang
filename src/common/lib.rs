#![allow(non_snake_case)]
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
#![feature(core_intrinsics)]
#![feature(adt_const_params)]
#![feature(generic_const_exprs)]
#![feature(fn_traits)]
#![feature(unchecked_math)]
#![feature(abi_c_cmse_nonsecure_call)]
#![feature(get_many_mut)]
#![feature(slice_ptr_get)]
#![feature(try_trait_v2)]
#![allow(non_snake_case)]
#![feature(slice_internals)]
#![feature(pattern)]
#![feature(exclusive_range_pattern)]
#![deny(unused_must_use)]
#![deny(unused_assignments)]

// FIXME this is just quick workaround
// #[global_allocator]
// static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

extern crate core;
pub mod ast;
pub mod asm;
pub mod bytecodeGen;
pub mod errors;
pub mod fastAccess;
pub mod ffi;
pub mod lexer;
pub mod lexingUnits;
pub mod naughtyBox;
pub mod parser;
pub mod parsingUnits;
pub mod rice;
pub mod std;
pub mod utils;
pub mod vm;
pub mod viplParser;
pub mod wss;
pub mod symbolManager;
pub mod termon;
