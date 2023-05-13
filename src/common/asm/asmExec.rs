use libc::{c_void, c_int, PROT_READ, PROT_WRITE, PROT_EXEC, mprotect};
use std::ptr::{null_mut, copy_nonoverlapping};
use std::mem::{size_of, transmute, forget};
use std::alloc::{alloc, dealloc, Layout};
use std::arch::asm;
use std::fs;
use crate::vm::heap::Allocation;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::{ExternFn, VirtualMachine};

pub fn allocateBinFunction(machineCode: &mut [u8]) -> extern fn(&mut VirtualMachine, &mut StackFrame) -> Value {
    if machineCode.len() > 4096 {
        todo!()
    }

    // https://man7.org/linux/man-pages/man2/pkey_mprotect.2.html
    // linux page size = 4096
    let layout = Layout::from_size_align(machineCode.len(), 4096).unwrap();
    let ptr = unsafe { alloc(layout) };

    // todo handle more pages
    unsafe { mprotect(ptr as *mut c_void, layout.size(), PROT_READ | PROT_WRITE | PROT_EXEC) };

    unsafe { copy_nonoverlapping(machineCode.as_ptr(), ptr, machineCode.len()) };

    unsafe { transmute(ptr) }
}

fn readBin(file: &str) -> Vec<u8> {
    fs::read(file).unwrap()
}