use crate::vm::heap::Allocation;
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::{ExternFn, VirtualMachine};
use std::alloc::{alloc, dealloc, Layout};
use std::arch::asm;
use std::fs;
use std::mem::{forget, size_of, transmute};
use std::ptr::{copy_nonoverlapping, null_mut};

#[cfg(target_os = "windows")]
pub fn allocateBinFunction(
    machineCode: &mut [u8],
) -> extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> Value {
    todo!()
}

#[cfg(target_os = "linux")]
pub fn allocateBinFunction(
    machineCode: &mut [u8],
) -> extern "C" fn(&mut VirtualMachine, &mut StackFrame) -> Value {
    use libc;
    if machineCode.len() > 4096 {
        todo!()
    }

    // https://man7.org/linux/man-pages/man2/pkey_mprotect.2.html
    // linux page size = 4096
    let layout = Layout::from_size_align(machineCode.len(), 4096).unwrap();
    let ptr = unsafe { alloc(layout) };

    // todo handle more pages
    unsafe {
        libc::mprotect(
            ptr as *mut libc::c_void,
            layout.size(),
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
        )
    };

    unsafe { copy_nonoverlapping(machineCode.as_ptr(), ptr, machineCode.len()) };

    unsafe { transmute(ptr) }
}

fn readBin(file: &str) -> Vec<u8> {
    fs::read(file).unwrap()
}
