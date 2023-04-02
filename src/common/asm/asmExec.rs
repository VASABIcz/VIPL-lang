use libc::{c_void, c_int, PROT_READ, PROT_WRITE, PROT_EXEC, mprotect};
use std::ptr::{null_mut, copy_nonoverlapping};
use std::mem::{size_of, transmute, forget};
use std::alloc::{alloc, dealloc, Layout};
use std::arch::asm;
use std::fs;
use crate::vm::{ExternFn, StackFrame, VirtualMachine};

pub fn allocateBinFunction(machineCode: &mut [u8]) -> extern fn(&mut VirtualMachine, &mut StackFrame) -> () {
    // https://man7.org/linux/man-pages/man2/pkey_mprotect.2.html
    // linux page size = 4096
    let layout = Layout::from_size_align(machineCode.len(), 4096).unwrap();
    let ptr = unsafe { alloc(layout) };
    println!("orig ptr {:?}", ptr);

    // todo handle more pages
    unsafe { mprotect(ptr as *mut c_void, layout.size(), PROT_READ | PROT_WRITE | PROT_EXEC) };

    unsafe { copy_nonoverlapping(machineCode.as_ptr(), ptr, machineCode.len()) };

    unsafe { transmute(ptr) }
}

fn readBin(file: &str) -> Vec<u8> {
    fs::read(file).unwrap()
}

fn main() {
    let size = 4096; // allocate 1 page of memory
    let layout = Layout::from_size_align(size, 4096).unwrap();
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        eprintln!("malloc failed");
        std::process::exit(1);
    }

    // Mark the memory as executable
    let result = unsafe { mprotect(ptr as *mut c_void, size, PROT_READ | PROT_WRITE | PROT_EXEC) };
    if result != 0 {
        eprintln!("mprotect failed");
        std::process::exit(1);
    }

    // Use the memory as executable code
    let code: &[u8] = &readBin("/home/vasabi/programing/rust/asmGen/sus"); // &[0x4d, 0x31, 0xdb, 0x41, 0xbb, 0x45, 0x00, 0x00, 0x00, 0xc3];
    println!("{:?} {}", code, code.len());
    unsafe { copy_nonoverlapping(code.as_ptr(), ptr, code.len()) };
    let func: ExternFn = unsafe { transmute(ptr) };
    forget(code);
    let mut res = 0;

    // Execute the code

    // func();
    unsafe {
        asm!(
        "mov {res}, r12",
        res = out(reg) res
        )
    }
    println!("{}", res);

    // Free the memory
    unsafe { dealloc(ptr, layout) };
}