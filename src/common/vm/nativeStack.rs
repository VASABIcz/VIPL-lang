use std::alloc::{alloc, Global, Layout};
use std::arch::asm;
use std::hint::black_box;
use std::ops::{Add, Sub};
use std::ptr;

const SIZE: usize = 256 + 16;

#[derive(Debug)]
pub struct StackManager<const SIZE: usize> {
    pub nativeStackBase: usize,
    pub nativeStackOffset: usize,
    pub cStack: usize,
}

impl<const SIZE: usize> StackManager<SIZE> {
    pub fn new() -> Self {
        let p = Box::into_raw(Box::new([0usize; SIZE]));
        Self {
            nativeStackBase: p as usize,
            nativeStackOffset: 0,
            cStack: 0,
        }
    }

    #[inline(always)]
    pub fn getRbpRsp() -> (usize, usize) {
        let mut rbp = 0;
        let mut rsp = 0;
        unsafe {
            asm!(
            "mov {rbp}, rbp",
            "mov {rsp}, rsp",
            rsp = out(reg) rsp,
            rbp = out(reg) rbp
            )
        }
        return (rbp, rsp);
    }

    #[inline(always)]
    pub fn setupStack(ptr: usize, size: usize) {
        unsafe {
            asm!(
            // copy rsp and rbp to r8,9
            "mov r8, rsp",
            "mov r9, rbp",
            // set rsp and rbp to new stack ptr
            "mov rsp, {nativePtr}",
            "mov rbp, {nativeBase}",
            // save original rsp, rbp on new stack
            "push r8",
            "push r9",
            // "push 77",
            nativePtr = in(reg) ptr as usize+size,
            nativeBase = in(reg) ptr as usize
            );
        }
    }

    #[inline(always)]
    pub fn callNative(&mut self, f: fn() -> ()) {
        Self::setupStack(self.nativeStackBase, SIZE);
        f();
        restoreStack();
    }

    #[inline(always)]
    pub fn callC(&self, f: fn() -> ()) {
        Self::setupStack(self.cStack, 1)
    }
}

pub fn testProc(a: &usize, b: &usize) {
    println!("{}", a * b);
    black_box(a);
    black_box(a);
}

pub fn printRegisters() {
    let x = 0usize;
    let c = 1usize;
    let c = 2usize;
    let c = 3usize;
    let c = 4usize;
    println!("v {}", &x as *const usize as usize);
    let mut rbpReg = 0usize;
    let mut rspReg = 0usize;
    unsafe {
        asm!(
        "mov {rbpReg}, rbp",
        "mov {rspReg}, rsp",
        rspReg = out(reg) rspReg,
        rbpReg = out(reg) rbpReg
        )
    }
    // println!("===");
    println!("s {}", rspReg);
    println!("b {}", rbpReg);
    // unsafe { println!("{:?}", &*((rbpReg as *mut usize).sub(SIZE) as *mut [usize; SIZE])) }
    // println!("===");
}

#[inline(always)]
pub fn restoreStack() {
    unsafe {
        asm!("pop rbp", "pop rsp");
    }
}

pub fn testStack(fin: bool) {
    for _ in 0..10 {
        if !fin {
            testStack(true)
        }
        let a = 12;
        let b = 64;
        let d = 1;
        let mut x = a * a - b + d;
        x += a + b + d;
        println!("{}", x)
    }
}

#[inline(never)]
pub fn myFunc() {
    let c = 69usize;
    println!("v {}", &c as *const usize as usize);
    let c1 = 420usize;
    testProc(&c, &c1);
    black_box(&c);
    black_box(&c1);
}

fn main() {
    let mut bpx = Box::new([3usize; SIZE]);
    println!("{}", &bpx as *const Box<[usize; SIZE]> as usize);
    println!("f {}", bpx.first().unwrap() as *const usize as usize);
    println!("l {}", bpx.last().unwrap() as *const usize as usize);
    let mut ptr = bpx.as_mut_ptr();
    Box::into_raw(bpx);
    println!("{:?}", ptr);
    println!("a {}", ptr as usize);
    unsafe {
        println!("b {}", ptr.add(SIZE - 1) as usize);
    }
    unsafe {
        asm!(
        // copy rsp and rbp to r8,9
        "mov r8, rsp",
        "mov r9, rbp",
        // set rsp and rbp to new stack ptr
        "mov rsp, {nativePtr}",
        "mov rbp, {nativeBase}",
        // save original rsp, rbp on new stack
        "push r8",
        "push r9",
        nativePtr = in(reg) ptr.add(SIZE),
        nativeBase = in(reg) ptr.add(SIZE),
        );
    }
    myFunc();
    // printRegisters();
    // println!("KYS123");
    unsafe {
        asm!("pop rbp", "pop rsp",);
    }
    println!("{:?}", ptr as usize);
}
