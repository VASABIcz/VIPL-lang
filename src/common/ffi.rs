use std::ffi::{c_char, CStr};
use std::mem::forget;

use crate::std::bootStrapVM;
use crate::vm::{DataType, OpCode, run, SeekableOpcodes, StackFrame, VirtualMachine};

// unsafe rust is so interesting :D

#[no_mangle]
pub extern "C" fn createVm() -> *mut VirtualMachine {
    let vm = Box::new(bootStrapVM());
    let ptr = Box::into_raw(vm);
    forget(ptr);
    ptr
}

pub extern "C" fn registerNative(vm: *mut VirtualMachine, name: *const c_char, args: *const DataType, callback: extern fn(*mut VirtualMachine, *mut StackFrame) -> ()) {
    todo!()
}

pub extern "C" fn pushStack(vm: *mut VirtualMachine) {
    todo!()
}

#[no_mangle]
pub extern "C" fn test(vm: *mut VirtualMachine) {
    println!("i am here");
    let ops = vec![OpCode::PushInt(69), OpCode::Call { encoded: String::from("print(int)").into_boxed_str() }];

    unsafe {
        for _ in 0..ops.len() {
            (*vm).opCodeCache.push(None)
        }
    }

    unsafe {
        run(&mut SeekableOpcodes {
            index: 0,
            opCodes: &ops,
            start: None,
            end: None,
        }, &mut *vm as &mut VirtualMachine, &mut StackFrame {
            previous: None,
            localVariables: &mut [],
            name: None,
        })
    }
}