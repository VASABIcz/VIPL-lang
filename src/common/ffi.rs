use std::mem::forget;
use std::ptr;

use crate::codegen::bytecodeGen;
use crate::lexer::{lexingUnits, SourceProvider};
use crate::std::bootStrapVM;
use crate::vm::{DataType, MyStr, OpCode, run, SeekableOpcodes, StackFrame, Value, VirtualMachine};

#[no_mangle]
pub extern fn createVm() -> *mut VirtualMachine {
    let vm = Box::new(bootStrapVM());
    let ptr = Box::into_raw(vm);
    forget(ptr);
    ptr
}

#[no_mangle]
pub extern fn pushStack(vm: &mut VirtualMachine, value: &mut Value) {
    vm.stack.push(value.clone());
}

#[no_mangle]
pub extern fn registerNative(
    vm: &mut VirtualMachine,
    name: *const u8,
    nameLen: usize,
    args: *const DataType,
    argsLen: usize,
    callback: extern fn(&mut VirtualMachine, &mut StackFrame) -> (),
) {
    let mut buf = vec![0u8; nameLen];
    unsafe { name.copy_to(buf.as_mut_ptr(), nameLen); }
    let nameCopy = unsafe { String::from_utf8_unchecked(buf) };

    let mut argsCopy = Vec::with_capacity(argsLen);
    unsafe { args.copy_to(argsCopy.as_mut_ptr(), argsLen); }

    vm.makeExtern(nameCopy, argsCopy.into_iter().map(|it| { it.into() }).collect(), callback, None);
}

#[no_mangle]
pub extern fn popStack(vm: &mut VirtualMachine) -> Option<Value> {
    vm.stack.pop()
}

#[no_mangle]
pub extern fn evaluate(vm: &mut VirtualMachine, d: *const u8, len: usize) {
    let mut buf = vec![0u8; len];
    unsafe { d.copy_to(buf.as_mut_ptr(), len); }
    let mut s = unsafe { String::from_utf8_unchecked(buf) };

    unsafe { d.copy_to(s.as_mut_ptr(), len); }

    println!("{:?}", &s);
    println!("{d:?} {len:?}");

    let res = match unsafe { crate::lexer::tokenize(&mut lexingUnits(), SourceProvider { data: &s, index: 0 }) } {
        Ok(v) => v,
        Err(_) => {
            println!("lexer failed");
            return;
        }
    };

    println!("{:?}", &res);

    let ast = match crate::parser::parseTokens(res) {
        Ok(v) => v,
        Err(_) => {
            println!("parser failed");
            return;
        }
    };

    println!("{:?}", &ast);

    let opCodes = match bytecodeGen(ast) {
        Ok(v) => v,
        Err(_) => {
            println!("codegen failed");
            return;
        }
    };

    println!("{:?}", &opCodes);

    for _ in 0..opCodes.0.len() {
        vm.opCodeCache.push(None)
    }

    run(&mut SeekableOpcodes {
        index: 0,
        opCodes: &opCodes.0,
        start: None,
        end: None,
    }, vm, &mut StackFrame::new(&mut opCodes.1.into_iter().map(|it| { it.into() }).collect::<Vec<Value>>()));


    println!("finished");
}

#[no_mangle]
pub extern fn test(vm: *mut VirtualMachine) {
    println!("i am here");
    let ops = vec![OpCode::PushInt(69), OpCode::Call { encoded: MyStr::Static("print(int)") }];

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
            // previous: None,
            localVariables: &mut [],
            name: None,
        })
    }
}

#[no_mangle]
pub extern "C" fn dropVm(ptr: *mut VirtualMachine) {
    unsafe { ptr::drop_in_place(ptr); }
}