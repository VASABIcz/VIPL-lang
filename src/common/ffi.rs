use std::ffi::{c_char, CStr};
use std::mem::forget;

use crate::std::bootStrapVM;
use crate::vm::{DataType, MyStr, OpCode, run, SeekableOpcodes, StackFrame, Value, VirtualMachine};

// unsafe rust is so interesting :D
// FIXME

#[no_mangle]
pub extern fn createVm() -> *mut VirtualMachine {
    let vm = Box::new(bootStrapVM());
    let ptr = Box::into_raw(vm);
    forget(ptr);
    ptr
}

pub extern fn pushStack(vm: &mut VirtualMachine, value: &mut Value) {
    vm.stack.push(value.clone());
}


type rustFn = fn(&mut VirtualMachine, &mut StackFrame) -> ();
type cFn = extern fn(&mut VirtualMachine, &mut StackFrame) -> ();

enum FunctionWrapper {
    Rust(fn(&mut VirtualMachine, &mut StackFrame) -> ()),
    C(extern fn(&mut VirtualMachine, &mut StackFrame) -> ()),
}

impl From<cFn> for FunctionWrapper {
    fn from(value: cFn) -> Self {
        Self::C(value)
    }
}

impl From<rustFn> for FunctionWrapper {
    fn from(value: rustFn) -> Self {
        Self::Rust(value)
    }
}

impl FunctionWrapper {
    pub fn call(&self, vm: &mut VirtualMachine, locals: &mut StackFrame) {
        match self {
            FunctionWrapper::Rust(f) => f(vm, locals),
            FunctionWrapper::C(f) => f(vm, locals)
        }
    }
}

pub unsafe extern "C" fn registerNative(
    vm: &mut VirtualMachine,
    name: *const u8,
    nameLen: usize,
    args: *const DataType,
    argsLen: usize,
    callback: fn(&mut VirtualMachine, &mut StackFrame) -> (),
) {
    let mut nameCopy = String::with_capacity(nameLen);
    name.copy_to(nameCopy.as_mut_ptr(), nameLen);

    let mut argsCopy = Vec::with_capacity(argsLen);
    args.copy_to(argsCopy.as_mut_ptr(), argsLen);

    // let clonArgs = Box::from([]);
    vm.makeNative(nameCopy., argsCopy.iter().map().collect(), callback, None);
}

pub extern fn popStack(vm: &mut VirtualMachine) -> Option<Value> {
    vm.stack.pop()
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
            previous: None,
            localVariables: &mut [],
            name: None,
        })
    }
}