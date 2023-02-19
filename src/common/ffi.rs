use std::ffi::{c_char, CStr};
use std::fmt::{Debug, Formatter};
use std::mem::forget;
use std::ptr;
use std::rc::Rc;
use std::str::Utf8Error;

use crate::codegen::bytecodeGen;
use crate::lexer::{lexingUnits, SourceProvider};
use crate::objects::{Str, ViplObject};
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

    let mut opCodes = match bytecodeGen(ast) {
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
        opCodes: &mut opCodes.0
    }, vm, &mut StackFrame::new(&mut opCodes.1.into_iter().map(|it| { it.into() }).collect::<Vec<Value>>()));


    println!("finished");
}

#[no_mangle]
pub extern fn test(vm: *mut VirtualMachine) {
    println!("i am here");
    let mut ops = vec![OpCode::PushInt(69), OpCode::Call { encoded: MyStr::Static("print(int)") }];

    unsafe {
        for _ in 0..ops.len() {
            (*vm).opCodeCache.push(None)
        }
    }

    unsafe {
        run(&mut SeekableOpcodes {
            index: 0,
            opCodes: &mut ops
        }, &mut *vm as &mut VirtualMachine, &mut StackFrame {
            // previous: None,
            localVariables: &mut [],
            name: None,
        })
    }
}

#[no_mangle]
pub extern fn dropVm(ptr: *mut VirtualMachine) {
    unsafe { ptr::drop_in_place(ptr); }
}


#[no_mangle]
pub extern fn pushInt(vm: &mut VirtualMachine, v: isize) {
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn pushFloat(vm: &mut VirtualMachine, v: f32) {
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn pushChar(vm: &mut VirtualMachine, v: u8) {
    vm.stack.push(Value::from(v as char))
}

#[no_mangle]
pub extern fn pushBool(vm: &mut VirtualMachine, v: bool) {
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn pushRef(vm: &mut VirtualMachine, v: *const ViplObject) {
    unsafe { vm.stack.push(Value::Reference { instance: Some(Rc::from_raw(v)) }) }
}

#[no_mangle]
pub extern fn popInt(vm: &mut VirtualMachine) -> isize {
    vm.stack.pop().unwrap().getNum()
}

#[no_mangle]
pub extern fn popFloat(vm: &mut VirtualMachine) -> f32 {
    vm.stack.pop().unwrap().getFlo()
}

#[no_mangle]
pub extern fn popChar(vm: &mut VirtualMachine) -> u8 {
    vm.stack.pop().unwrap().getChar() as u8
}

#[no_mangle]
pub extern fn popBool(vm: &mut VirtualMachine) -> bool {
    vm.stack.pop().unwrap().getBool()
}

#[no_mangle]
pub extern fn popRef(vm: &mut VirtualMachine) -> *const ViplObject {
    let a = vm.stack.pop().unwrap().getReferenceValue().unwrap();
    let ptr = Rc::into_raw(a);
    ptr
}

#[no_mangle]
pub extern fn getLocalsInt(vm: &mut StackFrame, index: usize) -> isize {
    unsafe { vm.localVariables.get_unchecked_mut(index) }.getNum()
}

#[no_mangle]
pub extern fn getLocalsFloat(vm: &mut StackFrame, index: usize) -> f32 {
    unsafe { vm.localVariables.get_unchecked_mut(index) }.getFlo()
}

#[no_mangle]
pub extern fn getLocalsChar(vm: &mut StackFrame, index: usize) -> u8 {
    unsafe { vm.localVariables.get_unchecked_mut(index) }.getChar() as u8
}

#[no_mangle]
pub extern fn getLocalsBool(vm: &mut StackFrame, index: usize) -> bool {
    unsafe { vm.localVariables.get_unchecked_mut(index) }.getBool()
}

#[no_mangle]
pub extern fn getLocalsRef(vm: &mut StackFrame, index: usize) -> *const ViplObject {
    let ptr = unsafe { vm.localVariables.get_unchecked_mut(index) }.getReference().clone().unwrap();
    Rc::into_raw(ptr)
}

#[no_mangle]
pub extern fn call(vm: &mut VirtualMachine, s: *const c_char) {
    let name = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
    vm.call(MyStr::Runtime(name.to_owned().into_boxed_str()))
}

#[no_mangle]
pub extern fn stringNew(vm: &mut VirtualMachine, s: *const c_char) -> *const ViplObject {
    let st = unsafe { CStr::from_ptr(s) }.to_str().unwrap().to_owned();
    let rc = Rc::new(ViplObject::Str(Str { string: st }));
    Rc::into_raw(rc)
}

#[no_mangle]
pub extern fn stringGetChar(vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> u8 {
    match obj {
        ViplObject::Str(v) => {
            *v.string.as_bytes().get(index).unwrap()
        }
        _ => panic!()
    }
}

#[repr(C)]
pub struct NativeWrapper {
    pub pushInt: extern fn(&mut VirtualMachine, isize) -> (),
    pub pushFloat: extern fn(&mut VirtualMachine, f32) -> (),
    pub pushBool: extern fn(&mut VirtualMachine, bool) -> (),
    pub pushChar: extern fn(&mut VirtualMachine, u8) -> (),
    pub pushRef: extern fn(&mut VirtualMachine, *const ViplObject) -> (),

    pub popInt: extern fn(&mut VirtualMachine) -> isize,
    pub popFloat: extern fn(&mut VirtualMachine) -> f32,
    pub popBool: extern fn(&mut VirtualMachine) -> bool,
    pub popChar: extern fn(&mut VirtualMachine) -> u8,
    pub popRef: extern fn(&mut VirtualMachine) -> *const ViplObject,

    pub getLocalsInt: extern fn(&mut StackFrame, usize) -> isize,
    pub getLocalsFloat: extern fn(&mut StackFrame, usize) -> f32,
    pub getLocalsBool: extern fn(&mut StackFrame, usize) -> bool,
    pub getLocalsChar: extern fn(&mut StackFrame, usize) -> u8,
    pub getLocalsRef: extern fn(&mut StackFrame, usize) -> *const ViplObject,

    pub call: extern fn(&mut VirtualMachine, *const c_char),
    pub stringNew: extern fn(&mut VirtualMachine, *const c_char) -> *const ViplObject,
    pub stringGetChar: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> u8,
}

impl Debug for NativeWrapper {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}


impl NativeWrapper {
    pub fn new() -> Self {
        Self {
            pushInt,
            pushFloat,
            pushBool,
            pushChar,
            pushRef,
            popInt,
            popFloat,
            popBool,
            popChar,
            popRef,
            getLocalsInt,
            getLocalsFloat,
            getLocalsBool,
            getLocalsChar,
            getLocalsRef,
            call,
            stringNew,
            stringGetChar,
        }
    }
}