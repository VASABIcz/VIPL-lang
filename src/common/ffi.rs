use std::arch::asm;
use std::ffi::{c_char, c_int, CStr};
use std::fmt::{Debug, Formatter};
use std::mem::forget;
use std::ptr;
use std::time::Duration;
use libc::exit;

use crate::lexer::{lexingUnits, SourceProvider};
use crate::std::std::bootStrapVM;
use crate::vm::heap::Hay;
use crate::vm::myStr::MyStr;
use crate::vm::namespace::{callNative, LoadedFunction, Namespace};
use crate::vm::nativeObjects::ViplObject;
use crate::vm::objects::{Array, Str};
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

const DEBUG: bool = false;


#[no_mangle]
pub extern fn createVm() -> *mut VirtualMachine {
    let vm = Box::new(bootStrapVM());
    let ptr = Box::into_raw(vm);
    forget(ptr);
    ptr
}

#[no_mangle]
pub extern fn pushStack(vm: &mut VirtualMachine, value: &mut Value) {
    vm.stack.push(*value);
}

#[no_mangle]
pub extern fn popStack(vm: &mut VirtualMachine) -> Option<Value> {
    vm.stack.pop()
}

#[no_mangle]
pub extern fn test(vm: *mut VirtualMachine) {
    println!("hello from vipl :3");
    unsafe { println!("{:?}", &*vm) }
}

#[no_mangle]
pub extern fn dropVm(ptr: *mut VirtualMachine) {
    unsafe {
        ptr::drop_in_place(ptr);
    }
}

#[no_mangle]
pub extern fn createNamespace(vm: &mut VirtualMachine, name: *const c_char) -> usize {
    let name = unsafe { CStr::from_ptr(name) }.to_str().unwrap();

    let namespace = Namespace::new(name);

    vm.registerNamespace(namespace)
}

#[no_mangle]
pub extern fn pushValue(vm: &mut VirtualMachine, v: isize) {
    if DEBUG {
        println!("[ffi] pushInt {}", v);
    }
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn popValue(vm: &mut VirtualMachine) -> isize {
    if DEBUG {
        println!("[ffi] popInt");
    }
    vm.pop().getNum()
}

#[no_mangle]
pub extern fn getLocalsValue(vm: &mut StackFrame, index: usize) -> isize {
    if DEBUG {
        println!("[ffi] getLocalsInt");
    }
    unsafe { vm.localVariables.add(index).read().getNumRef() }
}

#[no_mangle]
pub extern fn arrSetValue(_vm: &mut VirtualMachine, obj: &mut ViplObject<Array>, index: usize, value: isize) {
    if DEBUG {
        println!("[ffi] arrSetInt");
    }
    *(obj.data.internal.get_mut(index).unwrap().getRefNum()) = value.into()
}

#[no_mangle]
pub extern fn stringGetChar(_vm: &mut VirtualMachine, obj: &mut ViplObject<Str>, index: usize) -> u8 {
    if DEBUG {
        println!("[ffi] stringGetChar");
    }

    *obj.data.string.as_bytes().get(index).unwrap()
}

#[no_mangle]
pub extern fn arrGetValue(_vm: &mut VirtualMachine, obj: &mut ViplObject<Array>, index: usize) -> isize {
    if DEBUG {
        println!("[ffi] arrGetInt");
    }
    obj.data.internal.get(index).unwrap().getNumRef()
}

#[no_mangle]
pub extern fn stringNew(vm: *mut VirtualMachine, _locals: *mut StackFrame, s: *const c_char) -> *mut ViplObject<Str> {
    if DEBUG {
        println!("[ffi] stringNew");
    }
    let st = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
    let d = unsafe { &mut *vm };

    let all = d.allocateString(st);

    let mut a = Value::from(all);

    a.asMutRef() as *mut ViplObject<Str>
}

#[no_mangle]
pub extern fn strConcat(
    vm: &mut VirtualMachine,
    _locals: &mut StackFrame,
    s1: &mut ViplObject<Str>,
    s2: &mut ViplObject<Str>,
) -> *mut ViplObject<Str> {
    if DEBUG {
        println!("[ffi] strConcat");
    }
    let mut s3 = String::new();
    s3.push_str(&s1.data.string);
    s3.push_str(&s2.data.string);

    // FIXME not sure if this is needed
    let ptr = Value::makeString(s3, vm);
    // locals.addObject(ptr.asHay());

    ptr.asHay().inner
}


#[no_mangle]
pub extern fn lCall(vm: &mut VirtualMachine, functionID: usize, namespaceID: usize, rsp: *mut Value) -> Value {
    if DEBUG {
        println!("[ffi] LCall {:?} {} {} {:?}", vm as *mut VirtualMachine, namespaceID, functionID, rsp)
    }

    let d =unsafe { &mut *  (vm as *mut VirtualMachine) };
    let namespace = vm.namespaces.get(namespaceID).unwrap();//
    let f = namespace.getFunction(functionID);

    let returns = f.0.returns();

    if DEBUG {
        println!("[ffi] before call {:?}", rsp);
    }

    let callable = f.1.as_ref().unwrap();

    let frame = StackFrame{
        localVariables: rsp,
        programCounter: 0,
        namespaceId: namespaceID,
        functionId: functionID,
    };

    // println!("vm: {:?} {}", vm.rawPtr(), vm.rawPtr() as usize);

    d.pushFrame(frame);

    let vm2 = unsafe { &mut *vm.rawPtr() };

    let a = vm.getMutFrame();

    match callable {
        LoadedFunction::BuiltIn(b) => {
            b(vm2, a);
        }
        LoadedFunction::Native(n) => {
            // FIXME in release problem with return value
            let v = callNative(n, vm2, a);

            if returns {
                d.push(v.into())
            }
        }
        LoadedFunction::Virtual(v) => {
            d.execute(v);
        }
    }
    d.popFrame();

    if DEBUG {
        println!("[ffi] after call {} {:?}", vm.frames.len(), vm.getFrame().localVariables);
    }

    if returns {
        let v = vm.pop();
        if DEBUG {
            println!("[ffi] function {} returned {:?} {:?}", f.0.name, v, v.asUnsigned() as *const ());
        }

        v
    }
    else {
        Value::from(0)
    }
}

#[no_mangle]
pub extern fn printDigit(n: isize) {
    println!("[debug] dec: \"{}\"", n);
    println!("[debug] hex: \"{:#01x}\"", n);
}


// TODO
#[repr(C)]
pub struct NativeWrapper {
    pub pushValue: extern fn(&mut VirtualMachine, isize) -> (),

    pub popValue: extern fn(&mut VirtualMachine) -> isize,

    pub getLocalsValue: extern fn(&mut StackFrame, usize) -> isize,

    pub arrGetValue: extern fn(&mut VirtualMachine, &mut ViplObject<Array>, usize) -> isize,

    pub arrSetValue: extern fn(&mut VirtualMachine, &mut ViplObject<Array>, usize, isize),

    pub stringNew: extern fn(*mut VirtualMachine, *mut StackFrame, *const c_char) -> *mut ViplObject<Str>,
    pub stringGetChar: extern fn(&mut VirtualMachine, &mut ViplObject<Str>, usize) -> u8,
    pub strConcat: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject<Str>, &mut ViplObject<Str>) -> *mut ViplObject<Str>,
    pub lCall: extern fn(&mut VirtualMachine, usize, usize, *mut Value) -> Value,
    pub printDigit: extern fn(isize)
}

fn getNativeWrapperOffset(n: &NativeWrapper, fieldPtr: usize) -> usize {
    let ptr = n as *const NativeWrapper as usize;

    fieldPtr-ptr
}

impl NativeWrapper {
    pub fn new() -> Self {
        Self {
            pushValue,
            popValue,
            getLocalsValue,
            arrGetValue,
            arrSetValue,
            stringNew,
            stringGetChar,
            strConcat,
            lCall,
            printDigit
        }
    }
}

impl Debug for NativeWrapper {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}