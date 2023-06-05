use libc::exit;
use std::arch::asm;
use std::ffi::{c_char, c_int, CStr};
use std::fmt::{Debug, Formatter};
use std::mem::forget;
use std::ptr;
use std::time::Duration;
use crate::std::std::bootStrapVM;

use crate::vm::heap::Hay;
use crate::vm::namespace::{callNative, LoadedFunction, Namespace};
use crate::vm::nativeObjects::{UntypedObject, ViplObject};
use crate::vm::objects::{Array, Str};
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

const DEBUG: bool = true;

#[no_mangle]
pub extern "C" fn createVm() -> *mut VirtualMachine {
    let vm = Box::new(bootStrapVM());
    let ptr = Box::into_raw(vm);
    forget(ptr);
    ptr
}

#[no_mangle]
pub extern "C" fn pushStack(vm: &mut VirtualMachine, value: &mut Value) {
    vm.push(*value);
}

#[no_mangle]
pub extern "C" fn popStack(vm: &mut VirtualMachine) -> Value {
    vm.pop()
}

#[no_mangle]
pub extern "C" fn test(vm: *mut VirtualMachine) {
    println!("hello from vipl :3");
    unsafe { println!("{:?}", &*vm) }
}

#[no_mangle]
pub extern "C" fn dropVm(ptr: *mut VirtualMachine) {
    unsafe {
        ptr::drop_in_place(ptr);
    }
}

#[no_mangle]
pub extern "C" fn createNamespace(vm: &mut VirtualMachine, name: *const c_char) -> usize {
    let name = unsafe { CStr::from_ptr(name) }.to_str().unwrap();

    let namespace = Namespace::new(name, vm);

    vm.registerNamespace(namespace)
}

#[no_mangle]
pub extern "C" fn pushValue(vm: &mut VirtualMachine, v: isize) {
    if DEBUG {
        println!("[ffi] pushValue {}", v);
    }
    vm.push(Value::from(v))
}

#[no_mangle]
pub extern "C" fn popValue(vm: &mut VirtualMachine) -> isize {
    if DEBUG {
        println!("[ffi] popValue");
    }
    vm.pop().getNum()
}

#[no_mangle]
pub extern "C" fn getLocalsValue(vm: &mut StackFrame, index: usize) -> isize {
    if DEBUG {
        println!("[ffi] getLocalsInt");
    }
    unsafe { vm.localVariables.add(index).read().getNumRef() }
}

#[no_mangle]
pub extern "C" fn arrSetValue(
    _vm: &mut VirtualMachine,
    obj: &mut ViplObject<Array>,
    index: usize,
    value: isize,
) {
    if DEBUG {
        println!("[ffi] arrSetValue");
    }

    obj.data.insert(value.into(), index)
}

#[no_mangle]
pub extern "C" fn stringGetChar(
    _vm: &mut VirtualMachine,
    obj: &mut ViplObject<Str>,
    index: usize,
) -> u8 {
    if DEBUG {
        println!("[ffi] stringGetChar");
    }

    *obj.data.string.as_bytes().get(index).unwrap()
}

#[no_mangle]
pub extern "C" fn arrGetValue(
    _vm: &mut VirtualMachine,
    obj: &mut ViplObject<Array>,
    index: usize,
) -> isize {
    if DEBUG {
        println!("[ffi] arrGetValue");
    }
    println!("arr {:?} {}", obj as *mut ViplObject<Array>, index);
    obj.data.internal.get(index).unwrap().getNumRef()
}

#[no_mangle]
pub extern "C" fn stringNew(
    vm: &mut VirtualMachine,
    _locals: &mut StackFrame,
    s: *const c_char,
) -> *mut ViplObject<Str> {
    if DEBUG {
        println!("[ffi] stringNew");
    }
    let st = unsafe { CStr::from_ptr(s) }.to_str().unwrap();

    let all = vm.allocateString(st);

    let mut a = Value::from(all);

    a.asMutRef() as *mut ViplObject<Str>
}

#[no_mangle]
pub extern "C" fn stringCached(
    vm: &mut VirtualMachine,
    _locals: &mut StackFrame,
    id: usize,
) -> *mut ViplObject<Str> {
    if DEBUG {
        println!("[ffi] stringCached");
    }

    let mut s = vm.getLocalString(id);

    s.asMutRef::<Str>()
}

#[no_mangle]
pub extern "C" fn arrayNew(
    vm: &mut VirtualMachine,
    _locals: &mut StackFrame,
    size: usize
) -> *mut ViplObject<Array> {
    if DEBUG {
        println!("[ffi] arrayNew");
    }

    let all = vm.allocateArray(Vec::with_capacity(size));

    let mut a = Value::from(all);

    a.asMutRef() as *mut ViplObject<Array>
}

#[no_mangle]
pub extern "C" fn strConcat(
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
pub extern "C" fn lCall(
    vm: &mut VirtualMachine,
    functionID: usize,
    namespaceID: usize,
    rsp: *mut Value,
) -> Value {
    let mut naughty = vm.getNaughty();

    if DEBUG {
        println!(
            "[ffi] LCall VM: {:?}, N: {}, F: {}, RSP: {:?}",
            vm as *mut VirtualMachine, namespaceID, functionID, rsp
        )
    }
    let namespace = vm.getNamespace(namespaceID);
    let f = namespace.getFunction(functionID);

    let returns = f.0.returns();

    if DEBUG {
        println!("[ffi] before call RSP: {:?}", rsp);
    }

    let callable = f.1.as_ref().unwrap();

    let frame = StackFrame {
        localVariables: rsp,
        programCounter: 0,
        namespaceId: namespaceID,
        functionId: functionID,
    };

    let ret = callable.call(naughty.getMut(), frame, returns);

    if DEBUG {
        println!(
            "[ffi] after call {} {:?}",
            vm.frameCount(),
            vm.getFrame().localVariables
        );
    }

    ret
}

#[no_mangle]
pub extern "C" fn printDigit(n: isize) {
    println!("[debug] dec: \"{}\"", n);
    println!("[debug] hex: \"{:#01x}\"", n);
}

#[no_mangle]
pub extern "C" fn allocateObject(vm: &mut VirtualMachine, locals: &mut StackFrame, nId: usize, sId: usize) -> *mut UntypedObject {
    if DEBUG {
        println!("[ffi] allocating {}:{}", nId, sId)
    }

    vm.allocateObject(nId, sId)
}

#[no_mangle]
pub extern "C" fn arrayLen(obj: &mut ViplObject<Array>) -> Value {
    if DEBUG {
        println!("[ffi] arrayLen {:?}", obj as *mut ViplObject<Array>)
    }

    println!("arr {:?}", obj.data);

    obj.data.internal.len().into()
}

#[no_mangle]
pub extern "C" fn strLen(obj: &mut ViplObject<Str>) -> Value {
    if DEBUG {
        println!("[ffi] strLen {:?}", obj as *mut ViplObject<Str>)
    }

    println!("str {:?}", obj.data);

    obj.data.string.len().into()
}

pub extern "C" fn dynamicCall(vm: &mut VirtualMachine, f: Value, rsp: usize) -> Value {
    let (nId, fId) = f.asFunction();

    let (fMeta, _) = vm.getNamespace(nId as usize).getFunction(fId as usize);



    todo!()
}

#[repr(C)]
pub struct NativeWrapper {
    pub pushValue: extern "C" fn(&mut VirtualMachine, isize) -> (),

    pub popValue: extern "C" fn(&mut VirtualMachine) -> isize,

    pub getLocalsValue: extern "C" fn(&mut StackFrame, usize) -> isize,

    pub arrGetValue: extern "C" fn(&mut VirtualMachine, &mut ViplObject<Array>, usize) -> isize,

    pub arrSetValue: extern "C" fn(&mut VirtualMachine, &mut ViplObject<Array>, usize, isize),

    pub stringNew:
        extern "C" fn(&mut VirtualMachine, &mut StackFrame, *const c_char) -> *mut ViplObject<Str>,
    pub arrayNew:
    extern "C" fn (&mut VirtualMachine, &mut StackFrame, usize) -> *mut ViplObject<Array>,
    pub stringCached:
    extern "C" fn(&mut VirtualMachine, &mut StackFrame, usize) -> *mut ViplObject<Str>,
    pub allocateObject:
    extern "C" fn(&mut VirtualMachine,  &mut StackFrame, usize, usize) -> *mut UntypedObject,

    pub stringGetChar: extern "C" fn(&mut VirtualMachine, &mut ViplObject<Str>, usize) -> u8,
    pub strConcat: extern "C" fn(
        &mut VirtualMachine,
        &mut StackFrame,
        &mut ViplObject<Str>,
        &mut ViplObject<Str>,
    ) -> *mut ViplObject<Str>,
    pub lCall: extern "C" fn(&mut VirtualMachine, usize, usize, *mut Value) -> Value,
    pub dynamicCall: extern "C" fn(&mut VirtualMachine, Value, usize) -> Value,
    pub printDigit: extern "C" fn(isize),
    pub arrayLen: extern "C" fn (&mut ViplObject<Array>) -> Value,
    pub strLen: extern "C" fn (&mut ViplObject<Str>) -> Value
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
            arrayNew,
            stringCached,
            allocateObject,
            stringGetChar,
            strConcat,
            lCall,
            dynamicCall,
            printDigit,
            arrayLen,
            strLen,
        }
    }
}

impl Debug for NativeWrapper {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
