use std::arch::asm;
use std::ffi::{c_char, c_int, CStr};
use std::fmt::{Debug, Formatter};
use std::mem::forget;
use std::ptr;
use std::time::Duration;

use libc::{exit, sleep};

use crate::lexer::{lexingUnits, SourceProvider};
use crate::std::std::bootStrapVM;
use crate::vm::heap::Hay;
use crate::vm::myStr::MyStr;
use crate::vm::namespace::Namespace;
use crate::vm::nativeObjects::ViplObject;
use crate::vm::objects::{Array, Str};
use crate::vm::stackFrame::StackFrame;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

const DEBUG: bool = true;


#[no_mangle]
pub extern fn createVm() -> *mut VirtualMachine<'static> {
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
pub extern fn pushInt(vm: &mut VirtualMachine, v: isize) {
    if DEBUG {
        println!("[ffi] pushInt {}", v);
    }
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn popInt(vm: &mut VirtualMachine) -> isize {
    if DEBUG {
        println!("[ffi] popInt");
    }
    vm.pop().getNum()
}

#[no_mangle]
pub extern fn getLocalsInt(vm: &mut StackFrame, index: usize) -> isize {
    if DEBUG {
        println!("[ffi] getLocalsInt");
    }
    unsafe { vm.localVariables.get(index).unwrap() }.getNumRef()
}

#[no_mangle]
pub extern fn arrSetInt(_vm: &mut VirtualMachine, obj: &mut ViplObject<Array>, index: usize, value: isize) {
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
pub extern fn arrGetInt(_vm: &mut VirtualMachine, obj: &mut ViplObject<Array>, index: usize) -> isize {
    if DEBUG {
        println!("[ffi] arrGetInt");
    }
    obj.data.internal.get(index).unwrap().getNumRef()
}

#[no_mangle]
pub extern fn call(vm: &mut VirtualMachine, s: *const c_char) {
    let name = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
    if DEBUG {
        println!("[ffi] call: {}", name);
    }
    panic!();
    // vm.call(MyStr::Static(name));
}

#[no_mangle]
pub extern fn stringNew(vm: *mut VirtualMachine, _locals: *mut StackFrame, s: *const c_char) -> *mut ViplObject<Str> {
    if DEBUG {
        println!("[ffi] stringNew");
    }
    let st = unsafe { CStr::from_ptr(s) }.to_str().unwrap().to_owned();
    let d = unsafe { &mut *vm };
    let a1 = Str::new(st);
    let aw = ViplObject::<Str>::str(a1);
    let all = d.heap.allocate(aw);
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
pub extern fn LCall(vm: &mut VirtualMachine, functionID: usize, namespaceID: usize, rsp: *mut Value) -> Value {
    if DEBUG {
        println!("[ffi] LCall")
    }

    let d =unsafe { &mut *(vm as *mut VirtualMachine) };
    let namespace = vm.namespaces.get(namespaceID).unwrap();
    let meta = namespace.functionsMeta.get(functionID).unwrap();
    let func = namespace.functions.get(functionID).unwrap();
    let mut buf = vec![];

    for x in 0..meta.argsCount {
        let v = unsafe { rsp.add(x).read() };
        buf.push(v);
    }

    for _ in 0..meta.localsMeta.len()-meta.argsCount {
        buf.push(Value::from(0))
    }

    let fr = StackFrame {
        localVariables: &mut buf,
        previous: None,
        programCounter: 0,
        namespace,
        functionID,
    };
    func.as_ref().unwrap().call(d, fr);
    if meta.returnType != None {
        vm.pop()
    }
    else {
        Value::from(0)
    }
}


// TODO
#[repr(C)]
pub struct NativeWrapper {
    pub pushValue: extern fn(&mut VirtualMachine, isize) -> (),

    pub popValue: extern fn(&mut VirtualMachine) -> isize,

    pub getLocalsValue: extern fn(&mut StackFrame, usize) -> isize,

    pub arrGetValue: extern fn(&mut VirtualMachine, &mut ViplObject<Array>, usize) -> isize,

    pub arrSetValue: extern fn(&mut VirtualMachine, &mut ViplObject<Array>, usize, isize),

    pub call: extern fn(&mut VirtualMachine, *const c_char),
    pub stringNew: extern fn(*mut VirtualMachine, *mut StackFrame, *const c_char) -> *mut ViplObject<Str>,
    pub stringGetChar: extern fn(&mut VirtualMachine, &mut ViplObject<Str>, usize) -> u8,
    pub strConcat: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject<Str>, &mut ViplObject<Str>) -> *mut ViplObject<Str>,
    pub LCall: extern fn(&mut VirtualMachine, usize, usize, *mut Value) -> Value
}

impl NativeWrapper {
    pub fn new() -> Self {
        Self {
            pushValue: pushInt,
            popValue: popInt,
            getLocalsValue: getLocalsInt,
            arrGetValue: arrGetInt,
            arrSetValue: arrSetInt,
            call,
            stringNew,
            stringGetChar,
            strConcat,
            LCall,
        }
    }
}

impl Debug for NativeWrapper {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}