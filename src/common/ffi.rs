use std::ffi::{c_char, CStr};
use std::fmt::{Debug, Formatter};
use std::mem::forget;
use std::ptr;
use std::rc::Rc;
use std::thread::{sleep, Thread};

use crate::codegen::bytecodeGen;
use crate::lexer::{lexingUnits, SourceProvider};
use crate::objects::{Str, ViplObject};
use crate::std::bootStrapVM;
use crate::vm::{DataType, MyStr, OpCode, run, SeekableOpcodes, StackFrame, Value, VirtualMachine};

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
    unsafe {
        name.copy_to(buf.as_mut_ptr(), nameLen);
    }
    let nameCopy = unsafe { String::from_utf8_unchecked(buf) };

    let mut argsCopy = Vec::with_capacity(argsLen);
    unsafe {
        args.copy_to(argsCopy.as_mut_ptr(), argsLen);
    }

    vm.makeExtern(
        nameCopy,
        argsCopy.into_iter().map(|it| it.into()).collect(),
        callback,
        None,
    );
}

#[no_mangle]
pub extern fn popStack(vm: &mut VirtualMachine) -> Option<Value> {
    vm.stack.pop()
}

#[no_mangle]
pub extern fn evaluate(vm: &mut VirtualMachine, d: *const u8, len: usize) {
    let mut buf = vec![0u8; len];
    unsafe {
        d.copy_to(buf.as_mut_ptr(), len);
    }
    let mut s = unsafe { String::from_utf8_unchecked(buf) };

    unsafe {
        d.copy_to(s.as_mut_ptr(), len);
    }



    let res = match unsafe {
        crate::lexer::tokenize(&mut lexingUnits(), SourceProvider { data: &s, index: 0 })
    } {
        Ok(v) => v,
        Err(_) => {
            println!("lexer failed");
            return;
        }
    };

    let ast = match crate::parser::parseTokens(res) {
        Ok(v) => v,
        Err(_) => {
            println!("parser failed");
            return;
        }
    };

    // println!("{:?}", &ast);

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

    run(
        &mut SeekableOpcodes {
            index: 0,
            opCodes: &mut opCodes.0,
        },
        vm,
        &mut StackFrame::new(
            &mut opCodes
                .1
                .into_iter()
                .map(|it| it.into())
                .collect::<Vec<Value>>(),
        ),
    );

    println!("finished");
}

#[no_mangle]
pub extern fn test(vm: *mut VirtualMachine) {
    println!("i am here");
    let mut ops = vec![
        OpCode::PushInt(69),
        OpCode::Call {
            encoded: MyStr::Static("print(int)"),
        },
    ];

    unsafe {
        for _ in 0..ops.len() {
            (*vm).opCodeCache.push(None)
        }
    }

    unsafe {
        run(
            &mut SeekableOpcodes {
                index: 0,
                opCodes: &mut ops,
            },
            &mut *vm as &mut VirtualMachine,
            &mut StackFrame {
                // previous: None,
                localVariables: &mut [],
                name: None,
                objects: None,
            },
        )
    }
}

#[no_mangle]
pub extern fn dropVm(ptr: *mut VirtualMachine) {
    unsafe {
        ptr::drop_in_place(ptr);
    }
}

#[no_mangle]
pub extern fn pushInt(vm: &mut VirtualMachine, v: isize) {
    if DEBUG {
        println!("ffi-pushInt {}", v);
    }
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn pushFloat(vm: &mut VirtualMachine, v: f32) {
    if DEBUG {
        println!("ffi-pushFloat {}", v);
    }
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn pushChar(vm: &mut VirtualMachine, v: u8) {
    if DEBUG {
        println!("ffi-pushChar {}", v);
    }
    vm.stack.push(Value::from(v as char))
}

#[no_mangle]
pub extern fn pushBool(vm: &mut VirtualMachine, v: bool) {
    if DEBUG {
        println!("ffi-pushBool {}", v);
    }
    vm.stack.push(Value::from(v))
}

#[no_mangle]
pub extern fn pushRef(vm: &mut VirtualMachine, v: *mut ViplObject) {
    if DEBUG {
        unsafe { println!("ffi-pushRef {:?}", &*v); }
    }
    unsafe {
        // incrementing rc so that it doesnt get freed while being used by native function
        Rc::increment_strong_count(v);
        let rc = Rc::from_raw(v);
        vm.stack.push(Value::Reference {
            instance: Some(rc),
        });
    }
}

#[no_mangle]
pub extern fn popInt(vm: &mut VirtualMachine) -> isize {
    if DEBUG {
        println!("ffi-popInt");
    }
    vm.stack.pop().unwrap().getNum()
}

#[no_mangle]
pub extern fn popFloat(vm: &mut VirtualMachine) -> f32 {
    if DEBUG {
        println!("ffi-popFloat");
    }
    vm.stack.pop().unwrap().getFlo()
}

#[no_mangle]
pub extern fn popChar(vm: &mut VirtualMachine) -> u8 {
    if DEBUG {
        println!("ffi-popChar");
    }
    vm.stack.pop().unwrap().getChar() as u8
}

#[no_mangle]
pub extern fn popBool(vm: &mut VirtualMachine) -> bool {
    if DEBUG {
        println!("ffi-popBool");
    }
    vm.stack.pop().unwrap().getBool()
}

#[no_mangle]
pub extern fn popRef(vm: &mut VirtualMachine, locals: &mut StackFrame) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-popRef");
    }
    let rc = vm.stack.pop().unwrap().getReference().clone().unwrap();

    locals.addObject(rc.clone());

    let ptr = Rc::into_raw(rc);
    ptr as *mut ViplObject
}

#[no_mangle]
pub extern fn getLocalsInt(vm: &mut StackFrame, index: usize) -> isize {
    if DEBUG {
        println!("ffi-getLocalsInt");
    }
    unsafe { vm.localVariables.get(index).unwrap() }.getNum()
}

#[no_mangle]
pub extern fn getLocalsFloat(vm: &mut StackFrame, index: usize) -> f32 {
    if DEBUG {
        println!("ffi-getLocalsFloat");
    }
    unsafe { vm.localVariables.get(index).unwrap() }.getFlo()
}

#[no_mangle]
pub extern fn getLocalsChar(vm: &mut StackFrame, index: usize) -> u8 {
    if DEBUG {
        println!("ffi-getLocalsChar");
    }
    unsafe { vm.localVariables.get(index).unwrap() }.getChar() as u8
}

#[no_mangle]
pub extern fn getLocalsBool(vm: &mut StackFrame, index: usize) -> bool {
    if DEBUG {
        println!("ffi-getLocalsBool");
    }
    unsafe { vm.localVariables.get(index).unwrap() }.getBool()
}

#[no_mangle]
pub extern fn getLocalsRef(locals: &mut StackFrame, index: usize) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-getLocalsRef");
    }
    let rc = locals.localVariables.get(index).unwrap()
        .getReference()
        .clone()
        .unwrap();
    locals.addObject(rc.clone());
    Rc::into_raw(rc) as *mut ViplObject
}

#[no_mangle]
pub extern fn stringGetChar(vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> u8 {
    if DEBUG {
        // println!("ffi-stringGetChar");
    }

    *obj.getStr().string.as_bytes().get(index).unwrap() as u8
}

#[no_mangle]
pub extern fn arrGetInt(vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> isize {
    if DEBUG {
        println!("ffi-arrGetInt");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getNum(),
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetFloat(vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> f32 {
    if DEBUG {
        println!("ffi-arrGetFloat");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getFlo(),
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetBool(vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> bool {
    if DEBUG {
        println!("ffi-arrGetBool");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getBool(),
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetChar(vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> u8 {
    if DEBUG {
        println!("ffi-arrGetChar");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getChar() as u8,
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetRef(
    vm: &mut VirtualMachine,
    locals: &mut StackFrame,
    obj: &mut ViplObject,
    index: usize,
) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-arrGetRef");
    }
    match obj {
        ViplObject::Arr(a) => {
            let rc = a
                .internal
                .get(index)
                .unwrap()
                .getReference()
                .clone()
                .unwrap();

            locals.addObject(rc.clone());

            Rc::into_raw(rc) as *mut ViplObject
        }
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn call(vm: &mut VirtualMachine, s: *const c_char) {
    let name = unsafe { CStr::from_ptr(s) }.to_str().unwrap();
    if DEBUG {
        println!("ffi-call: {}", name);
    }
    vm.call(MyStr::Runtime(name.to_owned().into_boxed_str()));
}

#[no_mangle]
pub extern fn stringNew(vm: &mut VirtualMachine, locals: &mut StackFrame, s: *const c_char) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-stringNew");
    }
    let st = unsafe { CStr::from_ptr(s) }.to_str().unwrap().to_owned();
    let rc = Rc::new(ViplObject::Str(Str { string: st }));

    locals.addObject(rc.clone());

    Rc::into_raw(rc) as *mut ViplObject
}

#[no_mangle]
pub extern fn strConcat(
    vm: &mut VirtualMachine,
    locals: &mut StackFrame,
    s1: &mut ViplObject,
    s2: &mut ViplObject,
) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-strConcat");
    }
    let mut s3 = String::new();
    s3.push_str(&s1.getStr().string);
    s3.push_str(&s2.getStr().string);

    // FIXME not sure if this is needed
    let rc = Rc::new(ViplObject::Str(Str { string: s3 }));
    locals.addObject(rc.clone());

    Rc::into_raw(rc) as *mut ViplObject
}

#[repr(C)]
pub struct NativeWrapper {
    pub pushInt: extern fn(&mut VirtualMachine, isize) -> (),
    pub pushFloat: extern fn(&mut VirtualMachine, f32) -> (),
    pub pushBool: extern fn(&mut VirtualMachine, bool) -> (),
    pub pushChar: extern fn(&mut VirtualMachine, u8) -> (),
    pub pushRef: extern fn(&mut VirtualMachine, *mut ViplObject) -> (),

    pub popInt: extern fn(&mut VirtualMachine) -> isize,
    pub popFloat: extern fn(&mut VirtualMachine) -> f32,
    pub popBool: extern fn(&mut VirtualMachine) -> bool,
    pub popChar: extern fn(&mut VirtualMachine) -> u8,
    pub popRef: extern fn(&mut VirtualMachine, &mut StackFrame) -> *mut ViplObject,

    pub getLocalsInt: extern fn(&mut StackFrame, usize) -> isize,
    pub getLocalsFloat: extern fn(&mut StackFrame, usize) -> f32,
    pub getLocalsBool: extern fn(&mut StackFrame, usize) -> bool,
    pub getLocalsChar: extern fn(&mut StackFrame, usize) -> u8,
    pub getLocalsRef: extern fn(&mut StackFrame, usize) -> *mut ViplObject,

    pub arrGetInt: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> isize,
    pub arrGetFloat: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> f32,
    pub arrGetBool: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> bool,
    pub arrGetChar: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> u8,
    pub arrGetRef: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject, usize) -> *mut ViplObject,

    pub call: extern fn(&mut VirtualMachine, *const c_char),
    pub stringNew: extern fn(&mut VirtualMachine, &mut StackFrame, *const c_char) -> *mut ViplObject,
    pub stringGetChar: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> u8,
    pub strConcat: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject, &mut ViplObject) -> *mut ViplObject,
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
            arrGetInt,
            arrGetFloat,
            arrGetBool,
            arrGetChar,
            arrGetRef,
            call,
            stringNew,
            stringGetChar,
            strConcat,
        }
    }
}
