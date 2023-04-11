use std::arch::asm;
use std::ffi::{c_char, c_int, CStr};
use std::fmt::{Debug, Formatter};
use std::mem::{forget};
use std::ptr;
use std::time::Duration;
use libc::{exit, sleep};


use crate::codegen::bytecodeGen;
use crate::heap::Hay;
use crate::lexer::{lexingUnits, SourceProvider};
use crate::namespace::{Namespace, NamespaceState};
use crate::objects::{Str, ViplObject};
use crate::std::std::bootStrapVM;
use crate::value::Value;
use crate::vm::{DataType, MyStr, OpCode, run, SeekableOpcodes, StackFrame, VirtualMachine};
use crate::vm::FuncType::{Builtin, Extern, Runtime};

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
        todo!();
        run(
            &mut SeekableOpcodes {
                index: 0,
                opCodes: &mut ops,
            },
            &mut *vm as &mut VirtualMachine,
            &mut StackFrame {
                // previous: None,
                localVariables: &mut [],
                // name: None,
                objects: None,
                previous: None,
                programCounter: 0,
                namespace: &Namespace{
                    id: 0,
                    name: "".to_string(),
                    state: NamespaceState::Loaded,
                    functionsLookup: Default::default(),
                    globalsLookup: Default::default(),
                    functionsMeta: vec![],
                    functions: vec![],
                    globalsMeta: vec![],
                    globals: vec![],
                },
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
pub extern fn pushFloat(vm: &mut VirtualMachine, v: f64) {
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

        // Rc::increment_strong_count(v);
        // let mut rc = Rice::fromRaw(v);
        // Rice::increment_strong_count(&mut rc);
        vm.stack.push(Value {
            Reference: Hay::new(v),
        });
    }
}

#[no_mangle]
pub extern fn popInt(vm: &mut VirtualMachine) -> isize {
    if DEBUG {
        println!("ffi-popInt");
    }
    vm.pop().getNum()
}

#[no_mangle]
pub extern fn popFloat(vm: &mut VirtualMachine) -> f64 {
    if DEBUG {
        println!("ffi-popFloat");
    }
    vm.pop().getFlo()
}

#[no_mangle]
pub extern fn popChar(vm: &mut VirtualMachine) -> u8 {
    if DEBUG {
        println!("ffi-popChar");
    }
    vm.pop().getChar() as u8
}

#[no_mangle]
pub extern fn popBool(vm: &mut VirtualMachine) -> bool {
    if DEBUG {
        println!("ffi-popBool");
    }
    vm.pop().getBool()
}

#[no_mangle]
pub extern fn popRef(vm: &mut VirtualMachine, _locals: &mut StackFrame) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-popRef");
    }
    let mut rc = vm.pop();

    rc.asMutRef() as *mut ViplObject
}

#[no_mangle]
pub extern fn getLocalsInt(vm: &mut StackFrame, index: usize) -> isize {
    if DEBUG {
        println!("ffi-getLocalsInt");
    }
    unsafe { vm.localVariables.get(index).unwrap() }.getNumRef()
}

#[no_mangle]
pub extern fn getLocalsFloat(vm: &mut StackFrame, index: usize) -> f64 {
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
    let rc = unsafe { locals.localVariables.get_unchecked_mut(index) }.getMutReference();

    rc as *mut ViplObject
}

#[no_mangle]
pub extern fn stringGetChar(_vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> u8 {
    if DEBUG {
        // println!("ffi-stringGetChar");
    }

    *obj.getStr().string.as_bytes().get(index).unwrap()
}

#[no_mangle]
pub extern fn arrGetInt(_vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> isize {
    if DEBUG {
        println!("ffi-arrGetInt");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getNumRef(),
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetFloat(_vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> f64 {
    if DEBUG {
        println!("ffi-arrGetFloat");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getFlo(),
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetBool(_vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> bool {
    if DEBUG {
        println!("ffi-arrGetBool");
    }
    match obj {
        ViplObject::Arr(a) => a.internal.get(index).unwrap().getBool(),
        _ => panic!(),
    }
}

#[no_mangle]
pub extern fn arrGetChar(_vm: &mut VirtualMachine, obj: &mut ViplObject, index: usize) -> u8 {
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
    _vm: &mut VirtualMachine,
    _locals: &mut StackFrame,
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
                .asHay();

            // locals.addObject(rc);

            rc.inner
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
    vm.call(MyStr::Static(name));
}

#[no_mangle]
pub extern fn callFast(vm: &mut VirtualMachine, id: usize) {
    if DEBUG {
        println!("ffi-callFast: {}", id);
    }
    vm.callFast(id);
}

#[no_mangle]
pub extern fn stringNew(vm: *mut VirtualMachine, _locals: *mut StackFrame, s: *const c_char) -> *mut ViplObject {
    if DEBUG {
        println!("ffi-stringNew");
    }
    let st = unsafe { CStr::from_ptr(s) }.to_str().unwrap().to_owned();
    let d = unsafe { &mut *vm };
    let a1 = Str::new(st);
    let aw = a1.into();
    let all = d.heap.allocate(aw);
    let mut a = Value{Reference: all};

    a.asMutRef() as *mut ViplObject
}

#[no_mangle]
pub extern fn strConcat(
    vm: &mut VirtualMachine,
    _locals: &mut StackFrame,
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
    let ptr = Value::makeString(s3, vm);
    // locals.addObject(ptr.asHay());

    ptr.asHay().inner
}


#[no_mangle]
pub extern fn LCall(vm: &mut VirtualMachine, functionID: usize, namespaceID: usize, rsp: *mut Value) -> Value {
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
        objects: None,
        previous: None,
        programCounter: 0,
        namespace,
    };
    func.call(d, fr);
    if meta.returnType != None {
        vm.pop()
    }
    else {
        Value::from(0)
    }
}


#[no_mangle]
pub extern fn asmCall(vm: &mut VirtualMachine, name: *const c_char, rsp: *mut Value) -> usize {
    let name = unsafe { CStr::from_ptr(name) }.to_str().unwrap();
    if DEBUG {
        println!("ffi-call: {}", name);
    }
    let f = vm.functions.get(&MyStr::Static(name)).unwrap();
    let argsCount = f.argAmount;
    let doesReturn = f.returnType != None;
    let mut locals = vec![Value{Num: 0}; f.varTable.len()];

    for i in 0..(f.argAmount) {
        let v = unsafe {
            ptr::read(rsp.add(i))
        };
        locals[(f.argAmount - 1) - i] = v;
    }

    todo!();
    let mut stack = StackFrame {
        localVariables: &mut locals,
        // name: None,
        objects: None,
        previous: None,
        programCounter: 0,
        namespace: &Namespace {
            id: 0,
            name: "".to_string(),
            state: NamespaceState::Loaded,
            functionsLookup: Default::default(),
            globalsLookup: Default::default(),
            functionsMeta: vec![],
            functions: vec![],
            globalsMeta: vec![],
            globals: vec![],
        },
    };

    let t = f.typ;

    // FIXME this is so much cursed
    // FIXME i am bypassing all rust safety guaranties :)

    let ptr = vm as *const VirtualMachine as *mut VirtualMachine;

    match t {
        Runtime {
            rangeStart: s,
            rangeStop: _e,
        } => unsafe {
            let mut seekable = SeekableOpcodes {
                index: s,
                opCodes: &mut (*ptr).opCodes,
            };
            run(&mut seekable, &mut *ptr, &mut stack);
        },
        Builtin { callback } => callback(vm, &mut stack),
        Extern { callback } => {
            stack.objects = Some(vec![]);
            callback(vm, &mut stack);
        },
    };

    if doesReturn {
        let v = (*vm).pop();
        unsafe { *rsp.add(argsCount - 1) = v; }
        argsCount*8-8
    }
    else {
        argsCount*8
    }
}

#[repr(C)]
pub struct NativeWrapper {
    pub pushInt: extern fn(&mut VirtualMachine, isize) -> (),
    pub pushFloat: extern fn(&mut VirtualMachine, f64) -> (),
    pub pushBool: extern fn(&mut VirtualMachine, bool) -> (),
    pub pushChar: extern fn(&mut VirtualMachine, u8) -> (),
    pub pushRef: extern fn(&mut VirtualMachine, *mut ViplObject) -> (),

    pub popInt: extern fn(&mut VirtualMachine) -> isize,
    pub popFloat: extern fn(&mut VirtualMachine) -> f64,
    pub popBool: extern fn(&mut VirtualMachine) -> bool,
    pub popChar: extern fn(&mut VirtualMachine) -> u8,
    pub popRef: extern fn(&mut VirtualMachine, &mut StackFrame) -> *mut ViplObject,

    pub getLocalsInt: extern fn(&mut StackFrame, usize) -> isize,
    pub getLocalsFloat: extern fn(&mut StackFrame, usize) -> f64,
    pub getLocalsBool: extern fn(&mut StackFrame, usize) -> bool,
    pub getLocalsChar: extern fn(&mut StackFrame, usize) -> u8,
    pub getLocalsRef: extern fn(&mut StackFrame, usize) -> *mut ViplObject,

    pub arrGetInt: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> isize,
    pub arrGetFloat: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> f64,
    pub arrGetBool: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> bool,
    pub arrGetChar: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> u8,
    pub arrGetRef: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject, usize) -> *mut ViplObject,

    pub call: extern fn(&mut VirtualMachine, *const c_char),
    pub callFast: extern fn(&mut VirtualMachine, usize),
    pub callAsm: extern fn(&mut VirtualMachine, *const c_char, *mut Value) -> usize,
    pub stringNew: extern fn(*mut VirtualMachine, *mut StackFrame, *const c_char) -> *mut ViplObject,
    pub stringGetChar: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> u8,
    pub strConcat: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject, &mut ViplObject) -> *mut ViplObject,
    pub LCall: extern fn(&mut VirtualMachine, usize, usize, *mut Value) -> Value
}

impl Debug for NativeWrapper {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}


// TODO
#[repr(C)]
pub struct NativeWrapper2 {
    pub pushValue: extern fn(&mut VirtualMachine, isize) -> (),

    pub popValue: extern fn(&mut VirtualMachine) -> isize,

    pub getLocalsValue: extern fn(&mut StackFrame, usize) -> isize,

    pub arrGetValue: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> isize,

    pub arrSetValue: extern fn(&mut VirtualMachine, &mut ViplObject, usize, isize),

    pub call: extern fn(&mut VirtualMachine, *const c_char),
    pub callFast: extern fn(&mut VirtualMachine, usize),
    pub callAsm: extern fn(&mut VirtualMachine, *const c_char, *mut Value) -> usize,
    pub stringNew: extern fn(*mut VirtualMachine, *mut StackFrame, *const c_char) -> *mut ViplObject,
    pub stringGetChar: extern fn(&mut VirtualMachine, &mut ViplObject, usize) -> u8,
    pub strConcat: extern fn(&mut VirtualMachine, &mut StackFrame, &mut ViplObject, &mut ViplObject) -> *mut ViplObject,
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
            callFast,
            callAsm: asmCall,
            stringNew,
            stringGetChar,
            strConcat,
            LCall,
        }
    }
}
