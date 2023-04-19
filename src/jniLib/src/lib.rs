use std::collections::HashMap;
use std::mem::transmute;

use jni::{InitArgs, InitArgsBuilder, JavaVM, JNIEnv, JNIVersion};
use jni::objects::{GlobalRef, JClass, JLongArray, JObject, JObjectArray, JString, JValue, JValueGen};
use jni::strings::JNIStr;
use jni::sys::*;
use vipl::utils;
use vipl::vm::namespace::*;
use vipl::vm::value::*;
use vipl::vm::dataType::*;
use vipl::vm::vm::*;
use vipl::vm::stackFrame::*;
use vipl::vm::value::*;
use vipl::variableMetadata::VariableMetadata;

static mut ROUTER: Option<HashMap<(usize, usize), GlobalRef>> = None;
static mut CURRENT_ENV: Option<JNIEnv> = None;

pub fn getRouter() -> &'static mut HashMap<(usize, usize), GlobalRef> {
    unsafe {
        match &mut ROUTER {
            None => {
                ROUTER = Some(HashMap::new());
                ROUTER.as_mut().unwrap()
            }
            Some(v) => v
        }
    }
}

pub fn setEnv(env: JNIEnv) {
    unsafe { CURRENT_ENV = Some(transmute(env)) }
}

pub fn getEnv() -> &'static mut JNIEnv<'static> {
    unsafe {
        match &mut CURRENT_ENV {
            None => panic!(),
            Some(v) => {
                v
            }
        }
    }
}

#[no_mangle]
pub extern fn Java_org_example_VIPLSDK_createVM<'local>(mut env: JNIEnv<'local>, class: JClass<'local>) -> jlong {
    vipl::ffi::createVm() as i64
}

#[no_mangle]
pub extern fn Java_org_example_VIPLSDK_test(mut env: JNIEnv, class: JClass, vm: &mut VirtualMachine) {
    vipl::ffi::test(vm)
}

#[no_mangle]
pub extern fn Java_org_example_VIPLSDK_registerNamespace(mut env: JNIEnv, class: JClass, vm: &mut VirtualMachine, name: JString) -> jlong {
    let s = env.get_string(&name).unwrap();
    let namespace = Namespace::new(s.to_str().unwrap().to_string());

    unsafe { transmute(vm.registerNamespace(namespace)) }
}

#[no_mangle]
pub extern fn route(vm: &mut VirtualMachine, stack: &mut StackFrame) {
    println!("hello my friens :3");
    let r = getRouter().get(&(stack.namespace.id, stack.functionID));
    match r {
        None => {
            eprintln!("failed to lookup {} {}", stack.namespace.id, stack.functionID);
        },
        Some(v) => {
            let env = getEnv();

            env.call_method(
                v,
                "call",
                "(JJ)V",
                &[JValue::Long(vm as *mut VirtualMachine as i64), JValue::Long(stack as *mut StackFrame as i64)],
            ).unwrap();
        }
    }
}

#[no_mangle]
pub extern fn Java_org_example_VIPLSDK_registerFunction(mut env: JNIEnv, class: JClass, vm: &mut VirtualMachine, callable: JObject, name: JString, returnType: JString, args: JObjectArray, namespaceID: jlong) -> jlong {
    let namespace = vm.namespaces.get_mut(namespaceID as usize).unwrap();
    let functionName = env.get_string(&name).unwrap().to_str().unwrap().to_string();
    let argsLen = env.get_array_length(&args).unwrap();

    let mut rawArgs = vec![];
    let rawReturn = env.get_string(&returnType).unwrap().to_str().unwrap().to_string();

    for index in 0..argsLen {
        let obj = env.get_object_array_element(&args, index).unwrap();
        let arg = env.get_string(&obj.into()).unwrap().to_str().unwrap().to_string();
        rawArgs.push(arg);
    }

    let ret = utils::parseDataTypeFromStr(&rawReturn).unwrap();
    let args = rawArgs.into_iter().map(|it| {
        VariableMetadata::from(utils::parseDataTypeFromStr(&it).unwrap())
    }).collect::<Vec<_>>();

    let f = FunctionMeta::makeNative(functionName, args.into_boxed_slice(), argsLen as usize, Some(ret));

    let functionID = namespace.registerFunctionDef(f);

    let e = env.new_global_ref(callable).unwrap();

    getRouter().insert((namespaceID as usize, functionID), e);

    *namespace.functions.get_mut(functionID).unwrap() = Some(LoadedFunction::Native(route));

    vm.link();


    functionID as i64
}

#[no_mangle]
pub extern fn Java_org_example_VIPLSDK_call(mut env: JNIEnv, class: JClass, vm: &mut VirtualMachine, namespaceID: jlong, functionID: jlong, args: JLongArray) {
    let c = unsafe { env.unsafe_clone() };
    setEnv(c);
    let v = vm as *mut VirtualMachine;
    let namespace = vm.namespaces.get(namespaceID as usize).unwrap();


    let argsLen = env.get_array_length(&args).unwrap();
    let mut rawArgs = vec![0i64; argsLen as usize];
    env.get_long_array_region(args, 0, &mut rawArgs).unwrap();

    let mut args = rawArgs.into_iter().map(|it| {
        Value::from(it as usize)
    }).collect::<Vec<_>>();

    let frame = StackFrame{
        localVariables: &mut args,
        objects: None,
        previous: None,
        programCounter: 0,
        namespace: vm.namespaces.get(namespaceID as usize).unwrap(),
        functionID: 0,
    };


    let func = namespace.functions.get(functionID as usize).unwrap().as_ref().unwrap();

    unsafe { func.call(&mut *v, frame); }
}