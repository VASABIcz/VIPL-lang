use std::ffi::{CStr, CString};
use libc::{CS, system};
use crate::ast::Expression;
use crate::lexer::TokenType::In;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};
use crate::vm::dataType::{DataType, Generic};
use crate::vm::namespace::NamespaceState::Loaded;
use crate::vm::namespace::{GlobalMeta, Namespace};
use crate::vm::objects::{Array, Str};
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

pub fn registerOs(vm: &mut VirtualMachine) {
    let mut namespace = Namespace::new("os", vm);

    namespace.makeNative("exec", &[DataType::str()], |a, b| {
        let c = b.getString(0);
        let idk = CString::new(c.clone()).unwrap();

        unsafe { system(idk.as_ptr()); }

        return Value::null();
    }, DataType::Void, false);

    vm.registerNamespace(namespace);
}